:- module(
  ckan_lod,
  [
    ckan_download_lod/1 % +Site:atom
  ]
).

/** <module> CKAN Download

CKAN catalogues can be download and loaded using RDF.

All LOD described in a CKAN calalogue can be downloaded as well.

@author Wouter Beek
@tbd Reintroduce AP.
@version 2014/03-2014/04
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(thread)).
:- use_module(library(uri)).

:- use_remote_module(generics(deb_ext)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(http(http_download)).
:- use_remote_module(os(archive_ext)).
:- use_remote_module(os(dir_infra)).
:- use_remote_module(rdf(rdf_graph_name)).
:- use_remote_module(rdf_file(rdf_file)).
:- use_remote_module(rdf_file(rdf_ntriples_write)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(void(void_file)).

:- use_remote_module(prasem, ckan(ckan_catalog)).



ckan_download_lod(Site):-
  ckan_load_catalog(Site, G1),

  % Collect all CKAN resources that store LOD.
  % Note that sorting by size makes no sense,
  % since the semantics of the values of `ckan:size` is unknown.
  ckan_lod_resources(G1, Resources),

  length(Resources, NumberOfResources), %DEB
  debug(ckan, 'About to process ~:d resources.', [NumberOfResources]), %DEB

  output_directory(_),

  % Process the resources by authority.
  % This avoids being blocked by servers that do not allow
  % multiple simultaneous requests.
  findall(
    Authority-Resource,
    (
      rdf_string(Resource, ckan:url, Url, G1),
      uri_components(Url, Components),
      uri_data(authority, Components, Authority)
    ),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),
  maplist(ckan_download_authority(G1), Pairs2).
  %%%%concurrent_maplist(ckan_download_authority(G1), Pairs2).


ckan_download_authority(G1, _-Resources):-
  maplist(ckan_download_resource(G1), Resources).


ckan_download_resource(G1, Resource):-
  format(atom(Msg), 'Exception while downloading resource ~w.', [Resource]),
  rdf_string(Resource, ckan:url, Url, G1),
  catch_debug(
    ckan,
    Msg,
    ckan_process_resource(G1, Resource, Url)
  ), !.
ckan_download_resource(_, _, Resource):-
  debug(ckan, 'Could not download resource ~w.', [Resource]).


ckan_process_resource(G1, Resource, Url):-
  ignore(catch(
    download_to_file([force(true),never_give_up(true)], Url, TmpFile),
    Exception,
    debug(ckan, '[~w] No LOD from ~w due to HTTP error.', [Exception,Url])
  )),
  (
    var(Exception)
  ->
    file_directory_name(TmpFile, TmpDir),
    extract_directory([], TmpDir),
    rdf_directory_files(TmpDir, Files1),
    maplist(rdf_file_correct_extension, Files1, Files2),
    (
      Files2 == []
    ->
      debug(ckan, 'No LOD from URL ~a.', [Url])
    ;
      ckan_process_lod(G1, Resource, Url, Files2)
    ),
    delete_file(TmpFile)
  ;
    true
  ), !.
ckan_process_resource(G1, Resource, Url):-
  gtrace, %DEB
  ckan_process_resource(G1, Resource, Url).


ckan_process_lod(G1, Resource, Url, Files):-
  setup_call_cleanup(
    (
      rdf_new_graph(G2),
      rdf_load(Files, [graph(G2)])
    ),
    save_lod(Resource, G1, Url, G2),
    (
      rdf_unload_graph(G2),
      maplist(delete_file, Files)
    )
  ).


save_lod(Resource, G1, Url, G2):-
  % All resources are saved under the output directory.
  output_directory(OutputDir),
  % Each resource is saved in a separate subdirectory of the output
  % directory.
  url_flat_directory(OutputDir, Url, UrlDir),

  % Save the RDF file.
  absolute_file_name(
    input,
    RdfFile,
    [access(write),extensions([nt]),relative_to(UrlDir)]
  ),
  rdf_assert_string(Resource, ckan:local_file, RdfFile, G1),
  rdf_ntriples_write(RdfFile, [graph(G2)]),

  % Save the VoID file.
  absolute_file_name(
    'VoID',
    VoidFile,
    [access(write),file_type(ntriples),relative_to(UrlDir)]
  ),
  setup_call_cleanup(
    rdf_new_graph(G3),
    void_save([format(turtle)], G3, VoidFile),
    rdf_unload_graph(G3)
  ),

  % Save the resource's URL to a file.
  absolute_file_name(basename, UrlFile, [access(write),relative_to(UrlDir)]),
  setup_call_cleanup(
    open(UrlFile, write, Out),
    with_output_to(Out, writeln(Url)),
    close(Out)
  ).

