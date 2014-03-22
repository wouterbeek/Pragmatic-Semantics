:- module(sw_hoax, []).

/** <module> Semantic Web Hoax

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(db_ext)).
:- use_module(generics(uri_ext)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).
:- use_module(library(thread)).
:- use_module(os(archive_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_ntriples_write)).
:- use_module(rdf(rdf_serial), [rdf_directory_files/2,rdf_serialization/5]).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ckan, 'http://www.wouterbeek.com/ckan#').

:- debug(sw_hoax).



sw_hoax:-
  thread_create(sw_hoax(datahub_io), _, []).

sw_hoax(Site):-
  load_metadata(Site),
  create_output_dir(OutDir),
  lod_resources(Site, Resources),
  findall(
    process_resource(OutDir, Resource),
    member(Resource, Resources),
    Goals
  ),
  concurrent(25, Goals, []).
  %concurrent_maplist(process_resource(OutDir), Resources).


create_output_dir(OutDir):-
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_file_path(DataDir, 'Output', OutDir),
  make_directory_path(OutDir).


debug_exception(_, Exception):-
  var(Exception), !.
debug_exception(Url, Exception):-
  debug(sw_hoax, '[*****] Resource ~w; Exception: ~w', [Url,Exception]).


delete_file2(File):-
  nonvar(File),
  access_file(File, write),
  delete_file(File).
delete_file2(_).


download_file(Url, TmpFile):-
  catch(
    download_to_file([], Url, TmpFile),
    E,
    delete_file2(TmpFile)
  ),
  var(E).


drop_rdf_file(Graph, Dir):-
  absolute_file_name(
    input,
    File,
    [access(write),extensions([nt]),relative_to(Dir)]
  ),
  monkey(File, [graph(Graph)]).


drop_url_file(Url, Dir):-
  absolute_file_name(basename, File, [access(write),relative_to(Dir)]),
  setup_call_cleanup(
    open(File, write, Out),
    with_output_to(Out, writeln(Url)),
    close(Out)
  ).


file_correct_extension(File1, File2):-
  file_mime(File1, MIME),
  rdf_serialization(Ext, _, _, MIMEs, _),
  memberchk(MIME, MIMEs),
  file_alternative(File1, _, _, Ext, File2),
  File1 \== File2, !,
  link_file(File1, File2, symbolic).
file_correct_extension(File, File).


is_lod_resource(Site, Resource):-
  rdf_string(Resource, ckan:format, Format, Site),
  rdf_format(Format), !.
is_lod_resource(Site, Resource):-
  rdf_string(Resource, ckan:mimetype, Mimetype, Site),
  rdf_mimetype(Mimetype).


load_metadata(Site):-
  metadata_source(Site, Source),
  rdf_load(Source, [format(turtle),graph(Site)]).


lod_resources(Site, Resources):-
  findall(
    Resource,
    (
      rdfs_individual_of(Resource, ckan:'Resource'),
      is_lod_resource(Site, Resource)
    ),
    Resources
  ).


metadata_source(Site, File):-
  absolute_file_name(
    data(Site),
    File,
    [access(read),extensions([ttl]),file_errors(fail)]
  ), !.
metadata_source(Site, Location):-
  metadata_location(Site, Location).


metadata_location(datahub_io, 'https://dl.dropboxusercontent.com/s/brxpfdwn4n72c2z/datahub_io.ttl?dl=1&token_hash=AAEd9UWXY3SsIBAILVE-yIH7fuRq-_s8RYFgdEAePb0oSQ').


process_resource(OutDir, Resource):-
  once(rdf_string(Resource, ckan:url, Url, _)),
  catch(
    (
      download_file(Url, TmpFile),
      unpack_file(TmpFile),
      rdf_load_file(TmpFile),
      rdf_save_file(OutDir, Url)
    ),
    Exception,
    true
  ),
  debug_exception(Url, Exception), !.
% Who cares?
process_resource(_, _).


rdf_load_file(File):-
  file_directory_name(File, Dir),
  setup_call_cleanup(
    rdf_directory_files(Dir, Files1),
    (
      maplist(file_correct_extension, Files1, Files2),
      rdf_load(Files2, [graph(dummy)])
    ),
    maplist(delete_file, Files2)
  ).


rdf_format('RDF').
rdf_format('XML').
rdf_format('application/n-triples').
rdf_format('application/rdf+xml').
rdf_format('application/x-nquads').
rdf_format('application/x-ntriples').
rdf_format('example/n3').
rdf_format('example/ntriples').
rdf_format('example/rdf xml').
rdf_format('example/rdf+json').
rdf_format('example/rdf+json').
rdf_format('example/rdf+ttl').
rdf_format('example/rdf+xml').
rdf_format('example/rdfa').
rdf_format('example/turtle').
rdf_format('example/x-turtle').
rdf_format('html+rdfa').
rdf_format('linked data').
rdf_format('mapping/owl').
rdf_format('meta/owl').
rdf_format('meta/rdf-schema').
rdf_format('meta/void').
rdf_format(owl).
rdf_format('rdf-n3').
rdf_format('rdf-turtle').
rdf_format('rdf-xml').
rdf_format('rdf/n3').
rdf_format('rdf/turtle').
rdf_format('rdf/xml, html, json').
rdf_format('text/n3').
rdf_format('text/turtle').


rdf_mimetype('application/rdf+n3').
rdf_mimetype('application/rdf+xml').
rdf_mimetype('application/turtle').
rdf_mimetype('text/n3').
rdf_mimetype('text/json').
rdf_mimetype('text/rdf+n3').
rdf_mimetype('text/turtle').
%rdf_mimetype('text/xml').


rdf_save_file(OutDir, Url):-
  setup_call_cleanup(
    url_dir(OutDir, Url, Dir),
    (
      drop_rdf_file(dummy, Dir),
      drop_url_file(Url, Dir)
    ),
    rdf_unload_graph(dummy)
  ).


unpack_file(File):-
  catch(
    extract_archive(File),
    E,
    delete_file2(File)
  ),
  var(E).


url_dir(OutDir, Url, Dir):-
  rdf_atom_md5(Url, 1, Hash),
  directory_file_path(OutDir, Hash, Dir),
  make_directory_path(Dir).

