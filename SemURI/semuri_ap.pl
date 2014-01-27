:- module(
  semuri_ap,
  [
    semuri_ap/2 % +Site:atom
                % +PackageURL:pair(atom,url)
  ]
).

/** <module> Semantic URIs Automated Processes

Automated processes for semantic URIs.

@author Wouter Beek
@tbd Load canonical XSD
@tbd OWL materialize (Jena JAR)
@tbd Steven (JAR)
@tbd Table output HTML
@version 2014/01
*/

:- use_module(ap(ap)).
:- use_module(generics(archive_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_serial)).



semuri_ap(Site, Resource):-
  once(rdf_literal(Resource, ckan:url, URL, Site)),
  once(rdf_literal(Resource, ckan:id, ResourceId, Site)),
  once(rdf_literal(Resource, ckan:format, ResourceFormat, Site)),
  (
    once(rdf_literal(Resource, ckan:resource_type, ResourceType, Site))
  ->
    atomic_list_concat([ResourceId,ResourceFormat,ResourceType,URL], '\n', X1)
  ;
    atomic_list_concat([ResourceId,ResourceFormat,URL], '\n', X1)
  ),
  debug(semuri, 'Starting:\n~w', [X1]),

  once(rdf(Package, ckan:resources, Resource, Site)),
  once(rdf_literal(Package, ckan:name, PackageName, Site)),
  once(rdf_literal(Package, ckan:title, PackageTitle, Site)),
  atomic_list_concat([PackageName,PackageTitle], '\n', X2),

  once(rdf(Package, ckan:organization, Organization, Site)),
  once(rdf_literal(Organization, ckan:display_name, OrganizationName, Site)),

  setoff(
    UserName,
    (
      rdf(Organization, ckan:users, User, Site),
      rdf_literal(User, ckan:fullname, UserName, Site)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', UserName),

  setoff(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site),
      rdf_literal(Tag, ckan:name, TagName, Site)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', TagName),

  % DEB
  flag(datasets, Id, Id + 1),
  format(user_output, '~w\n', [Id]),

  atomic_list_concat([Id,PackageName,ResourceId], '-', Name),
  Spec =.. [Site,Name],
  create_nested_directory(ckan_data(Spec)),
  db_add_novel(user:file_search_path(Name, Spec)),

  ap(
    Name,
    [
      ap_stage([], download_to_dir(URL)),
      ap_stage([from(input,_,_)], extract_archives),
      ap_stage([], mime_dir),
      ap_stage([], rdf_convert),
      ap_stage([], stupid_rename)
    ],
    T
  ),

  assert(semuri:row([X1,X2,OrganizationName,UserName,TagName|T])).


steven(FromDir, ToDir, ap(status(succeed),steven)):-
  directory_files([file_types([turtle])], FromDir, FromFiles),
  FromFiles = [FromFile|_],
  maplist(steven(ToDir), FromFile).

steven(ToDir, FromFile):-
  file_alternative(FromFile, ToDir, triples, dat, ToFile),
  rename_file(FromFile, ToFile),
  absolute_file_name(
    semuri('SemanticURIs-0.0.1-SNAPSHOT-jar-with-dependencies'),
    JAR_File,
    [access(read),file_type(jar)]
  ),
  run_jar(JAR_File, [file(ToFile)]).


mime_dir(FromDir, ToDir, ap(status(succeed),mimes(MIMEs))):-
  directory_files([], FromDir, FromFiles),
  maplist(file_mime, FromFiles, MIMEs),
  forall(
    member(FromFile, FromFiles),
    copy_file(FromFile, ToDir)
  ).


rdf_convert(FromDir, ToDir, ap(status(succeed),files(ToFiles))):-
  directory_files([], FromDir, FromFiles1),
  findall(
    ToFile,
    (
      member(FromFile, FromFiles1),
      file_mime(FromFile, MIME),
      rdf_mime(MIME),
      setup_call_cleanup(
        rdf_new_graph(TmpG),
        (
          relative_file_name(FromFile, FromDir, RelativeFile),
          rdf_load2(FromFile, [graph(TmpG),mime(MIME)]),
          directory_file_path(ToDir, RelativeFile, ToFile),
          create_file(ToFile),
          rdf_save2(ToFile, [format(turtle),graph(TmpG)])
        ),
        rdf_unload_graph(TmpG)
      )
    ),
    ToFiles
  ).


download_to_dir(URL, ToDir, ap(status(succeed),file(File3))):-
  url_to_file(URL, File1),
  directory_file_path(_, File2, File1),
  file_name_extensions(Base, Extensions, File2),
  create_file(ToDir, Base, Extensions, File3),
  download_to_file(URL, File3),
  size_file(File3, Size),
  % Expressed in megabytes.
  TooBig is 1024 * 1024 * 100,
  (Size > TooBig -> permission_error(open,'BIG-file',File3) ; true).


