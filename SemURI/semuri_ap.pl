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
:- use_module(generics(uri_ext)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_serial_conv)). % Used in AP.



semuri_ap(Site, Resource):-
  once(rdf_literal(Resource, ckan:id, ResourceId, Site)),
  once(rdf_literal(Resource, ckan:format, ResourceFormat, Site)),
  (
    once(rdf_literal(Resource, ckan:resource_type, ResourceType, Site))
  ->
    atomic_list_concat([ResourceId,ResourceFormat,ResourceType], '\n', X1)
  ;
    atomic_list_concat([ResourceId,ResourceFormat], '\n', X1)
  ),

  once(rdf(Package, ckan:resources, Resource, Site)),
  once(rdf_literal(Package, ckan:name, PackageName, Site)),
  once(rdf_literal(Package, ckan:title, PackageTitle, Site)),
  atomic_list_concat([PackageName,PackageTitle], '\n', X2),

  once(rdf(Package, ckan:organization, Organization, Site)),
  once(rdf_literal(Organization, ckan:display_name, OrganizationName, Site)),

  findall(
    UserName,
    (
      rdf(Package, ckan:users, User, Site),
      rdf_literal(User, ckan:fullname, UserName, Site)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', UserName),

  findall(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site),
      rdf_literal(Tag, ckan:name, TagName, Site)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', TagName),

  once(rdf_literal(Resource, ckan:url, URL, Site)),

  atomic_list_concat([PackageName,ResourceId], '-', Name),
  Spec =.. [Site,Name],
  create_nested_directory(ckan_data(Spec)),
  db_add_novel(user:file_search_path(Name, Spec)),

  ap(
    Name,
    [
      ap_stage([], download_to_dir(URL)),
      ap_stage([from(input,_,_)], extract_archives)%,
      %ap_stage([args([turtle])], rdf_convert_directory)
      %ap_stage([], owl_materialize),
      %ap_stage([args([turtle])], rdf_convert_directory),
      %ap_stage([between(1,5),to(output)], rdft_experiment)
    ],
    T
  ),

  assert(semuri:row([X1,X2,OrganizationName,UserName,TagName|T])).

download_to_dir(URL, ToDir, Msg):-
  url_to_file(URL, File1),
  directory_file_path(_, File2, File1),
  file_name_extensions(Base, Extensions, File2),
  create_file(ToDir, Base, Extensions, File3),
  download_to_file(URL, File3), !,
  format(atom(Msg), 'Downloaded ~w', [File3]).
download_to_dir(URL, _, Msg):-
  format(atom(Msg), 'Unable to download ~w', [URL]),
gtrace,
  throw(error(Msg)).

extract_archives(FromDir, ToDir, Msg):-
  directory_files([recursive(false)], FromDir, FromFiles),
  findall(
    Msg,
    (
      member(FromFile, FromFiles),
      extract_archive(FromFile, ToDir, Msg)
    ),
    Msgs
  ),
  atomic_list_concat(Msgs, '\n', Msg).

