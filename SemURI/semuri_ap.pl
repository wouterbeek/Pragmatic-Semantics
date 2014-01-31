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
:- use_module(generics(codes_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(file_mime)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_stat)).
:- use_module(void(void_stat)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(su, 'http://www.wouterbeek.com/semuri.owl#').
:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').



semuri_ap(Site1, Resource):-
  once(rdf_literal(Resource, ckan:url, URL, Site1)),
  once(rdf_literal(Resource, ckan:id, ResourceId, Site1)),
  once(rdf_literal(Resource, ckan:format, ResourceFormat, Site1)),
  (
    once(rdf_literal(Resource, ckan:resource_type, ResourceType, Site1))
  ->
    atomic_list_concat([ResourceId,ResourceFormat,ResourceType,URL], '\n', X1)
  ;
    atomic_list_concat([ResourceId,ResourceFormat,URL], '\n', X1)
  ),
  debug(semuri, 'Starting:\n~w', [X1]),

  once(rdf(Package, ckan:resources, Resource, Site1)),
  once(rdf_literal(Package, ckan:name, PackageName, Site1)),
  once(rdf_literal(Package, ckan:title, PackageTitle, Site1)),
  atomic_list_concat([PackageName,PackageTitle], '\n', X2),

  once(rdf(Package, ckan:organization, Organization, Site1)),
  once(rdf_literal(Organization, ckan:display_name, OrganizationName, Site1)),

  setoff(
    UserName,
    (
      rdf(Organization, ckan:users, User, Site1),
      rdf_literal(User, ckan:fullname, UserName, Site1)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', UserName),

  setoff(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site1),
      rdf_literal(Tag, ckan:name, TagName, Site1)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', TagName),

  % DEB
  flag(datasets, Id, Id + 1),
  format(user_output, '~w\n', [Id]),

  atomic_list_concat([Id,PackageName], '-', Name),
  Spec =.. [Site1,Name],
  create_nested_directory(ckan_data(Spec)),
  db_add_novel(user:file_search_path(Name, Spec)),
  
  atomic_list_concat([Site1,semuri], '_', Site2),
  ap(
    Name,
    [
      ap_stage([], download_to_directory(URL)),
      ap_stage([], extract_archives),
      ap_stage([], mime_dir),
      ap_stage([], rdf_convert_directory),
      ap_stage([args([Resource,Site2])], void_statistics),
      ap_stage([args([Resource,Site2])], compress),
      ap_stage([args([Resource,Site2])], randomize_iris)
    ],
    T
  ),

  assert(semuri:row([X1,X2,OrganizationName,UserName,TagName|T])).


randomize_iris(
  FromDir,
  ToDir,
  ap(status(succeed),properties([of_file(dummy,NVPairs)])),
  Resource,
  Site
):-
  absolute_file_name(
    lits,
    FromFileLiterals,
    [access(read),extensions([txt]),relative_to(FromDir)]
  ),
  absolute_file_name(
    triples,
    FromFileTriples,
    [access(read),extensions([dat]),relative_to(FromDir)]
  ),
  maplist(copy_file(ToDir, _, _), [FromFileLiterals,FromFileTriples], _),
  
  absolute_file_name(
    uris,
    FromFileURIs,
    [access(read),extensions([txt]),relative_to(FromDir)]
  ),
  file_lines(FromFileURIs, Lines),
  file_alternative(FromFileURIs, ToDir, _, _, ToFileURIs),
  setup_call_cleanup(
    open(ToFileURIs, write, Stream, []),
    forall(
      between(1, Lines, _),
      put_random_iri(Stream)
    ),
    close(Stream)
  ),

  absolute_file_name(semuri('RDFmodel'), JAR, [access(read),file_type(jar)]),
  run_jar(JAR, [compress,file(ToDir)]),

  maplist(
    file_to_nvpairs(ToDir, Resource, rnd, Site),
    [stats,compression],
    [NVPairs1,NVPairs2]
  ),
  append(NVPairs1, NVPairs2, NVPairs).


put_random_iri(Stream):-
  put_codes(Stream, `http://`),
  forall(
    between(1, 15, _),
    (
      random_between(97, 122, Code),
      put_code(Stream, Code)
    )
  ),
  put_code(Stream, 10). %LF


compress(
  FromDir,
  ToDir,
  ap(status(succeed),properties([of_file(dummy,NVPairs)])),
  Resource,
  Site
):-
  setup_call_cleanup(
    create_nested_directory(project(tmp), TmpDir),
    (
      absolute_file_name(
        semuri('RDFmodel'),
        JAR,
        [access(read),file_type(jar)]
      ),
      run_jar(JAR, [preprocess,file(FromDir),file(TmpDir)])
    ),
    (
      directory_files(
        [include_directories(false),include_self(false)],
        TmpDir,
        TmpFiles
      ),
      maplist(copy_file(FromDir, _, _), TmpFiles, _)
    )
  ),

  absolute_file_name(semuri('RDFmodel'), JAR, [access(read),file_type(jar)]),
  run_jar(JAR, [compress,file(FromDir)]),

  maplist(
    file_to_nvpairs(FromDir, Resource, _, Site),
    [stats,compression],
    [NVPairs1,NVPairs2]
  ),
  append(NVPairs1, NVPairs2, NVPairs),

  % Copy the Turtle file to the next AP stage.
  directory_files([], FromDir, FromFiles),
  maplist(copy_file(ToDir, _, _), FromFiles, _).


file_to_nvpairs(FromDir, Resource, KeyPrefix, Site, Base, NVPairs):-
  absolute_file_name(
    Base,
    StatisticsFile,
    [extensions([json]),relative_to(FromDir)]
  ),
  setup_call_cleanup(
    open(StatisticsFile, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream)
  ),
  findall(
    nvpair(Key1,Value),
    (
      get_dict(Key1, Dict, Value),
      (
        nonvar(Resource),
        nonvar(Site)
      ->
        (
          var(KeyPrefix)
        ->
          Key2 = Key1
        ;
          atomic_list_concat([KeyPrefix,Key1], '_', Key2)
        ),
        rdf_global_id(su:Key2, P),
        rdf_assert_datatype(Resource, P, xsd:float, Value, Site)
      ;
        true
      )
    ),
    NVPairs
  ).

