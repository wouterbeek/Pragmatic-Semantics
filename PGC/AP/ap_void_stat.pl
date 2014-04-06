:- module(
  ap_void_stat,
  [
    void_statistics/3 % +FromDirectory:atom
                      % +ToDirectory:atom
                      % +AP_Status:compound
  ]
).

/** <module> AP VoID statistics

VoID statistics process for the AP architecture.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(http_parameters(rfc2616_media_type)). % DCG-meta.
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(void(void_stat)). % If only for the namespace.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc2616, 'http://tools.ietf.org/html/rfc2616#').



void_statistics(FromDir, ToDir, ApStage):-
  directory_files([], FromDir, FromFiles),
  findall(
    File,
    (
      member(File, FromFiles),
      rdf_setup_call_cleanup(
        [],
        File,
        void_statistics_on_graph(ApStage, NVPairs)
      ),
      add_properties_of_file(ApStage, File, NVPairs)
    ),
    Files
  ),
  (Files == [] -> existence_error(lod, 'No LOD here') ; true),
  link_directory_contents(FromDir, ToDir).


void_statistics_on_graph(ApStage, NVPairs, ReadGraph):-
  ap_stage_resource(ApStage, Resource, WriteGraph),
  thread_create(count_classes(ReadGraph, Resource, WriteGraph), Id1, []),
  thread_create(count_objects(ReadGraph, Resource, WriteGraph), Id2, []),
  thread_create(count_subjects(ReadGraph, Resource, WriteGraph), Id3, []),
  thread_create(count_properties(ReadGraph, Resource, WriteGraph), Id4, []),
  thread_create(count_triples(ReadGraph, Resource, WriteGraph), Id5, []),
  forall(
    member(Id, [Id1,Id2,Id3,Id4,Id5]),
    thread_join(Id, true)
  ),

  NVPairs = [
    classes-NC,
    subjects-NS,
    properties-NP,
    objects-NO,
    triples-NT
  ],
  rdf_datatype(Resource, void:classes,          NC, xsd:integer, WriteGraph),
  rdf_datatype(Resource, void:distinctObjects,  NS, xsd:integer, WriteGraph),
  rdf_datatype(Resource, void:distinctSubjects, NP, xsd:integer, WriteGraph),
  rdf_datatype(Resource, void:properties,       NO, xsd:integer, WriteGraph),
  rdf_datatype(Resource, void:triples,          NT, xsd:integer, WriteGraph).


count_classes(ReadGraph, Resource, WriteGraph):-
  count_classes(ReadGraph, N),
  rdf_assert_datatype(Resource, void:classes, N, xsd:integer, WriteGraph).

count_objects(ReadGraph, Resource, WriteGraph):-
  count_objects(_, _, ReadGraph, N),
  rdf_assert_datatype(Resource, void:distinctObjects, N, xsd:integer, WriteGraph).

count_subjects(ReadGraph, Resource, WriteGraph):-
  count_subjects(_, _, ReadGraph, N),
  rdf_assert_datatype(Resource, void:distinctSubjects, N, xsd:integer, WriteGraph).

count_properties(ReadGraph, Resource, WriteGraph):-
  count_properties(_, _, ReadGraph, N),
  rdf_assert_datatype(Resource, void:properties, N, xsd:integer, WriteGraph).

count_triples(ReadGraph, Resource, WriteGraph):-
  rdf_statistics(triples_by_graph(ReadGraph, N)),
  rdf_assert_datatype(Resource, void:triples, N, xsd:integer, WriteGraph).

