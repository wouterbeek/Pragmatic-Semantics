:- module(
  ap_db,
  [
% ADD
    add_done/1, % +AP:iri
    add_operation_on_file/4, % +ApStage:iri
                             % +File:atom
                             % +Operation:atom
                             % +Modifiers:list
    add_properties_of_file/3, % +ApStage:iri
                              % +File:atom
                              % +NVPairs:list(pair)
    add_table/2, % +ApStage:iri
                 % +Table:iri
    add_skip/1, % +ApStage:iri
    add_succeed/1, % +ApStage:iri
% READ
    ap_resource/3, % +AP:iri
                   % ?Resource:iri
                   % ?Graph:atom
    ap_stage_name/2, % ?ApStage:iri
                     % ?Name:atom
    ap_stage_resource/3, % +ApStage:iri
                         % ?Resource:iri
                         % ?Graph:atom
% CREATE
    create_ap/2, % +AP_Collection:iri
                 % -AP:iri
    create_ap_collection/1, % -AP_Collection:iri
    create_initial_stage/2, % +AP:iri
                            % -Intitial_AP_Stage:iri
    create_next_stage/2, % +AP_Stage1:iri
                         % -AP_Stage2:iri
    create_resource/2 % +ClassName:atom
                      % -Resource:iri
  ]
).

/** <module> Automated Processes databases

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_remote_module(generics(error_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf(rdf_container)).
:- use_remote_module(rdf_term(rdf_boolean)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdfs(rdfs_build)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(xml(xml_namespace)).

:- rdf_meta(ap_resource(r,r,?)).
:- rdf_meta(ap_stage_resource(r,r,?)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

:- initialization(assert_schema).

assert_schema:-
  rdfs_assert_subclass(ap:'AP-Collection', rdf:'Bag', ap),
  rdfs_assert_label(ap:'AP-Collection', 'Collection of automated processes',
      ap),

  rdfs_assert_subclass(ap:'AP', rdf:'Seq', ap),
  rdfs_assert_label(ap:'AP', 'Automated process', ap),

  rdfs_assert_class(ap:'AP-Stage', ap),
  rdfs_assert_label(ap:'AP-Stage', 'Automated process stage', ap).


add_nvpair(Name-Value1, BNode):-
  rdf_bnode(BNode),
  rdf_assert_string(BNode, ap:name, Name, ap),
  with_output_to(atom(Value2), write_canonical_catch(Value1)),
  rdf_assert_string(BNode, ap:value, Value2, ap).


%! add_done(+AP:iri) is det.
% States that the given automated process was run in its entirety and
%  all its results (including failures and exceptions)
%  were stored succesfully.

add_done(AP):-
  rdf_assert_true(AP, ap:done, ap).


add_operation_on_file(ApStage, File, Operation, Modifiers):-
  rdf_assert_individual(ApStage, ap:'FileOperation', ap),
  rdf_assert_string(ApStage, ap:file, File, ap),
  rdf_assert_string(ApStage, ap:operation, Operation, ap),
  forall(
    member(Modifier, Modifiers),
    rdf_assert_string(ApStage, ap:has_modifier, Modifier, ap)
  ).


add_properties_of_file(ApStage, File, NVPairs):-
  rdf_assert_individual(ApStage, ap:'FileProperties', ap),
  rdf_assert_string(ApStage, ap:file, File, ap),
  maplist(add_nvpair, NVPairs, BNodes),
  forall(
    member(BNode, BNodes),
    rdf_assert(ApStage, ap:has_property, BNode, ap)
  ).


add_table(ApStage, Table):-
  rdf_assert_individual(ApStage, ap:'Tables', ap),
  rdf_assert(ApStage, ap:table, Table, ap).


%! add_skip(+ApStage:iri) is det.

add_skip(ApStage):-
  rdf_assert_individual(ApStage, ap:'Skip', ap),
  rdf_assert_string(ApStage, ap:status, skip, ap).


%! add_succeed(+ApStage:iri) is det.
% States that the given automated process stage was completed succesfully,
%  i.e. without failing or throwing an exception.

% Skipped AP stages are not asserted as succeeding.
add_succeed(ApStage):-
  rdfs_individual_of(ApStage, ap:'Skip'), !.
add_succeed(ApStage):-
  rdf_assert_string(ApStage, ap:status, succeed, ap).


ap_resource(AP, Resource, Graph):-
  rdf(AP, ap:resource, Resource, ap),
  rdf_string(AP, ap:graph, Graph, ap).


ap_stage_resource(ApStage, Resource, Graph):-
  nonvar(ApStage), !,
  rdf_collection_member(ApStage, AP, ap),
  ap_resource(AP, Resource, Graph).
ap_stage_resource(ApStage, Resource, Graph):-
  ap_resource(AP, Resource, Graph),
  rdf_collection_member(ApStage, AP, ap).


ap_stage_name(ApStage, Name):-
  rdf_string(ApStage, ap:name, Name, ap).


create_ap(AP_Collection, AP):-
  create_resource('AP', AP),
  rdf_assert_collection_member(AP_Collection, AP, ap).


create_ap_collection(AP_Collection):-
  create_resource('AP-Collection', AP_Collection).


create_initial_stage(AP, ApStage):-
  create_stage(AP, -1, ApStage).


create_stage(AP, StageNum, ApStage):-
  create_resource('AP-Stage', ApStage),
  rdf_assert_collection_member(AP, ApStage, ap),
  format(atom(Label), 'Automated stage ~d', [StageNum]),
  rdfs_assert_label(ApStage, Label, ap),
  rdf_assert_datatype(ApStage, ap:stage, StageNum, xsd:integer, ap).


create_next_stage(AP_Stage1, AP_Stage2):-
  rdf_datatype(AP_Stage1, ap:stage, StageNum1, xsd:integer, ap),
  once(rdf_collection_member(AP_Stage1, AP, ap)),
  StageNum2 is StageNum1 + 1,
  create_stage(AP, StageNum2, AP_Stage2).


create_resource(BaseName, Resource):-
  rdf_create_next_resource(ap, BaseName, Resource, ap).

