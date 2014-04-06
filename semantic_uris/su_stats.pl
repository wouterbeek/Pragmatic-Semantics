:- module(
  su_stats,
  [
    su_stats/3 % +ApStage:iri
               % +PredicateName:atom
               % +File:atom
  ]
).

/** <module> SemanticURIs statistics

@author Wouter Beek
@version 2014/02
*/

:- use_remote_module(ap(ap_db)).
:- use_remote_module(dcg(dcg_generic)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_conv(json_to_rdf)).
:- use_remote_module(su(su_db)).

legend(
  stats,
  _,
  [
    blank_nodes-integer-false,
    boundary-list(string)-false,
    %'codelength/boundary'-float-false,
    'codelength/root'-float-false,
    fingerprints-integer-false,
    literals-integer-false,
    partition-rdf_list(integer)-false,
    predicates-integer-false,
    propsets-integer-false,
    subjects-integer-false,
    triples-integer-false,
    type_targets-integer-false,
    typesets-integer-false,
    uris-integer-false
  ]
).



su_stats(ApStage, PredicateName, File):-
  ap_stage_resource(ApStage, Resource, Graph),

  % Assert the statistics on the resource.
  setup_call_cleanup(
    open(File, read, Stream),
    (
      json_read(Stream, JSON_Term),
      json_to_rdf(Graph, su_stats, su, JSON_Term, Stats),
      rdf_global_id(su:PredicateName, Predicate1),
      rdf_assert(Resource, Predicate1, Stats, Graph)
    ),
    close(Stream)
  ),

  % Assert the statistics on the AP stage.
  findall(
    Name-Value,
    (
      rdf(Stats, Predicate2, Object),
      dcg_with_output_to(atom(Name), rdf_term_name(Predicate2)),
      dcg_with_output_to(atom(Value), rdf_term_name(Object))
    ),
    NVPairs
  ),
  add_properties_of_file(ApStage, file, NVPairs).

