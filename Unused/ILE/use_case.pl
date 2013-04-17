:- module(
  use_case,
  [
    add_use_case/1, % -UseCase:use_case
    use_case/1 % ?UseCase:use_case
  ]
).

/** <module> Use cases

Use case objects.

@author Wouter Beek
@version 2012/08, 2012/10.
*/

:- use_module(ile(ile)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).



init_use_case:-
  rdfs_assert_individual( ile:use_case,   rdf:'Class',    ile),
  rdfs_assert_individual( ile:'ILE_Role', rdfs:'Class',   ile),
  rdfs_assert_subclass(   ile:'ILE_Role', rdf:'Property', ile),
  rdfs_assert_individual( ile:role,       rdf:'Property', ile),
  rdfs_assert_individual( ile:learner,    ile:'ILE_Role', ile),
  rdfs_assert_subproperty(ile:learner,    ile:role,       ile),
  rdfs_assert_individual( ile:teacher,    ile:'ILE_Role', ile),
  rdfs_assert_subproperty(ile:teacher,    ile:role,       ile),
  rdfs_assert_individual(ile:use_case,    rdf:'Property', ile),
  flag(use_case_id, _OldID, 0).

add_use_case(UseCase):-
  init_use_case,
  flag(use_case_id, ID, ID + 1),
  format(atom(Name), 'use_case_~w', [ID]),
  rdf_global_id(ile:Name, UseCase),
  rdfs_assert_individual(UseCase, ile:use_case, ile),
  format(atom(Label), 'Use case ~w.', [ID]),
  rdfs_assert_label(UseCase, Label, ile),
  ile(ILE),
  rdf_assert(ILE, ile:use_case, UseCase, ile).

use_case(UseCase):-
  rdfs_individual_of(UseCase, ile:use_case).

