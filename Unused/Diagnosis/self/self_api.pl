:- module(
  self_api,
  [
    integer_expression_definition/1, % ?Integer:expression_definition
    lefter_component_definition/1, % ?Lefter:component_definition
    middler_component_definition/1, % ?Middler:component_definition
    righter_component_definition/1 % ?Righter:component_definition
  ]
).

/** <module> Self API

API for Self1993.

@author Wouter Beek
@version 2012/09
*/

:- use_module(library(semweb/rdfs)).



integer_expression_definition(Integer):-
  rdfs_subclass_of(Integer, expression:integer).

lefter_component_definition(Lefter):-
  rdfs_subclass_of(Lefter, component:lefter).

middler_component_definition(Middler):-
  rdfs_subclass_of(Middler, component:middler).

righter_component_definition(Righter):-
  rdfs_subclass_of(Righter, component:righter).
