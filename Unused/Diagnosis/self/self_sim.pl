:- module(
  self_sim,
  [
    simulate_behavior/4 % +ComponentDefinition:component_definition
                        % +Inputs:list(point)
                        % +OutputNames:list(atom)
                        % -Outputs:list(point)
  ]
).

/** <module> Self Simulation

Simulation for Self1993.

@author Wouter Beek
@version Sep 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(self(self_api)).
:- use_module(self(self_beh)). % This is needed for meta-calls.



%% simulate_behavior(
%%   +ComponentDefinition:component_definition,
%%   +Inputs:list(point),
%%   +OutputNames:list(atom),
%%   -Outputs:list(point)
%% ) is det.

simulate_behavior(ComponentDefinition, Inputs, OutputNames, Outputs):-
  component_definition_to_behavior_rule(ComponentDefinition, BehaviorRule),
  maplist(point, InputExpressions, Inputs),
  maplist(expression_to_argument, InputExpressions, InputIntegers_),
  maplist(atom_number, InputIntegers_, InputIntegers),
  once(current_functor(BehaviorRule, Arity)),
  length(Integers, Arity),
  append(InputIntegers, OutputIntegers_, Integers),
  Call =.. [BehaviorRule | Integers],
  call(Call),
  maplist(atom_number, OutputIntegers, OutputIntegers_),
  once(integer_expression_definition(Integer)),
  maplist42(
    find_or_add_expression,
    OutputNames,
    Integer,
    OutputIntegers,
    OutputExpressions
  ),
  maplist(find_or_add_point, OutputExpressions, Outputs),
  maplist(add_input_relation, Inputs, Inputs_),
  maplist(add_input_relation, Outputs, Outputs_),
  find_or_add_component(
    ComponentDefinition,
    Inputs_,
    [],
    Outputs_,
    _Component,
    _ComponentLocation
  ).

add_input_relation(Point, RelationPoint):-
  add_relation(Point, has_input, RelationPoint).

add_output_relation(Point, RelationPoint):-
  add_relation(Point, has_output, RelationPoint).

add_relation(Point, ComponentRelation, ComponentRelation/Point).

%% maplist42(
%%   +Predicate:atom,
%%   +List1:list,
%%   +Element:atom,
%%   -List3:list,
%%   -List4:list
%% ) is det.

maplist42(Predicate, List1, Element, List3, List4):-
  length(List1, Length),
  repeating_list(Element, Length, List2),
  maplist(Predicate, List1, List2, List3, List4).
