:- module(
  self_base,
  [
    generate/2 % +Arg1:list(integer,integer,integer)
               % +Arg2:list(integer,integer,integer)
  ]
).

/** <module> SELF BASE

Generating the base CCM for Self1993.
CCM simulator for cognitive diagnoses in Self1993.

@author Wouter Beek
@version Sep 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(generic(meta_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(self(self_api)).
:- use_module(self(self_sim)).

:-
  absolute_file_name(self(components), ComponentsDirectory),
  rdf_assert_directory(ComponentsDirectory, ccm),
  absolute_file_name(self(expressions), ExpressionsDirectory),
  rdf_assert_directory(ExpressionsDirectory, ccm).



generate([A, B, C], [D, E, F]):-
  once(integer_expression_definition(Integer)),
  maplist(
    atom_number,
    [A_A, A_B, A_C, A_D, A_E, A_F],
    [A, B, C, D, E, F]
  ),
  maplist42(
    find_or_add_expression,
    [a, b, c, d, e, f],
    Integer,
    [A_A, A_B, A_C, A_D, A_E, A_F],
    [E_A, E_B, E_C, E_D, E_E, E_F]
  ),
  maplist(
    find_or_add_point,
    [E_A, E_B, E_C, E_D, E_E, E_F],
    [P_A, P_B, P_C, P_D, P_E, P_F]
  ),
  once(righter_component_definition(Righter)),
  simulate_behavior(Righter, [P_C, P_F], [x, i], [P_X, P_I]),
  once(middler_component_definition(Middler)),
  simulate_behavior(Middler, [P_X, P_B, P_E], [y, h], [P_Y, P_H]),
  once(lefter_component_definition(Lefter)),
  simulate_behavior(Lefter, [P_Y, P_A, P_D], [g], [P_G]),
  
  % DEBUG
  point_to_label(P_G, G), write(G),
  point_to_label(P_H, H), write(H),
  point_to_label(P_I, I), write(I),
  nl.

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

:- generate([1, 2, 3], [1, 2, 3]).
