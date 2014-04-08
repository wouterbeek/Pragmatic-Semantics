:- module(
  qsim_build,
  [
% DYNAMIC
    add_dynamic/3, % +Graph:atom
                   % +State:state
                   % :Goal
    add_derivative/4, % +Quantity:quantity
                      % +Derivative:derivative
                      % +Graph:atom
                      % -Statement:statement
    add_magnitude/4, % +Quantity:quantity
                     % +Magnitude:magnitude
                     % +Graph:atom
                     % -Statement:statement
    add_magnitude_exclusion/4, % +Quantity:quantity
                               % +Magnitude:magnitude
                               % +Graph:atom
                               % -Statement:statement

% STATIC
    add_correspondence/4, % +Magnitude1:magnitude
                          % +Magnitude2:magnitude
                          % +Graph:atom
                          % -Statement:statement
    add_derivative_relation/4, % +Quantity1:quantity
                               % +Quantity2:quantity
                               % +Graph:atom
                               % -Statement:statement
    add_entity/5, % +Graph:atom
                  % +EntityDefinition:iri
                  % +EntityName:atom
                  % +Quantities:list(iri)
                  % -EntityDefinition:iri
    add_entity_definition/3, % +Graph:atom
                             % +EntityDefinitionName:atom
                             % -EntityDefinition:atom
    add_monotonic_decreasing/4, % +Quantity1:quantity
                                % +Quantity2:quantity
                                % +Graph:atom
                                % -Statement:statement
    add_monotonic_increasing/4, % +Quantity1:quantity
                                % +Quantity2:quantity
                                % +Graph:atom
                                % -Statement:statement
    add_quantity/4, % +Graph:atom
                    % +QD:quantity_definition
                    % +Q_Name:atom
                    % -Q:quantity
    add_quantity_definition/4, % +Graph:atom
                               % +QD_Name:atom
                               % +QV_Names:list(atom)
                               % -QD: quantity_definition
    add_state/2, % +Graph:atom
                 % -State:iri
    add_state/3, % +Graph:atom
                 % +Statements:list(iri)
                 % -State:iri
    add_static/2, % +Graph:atom
                  % :Goal
    add_sum/5 % +Quantity1:quantity
              % +Quantity2:quantity
              % +Quantity3:quantity
              % +Graph:atom
              % -Statement:statement
  ]
).

/** <module> QSIM builder

Building QSIM models.

## Statements

We distinguish between the following two kinds of statements:
  * *dynamic*, true in a specific world.
  * *static*, true in all worlds.

@author Wouter Beek
@version 2013/02-2013/03, 2013/07, 2013/09
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(owl(owl_build)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf(rdf_export)).
:- use_remote_module(rdf(rdf_list)).
:- use_remote_module(rdf(rdf_reification)).
:- use_remote_module(rdf_term(rdf_term)).
:- use_remote_module(rdfs(rdfs_build)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(rdfs(rdfs_read)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').

% DYNAMIC
:- meta_predicate(add_dynamic(+,+,2)).
% STATIC
:- meta_predicate(add_static(+,2)).

% DYNAMIC
:- rdf_meta(add_derivative(r,r,+,r)).
:- rdf_meta(add_dynamic(+,r,+)).
:- rdf_meta(add_magnitude(r,r,+,r)).
:- rdf_meta(add_magnitude_exclusion(r,r,+,r)).
% STATIC
:- rdf_meta(add_correspondence(r,r,+,r)).
:- rdf_meta(add_derivative_relation(r,r,+,r)).
:- rdf_meta(add_entity(+,r,+,t,r)).
:- rdf_meta(add_entity_definition(+,+,r)).
:- rdf_meta(add_monotonic_decreasing(r,r,+,r)).
:- rdf_meta(add_monotonic_increasing(r,r,+,r)).
:- rdf_meta(add_quantity(+,r,+,r)).
:- rdf_meta(add_quantity_definition(+,+,+,r)).
:- rdf_meta(add_state(+,t,-)).
:- rdf_meta(add_sum(r,r,r,+,r)).



% DYNAMIC %

add_dynamic(G, SS, Goal):-
  call(Goal, G, Stmt),
  rdf_assert(SS, qsim:statement, Stmt, G),

  % DEB
  once(rdfs_label(SS, SSName)),
  rdf_statement(S, P, O, G, Stmt),
  once(rdfs_label(S, SName)),
  once(rdfs_label(P, PName)),
  once(rdfs_label(O, OName)),
  debug(
    qsim_build,
    'Added statement ~w @ ~w:   <~w,~w,~w>.',
    [Stmt,SSName,SName,PName,OName]
  ).

add_derivative(Q, DQV, G, Stmt):-
  rdf_assert_statement(Q, qsim:derivative, DQV, G, Stmt).

add_magnitude(Q, MQV, G, Stmt):-
  rdf_assert_statement(Q, qsim:magnitude, MQV, G, Stmt).

add_magnitude_exclusion(Q, MQV, G, Stmt):-
  rdf_assert_statement(Q, qsim:magnitude_exclusion, MQV, G, Stmt).



% STATIC %

add_correspondence(MQV1, MQV2, G, Stmt):-
  rdf_assert_statement(MQV1, qsim:correspondence, MQV2, G, Stmt).

add_derivative_relation(Q1, Q2, G, Stmt):-
  rdf_assert_statement(Q1, qsim:d_rel, Q2, G, Stmt).

add_entity(G, ED, E_Name, Qs, E):-
  % Construe the quantity within the quantity definition namespace.
  once(rdfs_label(ED, ED_Name)),
  format(atom(E_Name0), 'e_~w_~w', [ED_Name,E_Name]),
  rdf_global_id(qsim:E_Name0, E),
  rdf_assert_individual(E, ED, G),
  rdfs_assert_label(E, E_Name, G),

  % Connect the quantities with the entity.
  forall(
    member(Q, Qs),
    rdf_assert(E, qsim:quantity, Q, G)
  ).

add_entity_definition(G, ED_Name, ED):-
  format(atom(ED_Name0), 'ed_~w', [ED_Name]),
  rdf_global_id(qsim:ED_Name0, ED),
  rdfs_assert_subclass(ED, qsim:'Entity', G),
  rdfs_assert_label(ED, ED_Name, G).

%! add_landmark(
%!   +Graph:atom,
%!   +QD_Name:atom,
%!   +QV_Name:atom,
%!   -Landmark:uri
%! ) is det.
% Creates a qualitative landmark for the quantity definition
% with the given name.

add_landmark(G, QD_Name, QV_Name, Landmark):-
  format(atom(LandmarkName), 'l_~w_~w', [QD_Name, QV_Name]),
  rdf_global_id(qsim:LandmarkName, Landmark),
  rdfs_assert_label(Landmark, QV_Name, G).

add_monotonic_decreasing(Q1, Q2, G, Stmt):-
  rdf_assert_statement(Q1, qsim:m_neg, Q2, G, Stmt).

add_monotonic_increasing(Q1, Q2, G, Stmt):-
  rdf_assert_statement(Q1, qsim:m_pos, Q2, G, Stmt).

%! add_quantity(
%!   +Graph:atom,
%!   +QuantityDefinition:quantity_definition,
%!   +QuantityName:atom,
%!   -Quantity:quantity
%! ) is det.
% Creates a quantity for the given quantity definition, with the given name.

add_quantity(G, QD, Q_Name, Q):-
  % Construe the quantity within the quantity definition namespace.
  once(rdfs_label(QD, QD_Name)),
  format(atom(Q_Name0), 'q_~w_~w', [QD_Name, Q_Name]),
  rdf_global_id(qsim:Q_Name0, Q),
  rdf_assert_individual(Q, QD, G),
  rdfs_assert_label(Q, Q_Name, G),
  % A quantity takes the quantity space of its quantity definition.
  rdf_has(QD, qsim:quantity_space, QS),
  rdf_assert(Q, qsim:quantity_space, QS, G).

%! add_quantity_definition(
%!   +Graph:atom,
%!   +QD_Name:atom,
%!   +QV_Names:list(atom),
%!   -QD:quantity_definition
%! ) is det.
% Creates a quantity definition with the given name,
% with quantity values with the given names.

add_quantity_definition(G, QD_Name, QV_Names, QD):-
  format(atom(QD_Name0), 'qd_~w', [QD_Name]),
  rdf_global_id(qsim:QD_Name0, QD),
  rdfs_assert_subclass(QD, qsim:'Quantity', G),
  rdfs_assert_label(QD, QD_Name, G),
  add_quantity_space(G, QD_Name, QV_Names, QS),
  rdf_assert(QD, qsim:quantity_space, QS, G).

%! add_quantity_space(
%!   +Graph:atom,
%!   +QD_Name:atom,
%!   +QV_Names:list(atom),
%!   -QS:quantity_space
%! ) is det.
% Creates a quantity space with the given name,
% with quantity values with the given names.

add_quantity_space(G, QD_Name, QV_Names, QS):-
  format(atom(QS_Name0), 'qs_~w', [QD_Name]),
  rdf_global_id(qsim:QS_Name0, QS),
  term_to_atom(QV_Names, QS_Name),
  rdf_assert_individual(QS, qsim:'QuantitySpace', G),
  rdfs_assert_label(QS, QS_Name, G),
  add_quantity_values(G, QD_Name, QV_Names, Ls, QVs),
  rdf_assert_list(Ls, L_List, G),
  rdf_assert(QS, qsim:landmarks, L_List, G),
  assert_quantity_value_relations(G, QVs),
  rdf_assert_list(QVs, QV_List, G),
  rdf_assert(QS, qsim:values, QV_List, G).

%! add_quantity_values(
%!   +QD_Name:atom,
%!   +QV_Names:list(atom),
%!   +Graph:atom,
%!   -Ls:list(uri),
%!   -QVs:list(uri)
%! ) is det.

add_quantity_values(G, QD_Name, QV_Names, Ls, QVs):-
  add_quantity_values_(neg, G, QD_Name, QV_Names, Ls, QVs).
add_quantity_values_(_Sign, _G, _QD_Name, [], [], []):- !.
add_quantity_values_(
  In,
  G,
  QD_Name,
  [QV_Name | QV_Names],
  [Landmark | Landmarks],
  QVs
):-
  % The prosody predicate allows us to move from negative values,
  % via the zero value, to positive values.
  prosody(In, QV_Name, QV_ClassName, Out), !,
  add_landmark(G, QD_Name, QV_Name, Landmark),
  rdf_global_id(qsim:QV_ClassName, QV_Class),
  rdf_assert_individual(Landmark, QV_Class, G),
  add_quantity_values_(Out, G, QD_Name, QV_Names, Landmarks, QVs0),
  % If there are additional landmarks, then add an interval QV.
  (
    Landmarks = [NextLandmark | _Landmarks]
  ->
    % An interval value is a pair or list or length 2.
    rdf_assert_list([Landmark, NextLandmark], Interval, G),
    rdf_assert(Landmark, qsim:next_landmark, NextLandmark, G),
    % We distinguish between negative and positive intervals.
    (
      rdf_global_id(qsim:'NegativeLandmark', Landmark)
    ->
      rdf_assert_individual(Interval, qsim:'NegativeInterval', G)
    ;
      rdf_assert_individual(Interval, qsim:'PositiveInterval', G)
    ),
    QVs = [Landmark, Interval | QVs0]
  ;
    % This was the last landmark.
    QVs = [Landmark | QVs0]
  ).

add_state(G, S):-
  flag(state, ID0, ID0 + 1),
  format(atom(ID), 's_~w', [ID0]),
  rdf_global_id(qsim:ID, S),
  rdf_assert_individual(S, qsim:'State', G),
  atom_number(ID1, ID0),
  rdfs_assert_label(S, ID1, G).

%! add_state(+Graph:atom, +Statements:list(iri), -State:iri) is det.

add_state(G, Stmts, S):-
  add_state(G, S),
  forall(
    member(Stmt, Stmts),
    rdf_assert(S, qsim:statement, Stmt, G)
  ).

add_static(G, Goal):-
  call(Goal, G, Stmt),
  rdf_assert(qsim:static, qsim:statement, Stmt, G).

add_sum(Q1, Q2, Q3, G, Stmt):-
  rdf_assert_list([Q1, Q2], Inputs, G),
  rdf_assert_statement(Inputs, qsim:sum, Q3, G, Stmt).

assert_quantity_value_relations(_G, []):- !.
assert_quantity_value_relations(_G, [_]):- !.
assert_quantity_value_relations(G, [QV1, QV2 | T]):-
  rdf_assert(QV1, qsim:next_value, QV2, G),
  assert_quantity_value_relations(G, [QV2 | T]).


%! prosody(
%!   ?FromSign:oneof([neg,pos]),
%!   ?QV_Name:atom,
%!   ?Label:atom,
%!   ?ToSign:oneof([neg,pos])
%! ) is nondet.
% Positive qualitative values come after zero, which comes after
% negative qualitative values.
%
% Quantity value name `zero` causes the transition.

prosody(neg, zero,     'ZeroLandmark',     pos).
prosody(neg, _QV_Name, 'NegativeLandmark', neg).
prosody(pos, _QV_Name, 'PositiveLandmark', pos).

