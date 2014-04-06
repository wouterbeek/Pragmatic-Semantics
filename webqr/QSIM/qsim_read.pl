:- module(
  qsim_read,
  [
% DYNAMIC
    derivative/4, % +Graph:atom
                  % ?State:state
                  % ?Quantity:quantity
                  % ?DeriavtiveQuantityValue:derivative
    magnitude/4, % +Graph:atom
                 % ?State:state
                 % ?Quantity:quantity
                 % ?MagnitudeQuantityValue:magnitude
    magnitude_exclusion/4, % +Graph:atom
                           % ?State:state
                           % ?Quantity:quantity
                           % ?MagnitudeQuantityValue:magnitude
    state/2, % ?State:state
             % ?Graph:atom

% STATIC
    correspondence/3, % ?Graph:atom
                      % ?MQV1:magnitude
                      % ?MQV2:magnitude
    derivative_decreasing/1, % ?V
    derivative_increasing/1, % ?V
    derivative_relation/3, % ?Graph:atom
                           % ?Quantity1:quantity
                           % ?Quantity2:quantity
    derivative_steady/1, % ?V
    interval/8, % ?Graph:atom
                % ?Q:quantity
                % ?QName:atom
                % ?L1:landmark
                % ?L1_Name:atom
                % ?L2:landmark
                % ?L2_Name:atom
                % ?I:interval
    is_highest_interval/3, % +Graph:atom
                           % +Quantity:iri
                           % +Interval:list(iri)
    is_lowest_interval/3, % +Graph:atom
                          % +Quantity:iri
                          % +Interval:list(iri)
    is_outer_interval/3, % +Graph:atom
                         % +Quantity:iri
                         % +Interval:list(iri)
    is_highest_landmark/3, % +Graph:atom
                           % +Quantity:iri
                           % +Landmark:iri
    is_lowest_landmark/3, % +Graph:atom
                          % +Quantity:iri
                          % +Landmark:iri
    is_outer_landmark/3, % +Graph:atom
                         % +Quantity:iri
                         % +Landmark:iri
    landmark/5, % ?Graph:atom
                % ?Q:quantity
                % ?QName:atom
                % ?L:landmark
                % ?L_Name:atom
    monotonic_decreasing/3, % +Graph:atom
                            % ?Q1:quantity
                            % ?Q2:quantity
    monotonic_increasing/3, % +Graph:atom
                            % ?Q1:quantity
                            % ?Q2:quantity
    quantity/5, % +Graph:atom
                % ?Quantity:quantity
                % ?QuantityName:atom
                % ?Landmarks:rdf_list(landmark)
                % ?QuantityValues:rdf_list(quantity_value)
    quantity_definition/5, % +Graph:atom
                           % ?QuantityDefinition:quantity_definition
                           % ?QuantityDefinitionName:atom
                           % ?QuantitySpace:quantity_space
                           % ?QuantitySpaceName:atom
    sum_relation/4, % +Graph:atom
                    % ?Quantity1:quantity
                    % ?Quantity2:quantity
                    % ?Quantity3:quantity

% GENERIC
    qsim_statement/6 % ?State:state
                     % ?Subject:or([bnode,uri])
                     % ?Predicate:uri
                     % ?Object:or([bnode,literal,uri])
                     % ?Graph:atom
                     % ?Statement:statement
  ]
).

/** <module> QSIM read

Reading in QSIM.

@author Wouter Beek
@version 2013/02-2013/03, 2013/07, 2013/09
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf(rdf_reification)).
:- use_remote_module(rdf(rdf_list)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdfs(rdfs_read)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').

% DYNAMIC
:- rdf_meta(derivative(?,r,r,r)).
:- rdf_meta(magnitude(?,r,r,r)).
:- rdf_meta(magnitude_exclusion(?,r,r,r)).
:- rdf_meta(state(?,r)).
% STATIC
:- rdf_meta(correspondence(?,r,r)).
:- rdf_meta(derivative_relation(?,r,r)).
:- rdf_meta(interval(r,?,r,?,r,?,r)).
:- rdf_meta(is_highest_interval(+,r,t)).
:- rdf_meta(is_lowest_interval(+,r,t)).
:- rdf_meta(is_outer_interval(+,r,t)).
:- rdf_meta(is_highest_landmark(+,r,r)).
:- rdf_meta(is_lowest_landmark(+,r,r)).
:- rdf_meta(is_outer_landmark(+,r,r)).
:- rdf_meta(landmark(?,r,?,r,?)).
:- rdf_meta(magnitude(r,?,r,?)).
:- rdf_meta(monotonic_decreasing(?,r,r)).
:- rdf_meta(monotonic_increasing(?,r,r)).
:- rdf_meta(quantity(?,r,?,r,r)).
:- rdf_meta(quantity_definition(?,r,?,r,?)).
:- rdf_meta(sum_relation(?,r,r,r)).
% GENERIC
:- rdf_meta(qsim_statement(r,r,r,r,?)).
:- rdf_meta(qsim_statement(r,r,r,r,?,r)).



% DYNAMIC %

derivative(G, S, Q, DQV):-
  maplist(nonvar, [G,S,Q]), !,
  qsim_statement(S, Q, qsim:derivative, DQV, G), !.
derivative(G, S, Q, DQV):-
  qsim_statement(S, Q, qsim:derivative, DQV, G).

%! magnitude(?G:atom, ?S:state, ?Q:quantity, ?MQV:magnitude) is nondet.
% Quantity magnitude `MQV` is the value of quantity `Q` in state `S`.

magnitude(G, S, Q, MQV):-
  maplist(nonvar, [G,S,Q]), !,
  qsim_statement(S, Q, qsim:magnitude, MQV, G), !.
magnitude(G, S, Q, MQV):-
  qsim_statement(S, Q, qsim:magnitude, MQV, G).

magnitude_exclusion(G, S, Q, MQV):-
  maplist(nonvar, [G,S,Q]), !,
  qsim_statement(S, Q, qsim:magnitude_exlusion, MQV, G), !.
magnitude_exclusion(G, S, Q, MQV):-
  qsim_statement(S, Q, qsim:magnitude_exlusion, MQV, G).

%! state(?Graph:atom, ?State:iri) is nondet.
% Qualitative states, excluding the cache state and the static state.

state(G, S):-
  rdfs_individual_of(S, qsim:'State'),
  \+ rdf_global_id(qsim:cache, S),
  \+ rdf_global_id(qsim:static, S),
  once(rdf(S, _, _, G)).



% STATIC %

%! correspondence(?Graph:atom, ?MQV1:magnitude, ?MQV2:magnitude) is nondet
% Correspondences between magnitude quantity values.

correspondence(G, MQV1, MQV2):-
  qsim_statement(qsim:static, MQV1, qsim:correspondence, MQV2, G).

%! derivative_decreasing(?V)
% The qualitative value for a decreasing derivative.

derivative_decreasing(V):-
  rdf_global_id(qsim:dec, V).

%! derivative_increasing(?V)
% The qualitative value for an increasing derivative.

derivative_increasing(V):-
  rdf_global_id(qsim:inc, V).

derivative_relation(G, Q1, Q2):-
  qsim_statement(qsim:static, Q1, qsim:d_rel, Q2, G).

%! derivative_steady(?V)
% The qualitative value for a steady derivative.

derivative_steady(V):-
  rdf_global_id(qsim:std, V).

%! landmark(
%!   ?Graph:atom,
%!   ?Q:quantity,
%!   ?QName:atom,
%!   ?L:landmark,
%!   ?L_Name:atom
%! ) is nondet.
% A quantity-landmark pair, involving both the objects and their names.

landmark(G, Q, QName, L, L_Name):-
  quantity(G, Q, QName, Ls, _QVs),
  rdf_list_member(L, Ls),
  rdfs_label(L, L_Name).

%! interval(
%!   ?Graph:atom,
%!   ?Quantity:quantity,
%!   ?QuantityName:atom,
%!   ?OpenLandmark:landmark,
%!   ?OpenLandmarkName:atom,
%!   ?CloseLandmark:landmark,
%!   ?CloseLandmarkName:atom,
%!   ?Interval:rdf_list
%! ) is nondet.

interval(G, Q, QName, L1, L1_Name, L2, L2_Name, I):-
  landmark(G, Q, QName, L1, L1_Name),
  rdf_has(L1, qsim:next_landmark, L2),
  landmark(G, Q, QName, L2, L2_Name),
  rdf_has(L1, qsim:next_value, I),
  rdf_has(I, qsim:next_value, L2).

is_highest_interval(G, Q, [_,L]):-
  quantity(G, Q, _QName, Ls, _QVs),
  last(Ls, L), !.

is_lowest_interval(G, Q, [L,_]):-
  quantity(G, Q, _QName, Ls, _QVs),
  Ls = [L|_], !.

%! is_outer_interval(
%!   +Graph:atom,
%!   +Quantity:iri,
%!   +Interval:list(iri)
%! ) is semidet.

is_outer_interval(G, Q, I):-
  is_highest_interval(G, Q, I), !.
is_outer_interval(G, Q, I):-
  is_lowest_interval(G, Q, I), !.

is_highest_landmark(G, Q, L):-
  quantity(G, Q, _QName, Ls, _QVs),
  rdf_list_last(Ls, L), !.

is_lowest_landmark(G, Q, L):-
  quantity(G, Q, _QName, Ls, _QVs),
  rdf_list_first(Ls, L), !.

%! is_outer_landmark(
%!   +Graph:atom,
%!   +Quantity:iri,
%!   +Interval:list(iri)
%! ) is semidet.

is_outer_landmark(G, Q, I):-
  is_highest_landmark(G, Q, I), !.
is_outer_landmark(G, Q, I):-
  is_lowest_landmark(G, Q, I), !.

monotonic_decreasing(G, Q1, Q2):-
  qsim_statement(qsim:static, Q1, qsim:m_neg, Q2, G).

monotonic_increasing(G, Q1, Q2):-
  qsim_statement(qsim:static, Q1, qsim:m_pos, Q2, G).

%! quantity(
%!   ?Graph:atom,
%!   ?Q:quantity,
%!   ?QName:atom,
%!   ?Ls:rdf_list(landmark),
%!   ?QVs:rdf_list(quantity_value)
%! ) is nondet.
% Quantities with their landmarks and quantity values.

quantity(G, Q, QName, Ls, QVs):-
  rdfs_individual_of(Q, qsim:'Quantity'),
  rdfs_label(Q, QName),
  rdf_has(Q, qsim:quantity_space, QS),
  rdf_has(QS, qsim:landmarks, Ls),
  rdf_has(QS, qsim:values, QVs),
  rdf(Q, _, _, G).

quantity_definition(G, QD, QD_Name, QS, QS_Name):-
  rdfs_subclass_of(QD, qsim:'Quantity'),
  rdfs_label(QD, QD_Name),
  rdf_has(QD, qsim:quantity_space, QS),
  rdfs_label(QS, QS_Name),
  rdf(QD, _, _, G).

sum_relation(G, Q1, Q2, Q3):-
  qsim_statement(qsim:static, Inputs, qsim:sum, Q3, G),
  rdf_list(Inputs, [Q1,Q2]).



% GENERIC %

%! qsim_statement(
%!   ?State:state,
%!   ?Subject:or([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:or([bnode,literal,uri]),
%!   ?Graph:atom
%! ) is nondet.
% @see Wrapper around qsim_statement/6.

qsim_statement(State, S, P, O, G):-
  qsim_statement(State, S, P, O, G, _Stmt).

%! qsim_statement(
%!   ?State:state,
%!   ?Subject:or([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:or([bnode,literal,uri]),
%!   ?Graph:atom,
%!   ?Statement:statement
%! ) is nondet.
% Pairs of states and the components that make up a proposition
% in that state.

qsim_statement(State, S, P, O, G, Stmt):-
  rdf_has(State, qsim:statement, Stmt),
  rdf_statement(S, P, O, G, Stmt).

