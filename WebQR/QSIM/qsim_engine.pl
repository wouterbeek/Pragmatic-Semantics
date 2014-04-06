:- module(
  qsim_engine,
  [
    op(500, xfx, :),
    op(100, xfx, ..),
    simulate/2 % +Graph:atom
               % +InitialState:state
  ]
).

/** <module> QSIM

A qualitative simulator.

---+ Datatypes

---++ Derivative

| *Value* | *Sign* |
| =inc=   | =pos=  |
| =std=   | =zero= |
| =dec=   | =neg=  |

---++ Domain

A list of landmark values.

---++ Landmark

A user-chosen label.

| *Value* |
| =minf=  |
| =inf=   |

---++ Magnitude

Either a landmark or a pair of adjacent landmarks.

---++ Quantity

A triple of domain, magnitude and derivative.

---++ Sign

| *Value* |
| =pos=   |
| =zero=  |
| =neg=   |

---++ State

A list of quantities.

---+ Modeling language

---++ Correspondences

Using correspondence/3.

---++ Initial state

The initial state is described using initial_state/1 and consists of a list
of quantities.

---++ Landmarks

Using landmark/2.

---++ Legal state

The legal state constraints are given by legal_state/2 and can consist of
the following statements:
    * derivative/2
    * Explicit quantity value assignments
    * Negation as failure
    * @tbd monotonic_decreasing_/3
    * monotonic_increasing_/3
    * sum_/3

---+++ Explicit quantity value assignments

Example for setting the magnitude value of a quantity called =level=:

==
Level = level:top..inf/_Derivative
==

Example for setting the derivative value of a quantity called =inflow=:

==
Inflow = inflow:_Magnitude/zero
==

---+++ Negation as failure

Example for stating that quantity =level= should not have a certain magnitude
value:

==
% The liquid does not overflow the tub.
\+ Level = level:top..inf/_Derivative.
==

---+ Behavior of the bath tub example

| *Amount*       | *Level*       | *Outflow*        | *Netflow*        |
| zero/inc       | zero/inc      | zero/inc         | inflow/dec       |
| zero..full/inc | zero..top/inc | zero..inflow/inc | zero..inflow/dec |

@author Wouter Beek
@version 2013/02-2013/03, 2013/07, 2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_debug)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qsim(qsim_build)).
:- use_module(qsim(qsim_export)).
:- use_module(qsim(qsim_read)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_reification)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').

% CORRESPONDENCE
:- rdf_meta(correspondence(r,r,r,r,r,r)).
% DERIVATIVE
:- rdf_meta(derivative(r)).
:- rdf_meta(derivative_relation_(r,r,r)).
:- rdf_meta(derivative_sign(r,r,r)).
:- rdf_meta(derivative_sign_conversion(r,r)).
:- rdf_meta(derivative_sum(r,r,r,r)).
% MAGNITUDE
:- rdf_meta(compare_landmarks(r,r,r)).
:- rdf_meta(magnitude_sign(r,r,r)).
:- rdf_meta(magnitude_sign(r,r,r,r)).
% MONOTONIC RELATION
:- rdf_meta(monotonic_increasing_(r,r,r)).
% QUALITATIVE ARITHMETIC
:- rdf_meta(sum_(r,r,r,r)).
:- rdf_meta(sum_sign(r,r,r,r)).
% SIMULATE
:- rdf_meta(simulate(+,r)).
:- rdf_meta(simulate_(+,r)).
% STATE LEGALITY
:- rdf_meta(exsiting_statements(+,+,r)).
:- rdf_meta(legal_state(+,r)).
:- rdf_meta(legal_state_transitions(+,r,-)).
:- rdf_meta(unlike_statements(+,r,+)).
% TRANSITION
:- rdf_meta(quantity_transition(+,r,r,r,r)).
:- rdf_meta(quantity_transition_interval(+,r,r,r,r,r)).
:- rdf_meta(quantity_transition_landmark(+,r,r,r,r,r)).



% CORRESPONDENCE %

%! correspondence(
%!   ?Q1:quantity, ?QV1:landmark,
%!   ?Q2:quantity, ?QV2:landmark,
%!   ?Q3:quantity, ?QV3:landmark
%! ) is nondet.
% Constraint specifications for magnitudes.
% Constraints can only be defined between landmark values (and this between
% interval values).

correspondence(_Q1, QV1, _Q2, QV2, _Q3, QV3):-
  rdfs_individual_of(QV1, qsim:'ZeroLandmark'),
  rdfs_individual_of(QV2, qsim:'ZeroLandmark'),
  rdfs_individual_of(QV3, qsim:'ZeroLandmark').
correspondence(Q1, QV1, _Q2, QV2, Q3, QV3):-
  rdf_has(Q1, qsim:quantity_space, QS1),
  rdf_has(QS1, qsim:point_value, QV1),
  rdfs_individual_of(QV2, qsim:'ZeroLandmark'),
  rdf_has(Q3, qsim:quantity_space, QS3),
  rdf_has(QS3, qsim:point_value, QV3).
correspondence(_Q1, QV1, Q2, QV2, Q3, QV3):-
  rdfs_individual_of(QV1, qsim:'ZeroLandmark'),
  rdf_has(Q2, qsim:quantity_space, QS2),
  rdf_has(QS2, qsim:point_value, QV2),
  rdf_has(Q3, qsim:quantity_space, QS3),
  rdf_has(QS3, qsim:point_value, QV3).



% DERIVATIVE %

%! derivative(?QDV:derivative) is nondet.

derivative(DQV):-
  derivative_sign_conversion(DQV, _Sg).

%! derivative_relation_(
%!   +State:state,
%!   +FromQuantity:quantity,
%!   +ToQuantity:quantity
%! ) is semidet.
% The temporal derivative of the former quantity is qualitatively equal to
% the latter quantity.

derivative_relation_(S, Q1, Q2):-
  derivative_sign(S, Q1, Sg1),
  magnitude_sign(S, Q2, Sg2),
  Sg1 == Sg2.

%! derivative_sign(?S:state, ?Q:quantity, ?Sg:sign) is nondet.
% Translation between derivatives and signs.

derivative_sign(S, Q, Sg):-
  derivative(_G, S, Q, DQV),
  derivative_sign_conversion(DQV, Sg).

%! derivative_sign_conversion(?QDV:derivative, ?Sg:sign) is nondet.

derivative_sign_conversion(DQV, Sg):-
  var(DQV), var(Sg), !,
  derivative_sign_conversion_(DQV, Sg).
derivative_sign_conversion(DQV, Sg):-
  once(derivative_sign_conversion_(DQV, Sg)).

derivative_sign_conversion_(DQV, Sg):-
  rdf_global_id(qsim:dec, DQV),
  rdf_global_id(qsim:neg, Sg).
derivative_sign_conversion_(DQV, Sg):-
  rdf_global_id(qsim:std, DQV),
  rdf_global_id(qsim:zero, Sg).
derivative_sign_conversion_(DQV, Sg):-
  rdf_global_id(qsim:inc, DQV),
  rdf_global_id(qsim:pos, Sg).

%! derivative_sum(
%!   +S:state,
%!   ?QVD1:derivative, ?QVD2:derivative, ?QVD3:derivative
%! ) is nondet.
% Qualitative sum over directions of change.

derivative_sum(S, DQV1, DQV2, DQV3):-
  maplist(derivative_sign(S), [DQV1, DQV2, DQV3], [Sg1, Sg2, Sg3]),
  sum_sign(Sg1, Sg2, Sg3).



% MAGNITUDE %

%! compare_landmarks(+QV1:landmark, +QV2:landmark, -Sg:sign) is det.

compare_landmarks(QV, QV, Sg):- !,
  rdf_global_id(qsim:zero, Sg).
compare_landmarks(QV1, QV2, Sg):-
  (
    rdf_list_occurs_before(QV1, QV2)
  ->
    rdf_global_id(qsim:neg, Sg)
  ;
    rdf_list_occurs_after(QV1, QV2)
  ->
    rdf_global_id(qsim:pos, Sg)
  ).

%! magnitude_sign(
%!   +State,
%!   +Quantity,
%!   -Sign:oneof([qsim:neg,qsim:zero,qsim:pos])
%! ) is det.

% By default, magnitude values are taken relative to zero.
magnitude_sign(S, Q, Sg):-
  landmark(_G, Q, _Q_Name, Rel, zero),
  magnitude_sign(S, Q, Rel, Sg).

magnitude_sign(S, Q, Rel, Sg):-
  magnitude(_G, S, Q, QV),
  (
    rdfs_individual_of(QV, qsim:'Landmark')
  ->
    compare_landmarks(QV, Rel, Sg)
  ;
    rdfs_individual_of(QV, qsim:'Interval')
  ->
    rdf_list_first(QV, QV_First),
    (
      compare_landmarks(QV_First, Rel, qsim:neg)
    ->
      rdf_global_id(qsim:neg, Sg)
    ;
      rdf_global_id(qsim:pos, Sg)
    )
  ).



% MONOTONIC RELATION %

%! monotonic_increasing_(
%!   +State:state,
%!   +Quantity1:quantity,
%!   +Quantity2:quantity
%! ) is semidet.
% The quantities are monotonically increasing functions of each other.
% Monotonicity is a symmetric relation.
%
% Two quantities are monotonically increasing if
%   1. their derivative values are the same, and
%   2. all their reciprocal correspondences are respected.
%
% "It is not the case that there is a correspondence between the
%  magnitude quantity values of the respective quantities such that
%  the relative signs of the quantities are not the same in
%  the given state."
%
% Note that 'relative sing' means relative to the magntiude quantity values
% between which the correspondence is defined.
%
% By using double negation a universal constraint can be verified
% efficiently (one violation would already invalidate it).

% The derivatives are the same.
monotonic_increasing_(S, Q1, Q2):-
  derivative(G, S, Q1, DQV),
  derivative(G, S, Q2, DQV),
  % "It is not the case that there is a correspondence between the
  %  magnitude quantity values of the respective quantities such that
  %  the relative signs of the quantities are not the same in
  %  the given state."
  % Note that 'relative sign' means relative to the magntiude quantity values
  % between which correspondences are defined.
  % By using double negation a universal constraint can be verified
  % efficiently (one violation would already invalidate it).
  \+ (
    % A correspondence between two qualitative magnitudes.
    correspondence(G, Rel1, Rel2),
    once(landmark(G, Q1, _Q1_Name, Rel1, _L_Name1)),
    once(landmark(G, Q2, _Q2_Name, Rel2, _L_Name2)),
    magnitude_sign(S, Q1, Rel1, Sg1),
    magnitude_sign(S, Q2, Rel2, Sg2),
    Sg1 \== Sg2
  ).



% QUALITATIVE ARITHMETIC %

%! sum_(+S:state, ?Q1:quantity, ?Q2:quantity, ?Q3:quantity) is nondet.
% Q3 = Q1 + Q2
% Qualitative sum over qualitative values of form =|Domain:Magnitude/Dir|=.
% When called, this predicate assumes that the domains of all three arguments
% are instantiated.

sum_(S, Q1, Q2, Q3):-
  derivative_sum(S, Q1, Q2, Q3),
  % Magnitude1+Magnitude2=Magnitude3 must be consistent with all
  % correspondences between values.
  \+ (
    correspondence(Q1, Rel1, Q2, Rel2, Q3, Rel3),
    maplist(
      magnitude_sign(S),
      [Q1,Q2,Q3],
      [Rel1,Rel2,Rel3],
      [Sg1,Sg2,Sg3]
    ),
    \+ sum_sign(Sg1, Sg2, Sg3)
  ).

%! sum_sign(?Sg1:sign, ?Sg2:sign, ?Sg3:sign) is nondet.
% Laws of qualitative summation.
% Sg3 = Sg1 + Sg2, qualitative sum over domain [pos,zero,neg]
%
% | *=sum_sign=* | *=pos=*                | *=zero=* | *=neg=*                |
% | *=pos=*      | =pos=                  | =pos=    | =neg= V =zero= V =pos= |
% | *=zero=*     | =pos=                  | =zero=   | =neg=                  |
% | *=neg=*      | =neg= V =zero= V =pos= | =neg=    | =neg=                  |

sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:pos,  Sg1),
  rdf_global_id(qsim:pos,  Sg2),
  rdf_global_id(qsim:pos,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:pos,  Sg1),
  rdf_global_id(qsim:zero, Sg2),
  rdf_global_id(qsim:pos,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:pos,  Sg1),
  rdf_global_id(qsim:neg,  Sg2),
  rdf_global_id(qsim:pos,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:pos,  Sg1),
  rdf_global_id(qsim:neg,  Sg2),
  rdf_global_id(qsim:zero, Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:pos,  Sg1),
  rdf_global_id(qsim:neg,  Sg2),
  rdf_global_id(qsim:neg,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:zero, Sg1),
  rdf_global_id(qsim:pos,  Sg2),
  rdf_global_id(qsim:pos,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:zero, Sg1),
  rdf_global_id(qsim:zero, Sg2),
  rdf_global_id(qsim:zero, Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:zero, Sg1),
  rdf_global_id(qsim:neg,  Sg2),
  rdf_global_id(qsim:neg,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:neg,  Sg1),
  rdf_global_id(qsim:pos,  Sg2),
  rdf_global_id(qsim:pos,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:neg,  Sg1),
  rdf_global_id(qsim:pos,  Sg2),
  rdf_global_id(qsim:zero, Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:neg,  Sg1),
  rdf_global_id(qsim:pos,  Sg2),
  rdf_global_id(qsim:neg,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:neg,  Sg1),
  rdf_global_id(qsim:zero, Sg2),
  rdf_global_id(qsim:neg,  Sg3).
sum_sign(Sg1, Sg2, Sg3):-
  rdf_global_id(qsim:neg,  Sg1),
  rdf_global_id(qsim:neg,  Sg2),
  rdf_global_id(qsim:neg,  Sg3).



% SIMULATE %

%! simulate(+Graph:atom, +InitialState:state) is det.

simulate(G, InitialState):-
  % Thype checks.
  rdf_graph(G),
  state(G, InitialState),

  % The initial state must be legal.
  legal_state(G, InitialState),

  simulate_(G, InitialState).

% Breath-first simulation engine.
simulate_(G, From):-
  % First we generate all possible transitions.
  legal_state_transitions(G, From, Tos),
  % Then we continue simulation for each of these.
  maplist(simulate_(G), Tos).



% STATE LEGALITY %

%! existing_statements(
%!   +Graph:atom,
%!   +Statements:list(iri),
%!   -State:iri
%! ) is semidet.
% Returns the state in which the given statements are asserted,
% if such a state exists.

existing_statements(G, Stmts, S):-
  state(G, S),
  forall(
    member(Stmt, Stmts),
    rdf(S, qsim:statement, Stmt, G)
  ), !.

legal_state(G, S):-
  % Every state must have the magnitude values that are set
  % in the static state.
  forall(
    magnitude(G, qsim:static, Q, MQV),
    magnitude(_G1, S, Q, MQV)
  ),
  % Every state must have the derivative values that are set
  % in the static state.
  forall(
    derivative(G, qsim:static, Q, DQV),
    derivative(_G2, S, Q, DQV)
  ),
  % Every state must not have the magnitude values that are specifically
  % forbidden according to the static state.
  forall(
    magnitude_exclusion(G, qsim:static, Q, MQV),
    \+ magnitude(_G3, S, Q, MQV)
  ),
  forall(
    monotonic_increasing(G, Q1, Q2),
    monotonic_increasing_(S, Q1, Q2)
  ),
  forall(
    derivative_relation(G, Q1, Q2),
    derivative_relation_(S, Q1, Q2)
  ),
  forall(
    sum_relation(G, Q1, Q2, Q3),
    sum_(S, Q1, Q2, Q3)
  ).

%! legal_state_transitions(+Graph:atom, +From:iri, -Tos:list(iri)) is det.
% A legal transition is a system transition between different legal states.

legal_state_transitions(G, From, Tos):-
  % Collect all quantities.
  findall(
    Quantity,
    rdfs_individual_of(Quantity, qsim:'Quantity'),
    Quantities
  ),

  % Calculate *all* quantity transitions in terms of the statements
  % that describe the quantity values that are the result of those
  % transitions.
  aggregate_all(
    set(M_Stmts/D_Stmts),
    maplist(quantity_transition(G, From), Quantities, M_Stmts, D_Stmts),
    Triples
  ),
  
  % Each triple describes a state.
  % There are three types of states:
  %   1. Illegal states, no nothing.
  %   2. Existing legal states, add a transition (if this does not yet exist).
  %   3. Nonexisting legal states, add a state and a transition.
  triples_to_states(G, From, Triples, Tos).

triples_to_states(_, _From, [], []):- !.
triples_to_states(G, From, [M_Stmts/D_Stmts|Triples], ToStates):-
  % For each potential transition of the quantities,
  % we take the statements.
  append(M_Stmts, D_Stmts, Stmts),
  (
    % Not the same as the previous state?
    unlike_statements(G, From, Stmts),

    % Filter statements illegal that describe illegal states.
    legal_statements(G, Stmts)
  ->
    (
      existing_statements(G, Stmts, To)
    ->
      % If the statements are alreay described by an existing state,
      % then link to that state (and do nothing else).
      (
        % The link already exists.
        rdf(From, qsim:next_state, To, G), !
      ;
        % The link is added.
        rdf_assert(From, qsim:next_state, To, G),

        % DEB
        dcg_debug(qsim_egnine, qr_transition(0, true, From, To))
      ),

      % Do not collect these states (only new ones are collected).
      triples_to_states(G, From, Triples, ToStates)
    ;
      % Create a new state in which the statements hold.
      add_state(G, Stmts, To),
      rdf_assert(From, qsim:next_state, To, G),

      % DEB
      dcg_debug(qsim_engine, qr_state(0, true, ascii, natlang, G, To)),
      dcg_debug(qsim_engine, qr_transition(0, true, From, To)),

      % Include this state.
      triples_to_states(G, From, Triples, T),
      ToStates = [To|T]
    )
  ;
    % Consider the other triples for 'to' states.
    triples_to_states(G, From, Triples, ToStates)
  ).

%! legal_statements(+Graph:atom, +Statements:list(iri)) is semidet.
% Succeeds if the given collection of statements describes a legal state.

legal_statements(G, Stmts):-
  % Clear the old statements in the cache state.
  rdf_retractall(qsim:cache, qsim:statement, _Stmt, G),

  % Populate the new cache state.
  forall(
    member(Stmt, Stmts),
    rdf_assert(qsim:cache, qsim:statement, Stmt, G)
  ),

  % Check the cache state for legality.
  legal_state(G, qsim:cache).

%! unlike_statements(
%!   +Graph:atom,
%!   +State:state,
%!   +Statements:list(statement)
%! ) is semidet.
% Succeeds if the given statements do not describe the given state.

unlike_statements(G, State, Stmts):-
  rdf(State, qsim:statement, Stmt, G),
  \+ member(Stmt, Stmts), !.



% TRANSITION %

%! quantity_transition(
%!   +Graph:atom,
%!   +FromState:state,
%!   +Quantity:quantity,
%!   -MagnitudeStatement:statement,
%!   -DerivativeStatement:statement
%! ) is nondet.
% State transitions for singular quantities,
% i.e., without taking any correspondence into account.

quantity_transition(G, From, Q, M_Stmt, D_Stmt):-
  derivative(G, From, Q, DQV),
  magnitude(G, From, Q, MQV),

  % We distinguish between _interval_ and _landmark_ transitions.
  (
    rdfs_individual_of(MQV, qsim:'Interval')
  ->
    quantity_transition_interval(G, Q, MQV, DQV, M_Stmt, D_Stmt)
  ;
    rdfs_individual_of(MQV, qsim:'Landmark')
  ->
    quantity_transition_landmark(G, Q, MQV, DQV, M_Stmt, D_Stmt)
  ).

% Steady, stay in interval.
quantity_transition_interval(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:std, Old_DQV),
  derivative(DQV),
  add_magnitude(Q, MQV, G, M_Stmt),
  add_derivative(Q, DQV, G, D_Stmt).
% Increasing, stay in interval.
quantity_transition_interval(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:inc, Old_DQV),
  (rdf_global_id(qsim:inc, DQV) ; rdf_global_id(qsim:std, DQV)),
  add_magnitude(Q, MQV, G, M_Stmt),
  add_derivative(Q, DQV, G, D_Stmt).
% Increasing, move to upper landmark.
quantity_transition_interval(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:inc, Old_DQV),

  % Take care when moving into the highest landmark.
  rdf_list_last(MQV, MQV_Last),
  (
    is_highest_landmark(G, Q, MQV_Last)
  ->
    % The lowest landmark does not allow a decreasing derivative.
    rdf_global_id(qsim:std, DQV)
  ;
    (rdf_global_id(qsim:inc, DQV) ; rdf_global_id(qsim:std, DQV))
  ),

  add_magnitude(Q, MQV_Last, G, M_Stmt),
  add_derivative(Q, DQV, G, D_Stmt).
% Decreasing, stay in interval
quantity_transition_interval(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:dec, Old_DQV),
  (rdf_global_id(qsim:dec, DQV) ; rdf_global_id(qsim:std, DQV)),
  add_magnitude(Q, MQV, G, M_Stmt),
  add_derivative(Q, DQV, G, D_Stmt).
% Decreasing, move to lower landmark.
quantity_transition_interval(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:dec, Old_DQV),

  % Take care when moving into the lowest landmark.
  rdf_list_first(MQV, MQV_First),
  (
    is_lowest_landmark(G, Q, MQV_First)
  ->
    % The lowest landmark does not allow a decreasing derivative.
    rdf_global_id(qsim:std, DQV)
  ;
    (rdf_global_id(qsim:dec, DQV) ; rdf_global_id(qsim:std, DQV))
  ),

  add_magnitude(Q, MQV_First, G, M_Stmt),
  add_derivative(Q, DQV, G, D_Stmt).

% Steady, stay on landmark.
quantity_transition_landmark(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:std, Old_DQV),
  add_magnitude(Q, MQV, G, M_Stmt),

  (
    % The highest landmark does not allow an increasing derivative.
    is_highest_landmark(G, Q, MQV)
  ->
    ( rdf_global_id(qsim:std, DQV)
    ; rdf_global_id(qsim:dec, DQV))
  ;
    % The lowest landmark does not allow a decreasing derivative.
    is_lowest_landmark(G, Q, MQV)
  ->
    ( rdf_global_id(qsim:inc, DQV)
    ; rdf_global_id(qsim:std, DQV))
  ;
    % A non-outer landmark allows any derivative.
    ( rdf_global_id(qsim:inc, DQV)
    ; rdf_global_id(qsim:std, DQV)
    ; rdf_global_id(qsim:dec, DQV))
  ),

  add_derivative(Q, DQV, G, D_Stmt).
% Increasing, move into upper interval.
quantity_transition_landmark(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:inc, Old_DQV),
  rdf(MQV, qsim:next_value, MQV_Upper, G),
  add_magnitude(Q, MQV_Upper, G, M_Stmt),
  rdf_global_id(qsim:inc, Inc),
  add_derivative(Q, Inc, G, D_Stmt).
% Decreasing, move into lower interval.
quantity_transition_landmark(G, Q, MQV, Old_DQV, M_Stmt, D_Stmt):-
  rdf_global_id(qsim:dec, Old_DQV),
  rdf(MQV_Lower, qsim:next_value, MQV, G),
  add_magnitude(Q, MQV_Lower, G, M_Stmt),
  rdf_global_id(qsim:dec, Dec),
  add_derivative(Q, Dec, G, D_Stmt).

