:- module(
  qr_sim,
  [
    deriv/3, % +Space:space
             % +Quantity1:quantity
             % +Quantity2:quantity
    mplus/3, % +Space:space
             % +Quantity1:quantity
             % +Quantity2:quantity
    simulate/1, % +InputSpace:input_space
    sum/4 % +Space:space
          % +Quantity1:quantity
          % +Quantity2:quantity
          % +Quantity3:quantity
  ]
).

/** <module> QR SIM

QSIM implementation.

@author Wouter Beek
@version 2012/05
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_db)).
:- use_module(ccm(ccm_verb)).
:- use_module(generic(meta_ext)).
:- use_module(library(semweb/rdfs)).
:- use_module(qr(qr_api)).
:- use_module(rdfs(rdfs_read)).



add_triple(
  Space,
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue
):-
  magnitude_quantity_value_expression_definition(
    MagnitudeQuantityValueExpressionDefinition
  ),!,
  add_expression(
    Quantity,
    MagnitudeQuantityValueExpressionDefinition,
    MagnitudeQuantityValue,
    MagnitudeQuantityValueExpression
  ),
  add_point(
    MagnitudeQuantityValueExpression,
    Space,
    _MagnitudeQuantityValuePoint
  ),
  derivative_quantity_value_expression_definition(
    DerivativeQuantityValueExpressionDefinition
  ),!,
  add_expression(
    Quantity,
    DerivativeQuantityValueExpressionDefinition,
    DerivativeQuantityValue,
    DerivativeQuantityValueExpression
  ),
  add_point(
    DerivativeQuantityValueExpression,
    Space,
    _DerivativeQuantityValuePoint
  ).

%% copy_space(+Space1:space, +Space2:space) is det.
% Copies points between spaces.
%
% @param Space1 The space points are copied from.
% @param Space2 The space points are copied to.

copy_space(Space1, Space2):-
  rdf_transaction(
    forall(
      point(Expression, Space1, Point),
      add_point(Expression, Space2, _Point)
    )
  ).

%% clear_temp_space(-TempSpace:space) is det.
% Clears the temporary space and returns a pointer.
%
% @param TempSpace The temporary space.

clear_temp_space(TempSpace):-
  temp_space(TempSpace),
  rdf_transaction(
    forall(
      space_point(TempSpace, TempPoint),
      retract_point(TempPoint)
    )
  ).

correspond(EqualityExpression):-
  equal_to_expression(EqualityExpression),
  expression_from_argument(EqualityExpression, CalculusExpression),
  expression_from_argument(CalculusExpression, Quantity1),
  expression_to_argument(CalculusExpression, Quantity2),
  expression_to_argument(EqualityExpression, Quantity3),
  magnitude_quantity_value(Space, Quantity1, MagnitudeQuantityValue1),
  magnitude_quantity_value(Space, Quantity2, MagnitudeQuantityValue2),
  magnitude_quantity_value(Space, Quantity3, MagnitudeQuantityValue3),
  correspond1(
    Quantity1,
    MagnitudeQuantityValue1,
    Quantity2,
    MagnitudeQuantityValue2,
    Quantity3,
    MagnitudeQuantityValue3
  ).

correspond1(
  Quantity1,
  MagnitudeQuantityValue1,
  Quantity2,
  MagnitudeQuantityValue2,
  Quantity3,
  MagnitudeQuantityValue3
):-
  quantity(Quantity1),
  quantity(Quantity2),
  Quantity1 \== Quantity2,
  quantity(Quantity3),
  Quantity1 \== Quantity3,
  Quantity2 \== Quantity3,
  correspond2(
    Quantity1,
    MagnitudeQuantityValue1,
    Quantity2,
    MagnitudeQuantityValue2,
    Quantity3,
    MagnitudeQuantityValue3
  ).

correspond2(
  Quantity1,
  MagnitudeQuantityValue1,
  Quantity2,
  MagnitudeQuantityValue2,
  Quantity3,
  MagnitudeQuantityValue3
):-
  quantity_magnitude_quantity_value(Quantity1, MagnitudeQuantityValue1),
  zero_quantity_value(MagnitudeQuantityValue1),
  quantity_magnitude_quantity_value(Quantity2, MagnitudeQuantityValue2),
  zero_quantity_value(MagnitudeQuantityValue2),
  quantity_magnitude_quantity_value(Quantity3, MagnitudeQuantityValue3),
  zero_quantity_value(MagnitudeQuantityValue3).
correspond2(
  Quantity1,
  MagnitudeQuantityValue1,
  Quantity2,
  MagnitudeQuantityValue2,
  Quantity3,
  MagnitudeQuantityValue3
):-
  quantity_magnitude_quantity_value(Quantity1, MagnitudeQuantityValue1),
  zero_quantity_value(MagnitudeQuantityValue1),
  quantity_magnitude_quantity_value(Quantity2, MagnitudeQuantityValue2),
  quantity_magnitude_quantity_value(Quantity3, MagnitudeQuantityValue3),
  correspond3(MagnitudeQuantityValue2, MagnitudeQuantityValue3).
correspond2(
  Quantity1,
  MagnitudeQuantityValue1,
  Quantity2,
  MagnitudeQuantityValue2,
  Quantity3,
  MagnitudeQuantityValue3
):-
  quantity_magnitude_quantity_value(Quantity2, MagnitudeQuantityValue2),
  zero_quantity_value(MagnitudeQuantityValue2),
  quantity_magnitude_quantity_value(Quantity1, MagnitudeQuantityValue1),
  quantity_magnitude_quantity_value(Quantity3, MagnitudeQuantityValue3),
  correspond3(MagnitudeQuantityValue1, MagnitudeQuantityValue3).

correspond3(MagnitudeQuantityValue1, MagnitudeQuantityValue2):-
  \+(zero_quantity_value(MagnitudeQuantityValue1)),
  \+(zero_quantity_value(MagnitudeQuantityValue2)),
  quantity_value_definition_quantity_value(
    MagnitudeValueDefinition,
    MagnitudeQuantityValue1
  ),
  quantity_value_definition_quantity_value(
    MagnitudeValueDefinition,
    MagnitudeQuantityValue2
  ).

deriv_sg(DV, -1):-
  decreasing_derivative_quantity_value(DV).
deriv_sg(DV, 1):-
  increasing_derivative_quantity_value(DV).
deriv_sg(DV, 0):-
  steady_derivative_quantity_value(DV).

deriv(Space, Quantity1, Quantity2):-
  derivative_quantity_value(Space, Quantity1, DV1, _DE1),
  deriv_sg(DV1, Sg),
  magnitude_quantity_value(Space, Quantity2, MV2),
  quantity_zero_magnitude_quantity_value(Quantity2, ZeroMagnitude),
  relative_magnitude_quantity_value(MV2, ZeroMagnitude, Sg).

%% legal_transitions(
%%    +Behavior:list(space),
%%    -NewBehaviors:list(list(space))
%% ) is det.
% Returns all breadth-first results for the given behavior.
%
% @param Behavior A list of spaces. Newest space occurs first.
% @param NewBehaviors A list of behaviors, i.e. lists of spaces.

legal_transitions([Space | Behavior], NewBehaviors):-
  % Collect all triples.
  setoff(
    Quantity/MagnitudeQuantityValue/DerivativeQuantityValue,
    quantity_value(
      Space,
      Quantity,
      MagnitudeQuantityValue,
      _MagnitudeQuantityValueExpression,
      DerivativeQuantityValue,
      _DerivativeQuantityValueExpression
    ),
    Triples
  ),
  % Turn tripples into *all* traples (because of the RDF DB lock).
  setoff(
    Traples,
    system_transition(Triples, Traples),
    Trapless
  ),
  length(Trapless, Length), %DEB
  send(@pce, write_ln, '+++', Length), %DEB
  % Find all consecutive states (constructing possible behaviors).
  setoff(
    [NewState, Space | Behavior],
    (
      member(Traples, Trapless),
      legal_transition(Space, Traples, NewState)
    ),
    NewBehaviors
  ).

legal_transition(Space, Traples, NewState):-
  flag(cycle, Cycle, Cycle + 1), %DEB
  send(@pce, write_ln, Cycle), %DEB
  clear_temp_space(TempSpace),
  % Add traples to temporary space.
  forall(
    member(
      Quantity/MagnitudeQuantityValue/DerivativeQuantityValue,
      Traples
    ),
    add_triple(
      TempSpace,
      Quantity/MagnitudeQuantityValue/DerivativeQuantityValue
    )
  ),
  \+(exists_space(Traples)),
  legal_space(TempSpace),
  debug(ccm_verb, 'New legal state:', []), %DEB
  verbalize_model(TempSpace), %DEB
  add_state(noname, NewState),
  add_space_transition(Space, NewState, _SpaceTransition),
  copy_space(TempSpace, NewState).

exists_space(Traples):-
  space(Space),
  forall(
    member(Quantity/MagnitudeQuantityValue/DerivativeQuantityValue, Traples),
    (
      magnitude_quantity_value_expression_definition(
        MagnitudeQuantityValueExpressionDefinition
      ),
      point(
        MagnitudeQuantityValueExpression,
        Quantity,
        MagnitudeQuantityValueExpressionDefinition,
        MagnitudeQuantityValue,
        Space,
        _MagnitudeQuantityValuePoint
      ),
      derivative_quantity_value_expression_definition(
        DerivativeQuantityValueExpressionDefinition
      ),
      point(
        DerivativeQuantityValueExpression,
        Quantity,
        DerivativeQuantityValueExpressionDefinition,
        DerivativeQuantityValue,
        Space,
        _DerivativeQuantityValuePoint
      )
    )
  ).

%% mplus(+Space:space, +Quantity1:quantity, +Quantity2:quantity) is semidet.
% In Space, Quantity2 is a monotonically increasing function of Quantity1.
%
% @param Space A space.
% @param Quantity1 A quantity.
% @param Quantity2 A quantity.

mplus(Space, Quantity1, Quantity2):-
  % The two quantities have the same derivative value.
  derivative_quantity_value(
    Space,
    Quantity1,
    DerivativeQuantityValue1,
    _DerivativeExpression1
  ),
  derivative_quantity_value(
    Space,
    Quantity1,
    DerivativeQuantityValue2,
    _DerivativeExpression2
  ),
  rdfs_individual_of(DerivativeQuantityValue1, Class),
  rdfs_individual_of(DerivativeQuantityValue2, Class),
  
  % We use the definitions of the magnitude quantity values.
  magnitude_quantity_value(Space, Quantity1, MagnitudeQuantityValue1),
  magnitude_quantity_value(Space, Quantity2, MagnitudeQuantityValue2),

  % The magnitudes themselves need not agree, but their relative value
  % with respect to the correspondences is determined.
  \+((
    % The correspondence is used to determine the relative values.
    magnitude_quantity_value_correspondence(
      RelativeMagnitudeQuantityValue1,
      RelativeMagnitudeQuantityValue2
    ),
    relative_magnitude_quantity_value(
      MagnitudeQuantityValue1,
      RelativeMagnitudeQuantityValue1,
      Sign1
    ),
    relative_magnitude_quantity_value(
      MagnitudeQuantityValue2,
      RelativeMagnitudeQuantityValue2,
      Sign2
    ),
    Sign1 \== Sign2
  )).

qsum( 1, 1, 1).
qsum( 1, 0, 1).
qsum( 1,-1, 1).
qsum( 1,-1, 0).
qsum( 1,-1,-1).
qsum( 0, 1, 1).
qsum( 0, 0, 0).
qsum( 0,-1,-1).
qsum(-1, 1, 1).
qsum(-1, 1, 0).
qsum(-1, 1,-1).
qsum(-1, 0,-1).
qsum( 0, 0, 0).

relative_derivative_quantity_value(DQV, -1):-
  decreasing_derivative_quantity_value(DQV),
  !.
relative_derivative_quantity_value(DQV, 0):-
  steady_derivative_quantity_value(DQV),
  !.
relative_derivative_quantity_value(DQV, 1):-
  increasing_derivative_quantity_value(DQV).

%% relative_magnitude_quantity_value(
%%   +QuantityValue1:quantity_value,
%%   +QuantityValue2:quantity_value,
%%   -Sign:number
%% ) is det.
% Returns the relative sign of the first quantity value with respect to
% the second quantity value.
%
% @param QuantityValue1 A quantity value.
% @param QuantityValue2 A quantity value.
% @param Sign A sign, i.e. either -1, 0, or 1.

relative_magnitude_quantity_value(QuantityValue, QuantityValue, 0):-
  !.
relative_magnitude_quantity_value(QuantityValue1, QuantityValue2, 1):-
  quantity_value_greater_than(QuantityValue1, QuantityValue2),
  !.
relative_magnitude_quantity_value(QuantityValue1, QuantityValue2, -1):-
  quantity_value_greater_than(QuantityValue2, QuantityValue1).

%% retract_point(+Point:point) is det.
% Retracts the given point from the RDF DB.
%
% @param Point A point.

retract_point(Point):-
  rdf_retractall(Point, _, _, ccm),
  rdf_retractall(_, _, Point, ccm).

%% simulate(+InitialSpace:space) is det.
% Runs a qualitative simulation on the given initial space.
%
% @param InitialSpace A space.

simulate(InitialSpace):-
  legal_space(InitialSpace),
  simulate([[InitialSpace]], 1000).

%% simulate(+InitialSpace:space, +Depth:number) is det.
% Runs a qualitative simulation on the given initial space.
%
% @param InitialSpace A space.
% @param Depth A number designating the maximum depth to which the
%        simulation takes place.

simulate(_Spaces, 1):-
  !.
simulate([Behavior | Behaviors], MaxLength):-
  MaxLength > 1,
  NewMaxLength is MaxLength - 1,
  legal_transitions(Behavior, NewBehaviors),
  % Remove any doubly visited spaces.
  subtract(NewBehaviors, Behavior, NewBehaviors1),
  append(Behaviors, NewBehaviors1, NewBehaviors2),
  simulate(NewBehaviors2, NewMaxLength).

%% sum(Space, Quantity1, Quantity2, Quantity3) is det.
% Qualitative sum over qualitative values.
%
% @param Space A space.
% @param Quantity1 A quantity.
% @param Quantity2 A quantity.
% @param Quantity3 A quantity.

sum(Space, Quantity1, Quantity2, Quantity3):-
  % The directions of change summate.
  derivative_quantity_value(
    Space,
    Quantity1,
    DerivativeQuantityValue1,
    _DerivativeQuantityValueExpression1
  ),
  derivative_quantity_value(
    Space,
    Quantity2,
    DerivativeQuantityValue2,
    _DerivativeQuantityValueExpression2
  ),
  derivative_quantity_value(
    Space,
    Quantity3,
    DerivativeQuantityValue3,
    _DerivativeQauntityValueExpression3
  ),
  relative_derivative_quantity_value(
    DerivativeQuantityValue1,
    DerivativeSign1
  ),
  relative_derivative_quantity_value(
    DerivativeQuantityValue2,
    DerivativeSign2
  ),
  relative_derivative_quantity_value(
    DerivativeQuantityValue3,
    DerivativeSign3
  ),
  qsum(DerivativeSign1, DerivativeSign2, DerivativeSign3),

  % The magnitude quantity values must be consistent with magnitude summation.
  magnitude_quantity_value(Space, Quantity1, MagnitudeQuantityValue1),
  magnitude_quantity_value(Space, Quantity2, MagnitudeQuantityValue2),
  magnitude_quantity_value(Space, Quantity3, MagnitudeQuantityValue3),
  setoff(
    Sg1/Sg2/Sg3,
    (
    correspond1(
      Quantity1,
      RelativeMagnitudeQuantityValue1,
      Quantity2,
      RelativeMagnitudeQuantityValue2,
      Quantity3,
      RelativeMagnitudeQuantityValue3
    ),
    quantity_magnitude_quantity_value(
      Quantity1,
      RelativeMagnitudeQuantityValue1
    ),
    quantity_magnitude_quantity_value(
      Quantity2,
      RelativeMagnitudeQuantityValue2
    ),
    quantity_magnitude_quantity_value(
      Quantity3,
      RelativeMagnitudeQuantityValue3
    ),
    relative_magnitude_quantity_value(
      MagnitudeQuantityValue1,
      RelativeMagnitudeQuantityValue1,
      Sg1
    ),
    relative_magnitude_quantity_value(
      MagnitudeQuantityValue2,
      RelativeMagnitudeQuantityValue2,
      Sg2
    ),
    relative_magnitude_quantity_value(
      MagnitudeQuantityValue3,
      RelativeMagnitudeQuantityValue3,
      Sg3
    )),
    Signs
  ),
  forall(
    member(Sg1/Sg2/Sg3, Signs),
    (
      \+(qsum(Sg1, Sg2, Sg3))
    ;
      quantity_name(Quantity1, QuantityName1),
      quantity_name(Quantity2, QuantityName2),
      quantity_name(Quantity3, QuantityName3),
      debug(
        ccm_verb,
        'Failed to summate (~w,~w,~w), i.e. (~w,~w,~w).',
        [QuantityName1, QuantityName2, QuantityName3, Sg1, Sg2, Sg3]
      )
    )
  ).

% system_transition(+Triples:list(compound), -Traples:list(compound)) is det.
% A system transition is a transition for all quantity-triples.
%
% @param Triples A list of quantity-triples.
% @param Traples A list of quantity-triples.

system_transition([], []).
system_transition([Triple | Triples], [Traple | Traples]):-
  transition(Triple, Traple),
  system_transition(Triples, Traples).

%% temp_space(?TempSpace:space) is nondet.
% The temporary space.
%
% @param TempSpace A space.

temp_space(TempSpace):-
  rdf_global_id(space:temp, TempSpace).

%% transition(+Triple:list(compound), -Traple:list(compound)) is nondet.
% The lowest level at which transition are generated, i.e. at the level of
% quantities, represented as quantity-triple.
% A quantity-triple contains the following:
%   1. Quantity.
%   2. Magnitude quantity value.
%   3. Derivative quantity value.
%
% @param Triple A quantity-triple.
% @param Traple A quantity-triple.

% L1..L2/std -> L1..L2/[dec,std,inc]
transition(
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue2
):-
  interval_quantity_value(MagnitudeQuantityValue),
  steady_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_derivative_quantity_value(Quantity, DerivativeQuantityValue2).
% L1..L2/inc -> L1..L2/[std,inc]
transition(
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue2
):-
  interval_quantity_value(MagnitudeQuantityValue),
  increasing_derivative_quantity_value(DerivativeQuantityValue1),
  (
    quantity_steady_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ;
    quantity_increasing_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ).
% L1..L2/inc -> L2/[std,inc]
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  interval_quantity_value(MagnitudeQuantityValue1),
  increasing_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue1,
    MagnitudeQuantityValue2
  ),
  \+(highest_magnitude_quantity_value(MagnitudeQuantityValue2)),
  (
    quantity_steady_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ;
    quantity_increasing_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ).
% L1..L2/dec -> L1..L2/[dec,std]
transition(
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue2
):-
  interval_quantity_value(MagnitudeQuantityValue),
  decreasing_derivative_quantity_value(DerivativeQuantityValue1),
  (
    quantity_steady_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ;
    quantity_decreasing_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ).
% L1..L2/dec -> L1/[dec,std]
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  interval_quantity_value(MagnitudeQuantityValue1),
  decreasing_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue2,
    MagnitudeQuantityValue1
  ),
  \+(lowest_magnitude_quantity_value(MagnitudeQuantityValue2)),
  (
    quantity_steady_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ;
    quantity_decreasing_derivative_quantity_value(
      Quantity,
      DerivativeQuantityValue2
    )
  ).
% L1/std -> L1/std
transition(
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue/DerivativeQuantityValue2
):-
  point_quantity_value(MagnitudeQuantityValue),
  steady_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_steady_derivative_quantity_value(
    Quantity,
    DerivativeQuantityValue2
  ).
% L1/std -> L1..L2/inc
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  point_quantity_value(MagnitudeQuantityValue1),
  steady_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue1,
    MagnitudeQuantityValue2
  ),
  quantity_increasing_derivative_quantity_value(
    Quantity,
    DerivativeQuantityValue2
  ).
% L1/std -> L0..L1/dec
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  point_quantity_value(MagnitudeQuantityValue1),
  steady_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue2,
    MagnitudeQuantityValue1
  ),
  quantity_decreasing_derivative_quantity_value(
    Quantity,
    DerivativeQuantityValue2
  ).
% L1/inc -> L1..L2/inc
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  point_quantity_value(MagnitudeQuantityValue1),
  increasing_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue1,
    MagnitudeQuantityValue2
  ),
  quantity_increasing_derivative_quantity_value(
    Quantity,
    DerivativeQuantityValue2
  ).
% L1/dec -> L0..L1/dec
transition(
  Quantity/MagnitudeQuantityValue1/DerivativeQuantityValue1,
  Quantity/MagnitudeQuantityValue2/DerivativeQuantityValue2
):-
  point_quantity_value(MagnitudeQuantityValue1),
  decreasing_derivative_quantity_value(DerivativeQuantityValue1),
  quantity_value_quantity_value_strict(
    MagnitudeQuantityValue2,
    MagnitudeQuantityValue1
  ),
  quantity_decreasing_derivative_quantity_value(
    Quantity,
    DerivativeQuantityValue2
  ).

