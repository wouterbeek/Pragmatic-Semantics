:- module(
  ccm_component,
  [
% WITHIN STATE COMPONENTS
    add_state_components/0,

% STATE TRANSITION COMPONENTS
    add_transition_components/1 % +StateTranstion:state_transition
  ]
).

/** <module> Add components to the CCM.

Adds components to states and state transitions.

@author Wouter Beek
@version 2012/01 - 2012/02
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(generic(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(qr(qr_api)).



% WITHIN STATE COMPONENTS %

add_state_components:-
  add_value_determination_component_type1.
  %%%%add_calculus_components,
  %%%%add_value_determination_component_type2,
  %%%%add_derivative_determination_component_type1,
  %%%%add_derivative_determination_component_type2,

add_calculus_components:-
  findall(
    [InputPoint1, InputPoint2, SupportPoint, OutputPoint],
    add_calculus_components_(
      InputPoint1,
      InputPoint2,
      SupportPoint,
      OutputPoint
    ),
    Triples
  ),
  forall(
    member([InputPoint1, InputPoint2, SupportPoint, OutputPoint], Triples),
    (
      find_or_add_component(
        component:magnitude_calculation,
        [has_magnitude_input1/InputPoint1, has_magnitude_input2/InputPoint2],
        [has_support/SupportPoint],
        [has_magnitude_output/OutputPoint],
        _CalculationComponent,
        _CalculationComponentCloud
      )
    )
  ).

add_calculus_components_(
  InputPoint1,
  InputPoint2,
  SupportPoint,
  OutputPoint
):-
  rdfs_individual_of(Expression, expression:qc_magnitude_equal_to),
  expression_from_argument(Expression, OutputQuantity),
  expression_to_argument(Expression, SupportExpression),
  point(
    SupportExpression,
    InputQuantity1,
    _Relation,
    InputQuantity2,
    _State,
    SupportPoint
  ),
  point(
    _MagnitudeExpression1,
    InputQuantity1,
    expression:qp_magnitude_equal_to,
    _ToArgument1,
    State,
    InputPoint1
  ),
  point(
    _MagnitudeExpression2,
    InputQuantity2,
    expression:qp_magnitude_equal_to,
    _ToArgument2,
    State,
    InputPoint2
  ),
  point(
    _MagnitudeExpression3,
    OutputQuantity,
    expression:qp_magnitude_equal_to,
    _ToArgument3,
    State,
    OutputPoint
  ).

add_value_determination_component_type1:-
  findall(
    [InputPoint, SupportPoint, OutputPoint],
    add_value_determination_component_type1_(
      InputPoint,
      SupportPoint,
      OutputPoint
    ),
    Triples
  ),
  forall(
    member([InputPoint, SupportPoint, OutputPoint], Triples),
    find_or_add_component(
      component:value_determination_type1,
      [has_magnitude_inequality_input/InputPoint],
      [has_support/SupportPoint],
      [has_magnitude_inequality_output/OutputPoint],
      _ValueDeterminationComponent,
      _ValueDeterminationComponentCloud
    )
  ).

add_value_determination_component_type1_(
  InputPoint,
  SupportPoint,
  OutputPoint
):-
  % Support point
  point(
    _SupportExpression,
    OutputQuantity,
    expression:qc_magnitude_equal_to,
    CaculationExpression,
    _Space,
    SupportPoint
  ),
  expression(
    CaculationExpression,
    InputQuantity1,
    _Relation,
    InputQuantity2
  ),

  % Input point
  point(InputExpression, State, InputPoint),
  qq_magnitude_inequality_expression(InputExpression),
  (
    expression_from_argument(InputExpression, InputQuantity1),
    expression_to_argument(InputExpression, InputQuantity2)
  ->
    X = normal
  ;
    expression_from_argument(InputExpression, InputQuantity2),
    expression_to_argument(InputExpression, InputQuantity1)
  ->
    X = inverted
  ),

  % Output point
  point(
    _OutputExpression,
    OutputQuantity,
    expression:qp_magnitude_equal_to,
    MagnitudeValue,
    State,
    OutputPoint
  ),

  % Extra constraints
  (
    X == normal
  ->
    (
      smaller_than(InputExpression),
      negative_magnitude_quantity_value(MagnitudeValue)
    ;
      equal_to_expression(InputExpression),
      zero_magnitude_quantity_value(MagnitudeValue)
    ;
      greater_than_expression(InputExpression),
      positive_magnitude_quantity_value(MagnitudeValue)
    )
  ;
    X == inverted
  ->
    (
      greater_than_expression(InputExpression),
      negative_magnitude_quantity_value(MagnitudeValue)
    ;
      equal_to_expression(InputExpression),
      zero_magnitude_quantity_value(MagnitudeValue)
    ;
      smaller_than(InputExpression),
      positive_magnitude_quantity_value(MagnitudeValue)
    )
  ).

add_value_determination_component_type2:-
  findall(
    [InputPoint1, InputPoint2, SupportPoint, OutputPoint],
    (
      % Support point
      % We begin with this since there are relatively few calculi.
      point(SupportExpression, SupportPoint),
      rdfs_individual_of(SupportExpression, expression:magnitude_calculus),
      expression_from_argument(SupportExpression, FromQuantity1),
      expression_to_from_argument2(SupportExpression, FromQuantity2),
      expression_to_argument(SupportExpression, ToQuantity),

      % Input point 1.
      point(InputExpression1, State, InputPoint1),
      to_quantity_expression(FromQuantity1, InputExpression1),

      % Input point 2.
      point(InputExpression2, State, InputPoint2),
      to_quantity_expression(FromQuantity2, InputExpression2),

      % Output point
      point(OutputExpression, State, OutputPoint),
      to_quantity_expression(ToQuantity, OutputExpression),

      % Extra constraints
      (
        rdfs_individual_of(
          SupportExpression,
          expression:negative_magnitude_calculus
        )
      ->
        (
          negative_magnitude_expression(InputExpression1),
          positive_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          negative_magnitude_expression(InputExpression1),
          zero_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          zero_magnitude_expression(InputExpression1),
          positive_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          zero_magnitude_expression(InputExpression1),
          zero_magnitude_expression(InputExpression2),
          zero_magnitude_expression(OutputExpression)
        )
      ;
        rdfs_individual_of(
          SupportExpression,
          expression:positive_magnitude_calculus
        )
      ->
        (
          negative_magnitude_expression(InputExpression1),
          negative_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          negative_magnitude_expression(InputExpression1),
          zero_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          zero_magnitude_expression(InputExpression1),
          negative_magnitude_expression(InputExpression2),
          negative_magnitude_expression(OutputExpression)
        ;
          zero_magnitude_expression(InputExpression1),
          zero_magnitude_expression(InputExpression2),
          zero_magnitude_expression(OutputExpression)
        ;
          positive_magnitude_expression(InputExpression1),
          positive_magnitude_expression(InputExpression2),
          positive_magnitude_expression(OutputExpression)
        ;
          positive_magnitude_expression(InputExpression1),
          zero_magnitude_expression(InputExpression2),
          positive_magnitude_expression(OutputExpression)
        ;
          zero_magnitude_expression(InputExpression1),
          positive_magnitude_expression(InputExpression2),
          positive_magnitude_expression(OutputExpression)
        )
      )
    ),
    Triples
  ),
  forall(
    member(
      [InputPoint1, InputPoint2, SupportPoint, OutputPoint],
      Triples
    ),
    find_or_add_component(
      component:value_determination_type2,
      [has_input/InputPoint1, has_input/InputPoint2],
      [has_support/SupportPoint],
      [has_output/OutputPoint],
      _ValueDeterminationComponent,
      _ValueDeterminationComponentCloud
    )
  ).

add_derivative_determination_component_type1:-
  findall(
    [InputPoint, SupportPoint, OutputPoint],
    (
      % Support point
      % We begin with this since there are relatively few calculi.
      point(SupportExpression, SupportPoint),
      rdfs_individual_of(
        SupportExpression,
        expression:negative_derivative_calculus
      ),
      expression_from_argument(SupportExpression, FromQuantity1),
      expression_to_from_argument2(SupportExpression, FromQuantity2),
      expression_to_argument(SupportExpression, ToQuantity),

      % Input point
      point(InputExpression, State, InputPoint),
      rdfs_individual_of(InputExpression, expression:qq_derivative_inequality),
      expression_from_argument(InputExpression, FromQuantity1),
      expression_to_argument(InputExpression, FromQuantity2),

      % Output point
      point(OutputExpression, State, OutputPoint),
      to_quantity_expression(ToQuantity, OutputExpression),

      % Extra constraints
      (
        rdfs_individual_of(
          InputExpression,
          expression:qp_magnitude_smaller_than
        )
      ->
        decreasing_derivative_expression(OutputExpression)
      ;
        rdfs_individual_of(
          InputExpression,
          expression:qp_magnitude_equal_to
        )
      ->
        steady_derivative_expression(OutputExpression)
      ;
        rdfs_individual_of(
          InputExpression,
          expression:qp_magnitude_greater_than
        )
      ->
        increasing_derivative_expression(OutputExpression)
      )
    ),
    Triples
  ),
  forall(
    member(
      [InputPoint, SupportPoint, OutputPoint],
      Triples
    ),
    find_or_add_component(
      component:derivative_determination_type1,
      [has_magnitude_inequality_input/InputPoint],
      [has_support/SupportPoint],
      [has_derivative_inequality_output/OutputPoint],
      _DerivativeDeterminationComponent,
      _DerivativeDeterminationComponentCloud
    )
  ).

add_derivative_determination_component_type2:-
  findall(
    [InputPoint1, InputPoint2, SupportPoint, OutputPoint],
    (
      % Support point
      % We begin with this since there are relatively few calculi.
      point(SupportExpression, SupportPoint),
      rdfs_individual_of(SupportExpression, expression:derivative_calculus),
      expression_from_argument(SupportExpression, FromQuantity1),
      expression_to_from_argument2(SupportExpression, FromQuantity2),
      expression_to_argument(SupportExpression, ToQuantity),

      % Input point 1
      point(InputExpression1, State, InputPoint1),
      to_quantity_expression(FromQuantity1, InputExpression1),

      % Input point 2
      point(InputExpression2, State, InputPoint2),
      to_quantity_expression(FromQuantity2, InputExpression2),

      % Output point
      point(OutputExpression, State, OutputPoint),
      to_quantity_expression(ToQuantity, OutputExpression),

      % Extra constraints
      (
        rdfs_individual_of(
          SupportExpression,
          expression:negative_derivative_calculus
        )
      ->
        (
          decreasing_derivative_expression(InputExpression1),
          increasing_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          decreasing_derivative_expression(InputExpression1),
          steady_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          steady_derivative_expression(InputExpression1),
          increasing_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          steady_derivative_expression(InputExpression1),
          steady_derivative_expression(InputExpression2),
          steady_derivative_expression(OutputExpression)
        )
      ;
        rdfs_individual_of(
          SupportExpression,
          expression:positive_derivative_calculus
        )
      ->
        (
          decreasing_derivative_expression(InputExpression1),
          decreasing_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          decreasing_derivative_expression(InputExpression1),
          steady_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          steady_derivative_expression(InputExpression1),
          decreasing_derivative_expression(InputExpression2),
          decreasing_derivative_expression(OutputExpression)
        ;
          steady_derivative_expression(InputExpression1),
          steady_derivative_expression(InputExpression2),
          steady_derivative_expression(OutputExpression)
        ;
          increasing_derivative_expression(InputExpression1),
          increasing_derivative_expression(InputExpression2),
          increasing_derivative_expression(OutputExpression)
        ;
          increasing_derivative_expression(InputExpression1),
          steady_derivative_expression(InputExpression2),
          increasing_derivative_expression(OutputExpression)
        ;
          steady_derivative_expression(InputExpression1),
          increasing_derivative_expression(InputExpression2),
          increasing_derivative_expression(OutputExpression)
        )
      )
    ),
    Triples
  ),
  forall(
    member([InputPoint1, InputPoint2, SupportPoint, OutputPoint], Triples),
    find_or_add_component(
      component:derivative_determination_type2,
      [has_input/InputPoint1, has_input/InputPoint2],
      [has_support/SupportPoint],
      [has_output/OutputPoint],
      _DerivativeDeterminationComponent,
      _DerivativeDeterminationComponentCloud
    )
  ).



% STATE TRANSITION COMPONENTS %

add_transition_components(StateTransition):-
  state_transition_to_state(StateTransition, ToState),
  add_inequality_termination_component_type1(ToState),
  add_inequality_termination_component_type2,
  add_inequality_continuity_component,
  add_magnitude_transition_components,
  add_derivative_continuity_components.

add_inequality_termination_component_type1(ToState):-
  findall(
    [
      FromMagnitudeInequalityPoint,
      FromDerivativeInequalityPoint,
      ToMagnitudeInequalityPoint
    ],
    (
      % Input 1
      point(
        FromMagnitudeInequalityExpression,
        FromState,
        FromMagnitudeInequalityPoint
      ),
      inequality_expression(FromMagnitudeInequalityExpression),
      rdfs_individual_of(FromMagnitudeInequalityExpression, expression:magnitude),
      expression_from_argument(FromMagnitudeInequalityExpression, Quantity1),
      expression_to_argument(FromMagnitudeInequalityExpression, Quantity2),

      % Input 2
      point(
        FromDerivativeInequalityExpression,
        FromState,
        FromDerivativeInequalityPoint
      ),
      inequality_expression(FromDerivativeInequalityExpression),
      rdfs_individual_of(FromDerivativeInequalityExpression, expression:derivative),
      expression_from_argument(FromDerivativeInequalityExpression, Quantity1),
      expression_to_argument(FromDerivativeInequalityExpression, Quantity2),

      % Output
      (
        rdfs_individual_of(FromMagnitudeInequalityExpression, expression:qq_magnitude_equal_to),
        rdfs_individual_of(FromDerivativeInequalityExpression, expression:qq_derivative_smaller_than)
      ->
        rdf_global_id(expression:magnitude_smaller_than, ExpressionDefinition)
      ;
        rdfs_individual_of(FromMagnitudeInequalityExpression, expression:qq_magnitude_greater_than),
        rdfs_individual_of(FromDerivativeInequalityExpression, expression:qq_derivative_smaller_than)
      ->
        rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition)
      ;
        rdfs_individual_of(FromMagnitudeInequalityExpression, expression:qq_magnitude_smaller_than),
        rdfs_individual_of(FromDerivativeInequalityExpression, expression:qq_derivative_greater_than)
      ->
        rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition)
      ;
        rdfs_individual_of(FromMagnitudeInequalityExpression, expression:qq_magnitude_equal_to),
        rdfs_individual_of(FromDerivativeInequalityExpression, expression:qq_derivative_greater_than)
      ->
        rdf_global_id(expression:magnitude_greater_than, ExpressionDefinition)
      ),
      expression(
        Quantity1,
        ExpressionDefinition,
        Quantity2,
        ToMagnitudeInequalityExpression
      ),
      point(
        ToMagnitudeInequalityExpression,
        ToState,
        ToMagnitudeInequalityPoint
      ),
      state_transition(FromState, ToState)
    ),
    Triples
  ),
  forall(
    member(
      [
        FromMagnitudeInequalityPoint,
        FromDerivativeInequalityPoint,
        ToMagnitudeInequalityPoint
      ],
      Triples
    ),
    find_or_add_component(
      component:inequality_termination_type1,
      [
        has_magnitude_inequality_input/FromMagnitudeInequalityPoint,
        has_derivative_inequality_input/FromDerivativeInequalityPoint
      ],
      [],
      [has_magnitude_inequality_output/ToMagnitudeInequalityPoint],
      _InequalityTerminationComponent,
      _InequalityTerminationComponentCloud
    )
  ).

add_inequality_termination_component_type2:-
  findall(
    [InputPoint1, InputPoint2, InputPoint3, OutputPoint],
    add_inequality_termination_component_type2_tuple(
      InputPoint1,
      InputPoint2,
      InputPoint3,
      OutputPoint
    ),
    Triples
  ),
  forall(
    member([InputPoint1, InputPoint2, InputPoint3, OutputPoint], Triples),
    find_or_add_component(
      component:inequality_termination_type2,
      [
        has_magnitude_inequality_input/InputPoint1,
        has_derivative_inequality_input1/InputPoint2,
        has_derivative_inequality_input2/InputPoint3
      ],
      [],
      [has_magnitude_inequality_output/OutputPoint],
      _Component,
      _ComponentCloud
    )
  ).

add_inequality_termination_component_type2_tuple(
  InputPoint1,
  InputPoint2,
  InputPoint3,
  OutputPoint
):-
  % Input 1
  point(InputExpression1, FromState, InputPoint1),
  state(FromState),
  inequality_expression(InputExpression1),
  rdfs_individual_of(InputExpression1, expression:qq),
  rdfs_individual_of(InputExpression1, expression:magnitude),
  expression_from_argument(InputExpression1, Quantity1),
  expression_to_argument(InputExpression1, Quantity2),

  % Input 2
  point(InputExpression2, FromState, InputPoint2),
  to_quantity_expression(Quantity1, InputExpression2),

  % Input 3
  point(InputExpression3, FromState, InputPoint3),
  to_quantity_expression(Quantity2, InputExpression3),

  % Output
  point(OutputExpression, ToState, OutputPoint),
  state_transition(FromState, ToState),
  expression_from_argument(OutputExpression, Quantity1),
  expression_to_argument(OutputExpression, Quantity2),

  % Restrictions
  (
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    decreasing_derivative_expression(InputExpression2),
    increasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_greater_than),
    decreasing_derivative_expression(InputExpression2),
    increasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    steady_derivative_expression(InputExpression2),
    increasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_greater_than),
    steady_derivative_expression(InputExpression2),
    increasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    decreasing_derivative_expression(InputExpression2),
    steady_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_greater_than),
    decreasing_derivative_expression(InputExpression2),
    steady_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_smaller_than),
    increasing_derivative_expression(InputExpression2),
    decreasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    increasing_derivative_expression(InputExpression2),
    decreasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_smaller_than),
    steady_derivative_expression(InputExpression2),
    decreasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    steady_derivative_expression(InputExpression2),
    decreasing_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_smaller_than),
    increasing_derivative_expression(InputExpression2),
    steady_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ;
    rdfs_individual_of(InputExpression1, expression:qq_magnitude_equal_to),
    increasing_derivative_expression(InputExpression2),
    steady_derivative_expression(InputExpression3),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ).

add_inequality_continuity_component:-
  findall(
    [FromInequalityPoint, ToInequalityPoint],
    (
      point(InequalityExpression, FromState, FromInequalityPoint),
      state(FromState),
      inequality_expression(InequalityExpression),
      rdfs_individual_of(InequalityExpression, expression:qq),
      point(InequalityExpression, ToState, ToInequalityPoint),
      state_transition(FromState, ToState)
    ),
    Tuples
  ),
  forall(
    member([FromInequalityPointCloud, ToInequalityPointCloud], Tuples),
    find_or_add_component(
      component:inequality_continuity,
      [has_inequality_input/FromInequalityPointCloud],
      [],
      [has_inequality_output/ToInequalityPointCloud],
      _InequalityContinuityComponent,
      _InequalityContinuityComponentCloud
    )
  ).

add_magnitude_transition_components:-
  setoff(
    [
      FromMagnitudePoint/FromMagnitude,
      FromDerivativePoint/FromDerivative,
      ToMagnitudePoint/ToMagnitude
    ],
    add_magnitude_transition_components_(
      FromMagnitudePoint/FromMagnitude,
      FromDerivativePoint/FromDerivative,
      ToMagnitudePoint/ToMagnitude
    ),
    Triples
  ),
  forall(
    member(
      [
        FromMagnitudePoint/FromMagnitude,
        FromDerivativePoint/FromDerivative,
        ToMagnitudePoint/ToMagnitude
      ],
      Triples
    ),
    (
      (
        FromMagnitude == ToMagnitude
      ->
        rdf_global_id(component:magnitude_continuity, Type)
      ;
        rdf_global_id(component:magnitude_termination, Type)
      ),
      find_or_add_component(
        Type,
        [
          has_magnitude_input/FromMagnitudePoint,
          has_derivative_input/FromDerivativePoint
        ],
        [],
        [has_magnitude_output/ToMagnitudePoint],
        _Component,
        _ComponentCloud
      )
    )
  ).

add_magnitude_transition_components_(
  FromMagnitudePoint/FromMagnitude,
  FromDerivativePoint/FromDerivative,
  ToMagnitudePoint/ToMagnitude
):-
  % Magnitude input
  magnitude_quantity_value_expression(FromMagnitudeExpression),
  point(FromMagnitudeExpression, FromState, FromMagnitudePoint),
  state(FromState),
  expression_from_argument(FromMagnitudeExpression, Quantity),
  expression_to_argument(FromMagnitudeExpression, FromMagnitude),

  % Derivative input
  derivative_quantity_value_expression(FromDerivativeExpression),
  point(FromDerivativeExpression, FromState, FromDerivativePoint),
  expression_from_argument(FromDerivativeExpression, Quantity),
  expression_to_argument(FromDerivativeExpression, FromDerivative),

  % Magnitude Output
  magnitude_quantity_value_expression(ToMagnitudeExpression),
  point(ToMagnitudeExpression, ToState, ToMagnitudePoint),
  state_transition(FromState, ToState),
  expression_from_argument(ToMagnitudeExpression, Quantity),
  expression_to_argument(ToMagnitudeExpression, ToMagnitude).

add_derivative_continuity_components:-
  findall(
    [FromDerivativePoint, ToDerivativePoint],
    (
      lonely_point(ToDerivativePoint),
      point(
        DerivativeExpression,
        ToState,
        ToDerivativePoint
      ),
      derivative_quantity_value_expression(DerivativeExpression),
      state_transition(FromState, ToState),
      point(DerivativeExpression, FromState, FromDerivativePoint)
    ),
    Pairs
  ),
  forall(
    member([FromDerivativePoint, ToDerivativePoint], Pairs),
    find_or_add_component(
      component:derivative_continuity,
      [has_derivative_input/FromDerivativePoint],
      [],
      [has_derivative_output/ToDerivativePoint],
      _DerivativeContinuityComponent,
      _DerivativeContinuityComponentCloud
    )
  ).

expression_to_from_argument2(Expression, FromArgument2):-
  rdf(Expression, expression:has_from_argument2, FromArgument2, ccm),
  !.
