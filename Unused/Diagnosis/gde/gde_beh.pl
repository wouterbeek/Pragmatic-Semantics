:- module(
  gde_beh,
  [
    calculate_derivative_continuity/2, % ?FromDerivative:expression
                                       % ?ToDerivative:expression
    calculate_magnitude_continuity/3, % ?FromMagnitude:expression
                                      % ?FromDerivative:expression
                                      % ?ToMagnitude:expression
    calculate_magnitude_termination/3, % ?FromMagnitude:expression
                                       % ?FromDerivative:expression
                                       % ?ToMagnitude:expression
    calculate_quantity_proportionality/3, % ?FromDerivative:expression
                                          % ?Support:expression,
                                          % ?ToDerivative:expression
    calculate_retrieval/2, % ?Input:expression
                           % ?Output:expression
    calculate_scenario_value/2 % ?Input:expression
                               % ?Output:expression
  ]
).

/** <module> Behavior rules for GDE.

Behavior .

@author Wouter Beek
@version 2011/08-2012/02
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(qr(qr_api)).
:- use_module(library(semweb/rdf_db)).



% DERIVATIVE CONTINUITY %

calculate_derivative_continuity(DerivativeExpression, DerivativeExpression).



% INEQUALITY CONTINUITY %

calculate_inequality_continuity(InequalityExpression, InequalityExpression).

calculate_inequality_continuity(
  InequalityExpression,
  inequality_continuity,
  InequalityExpression
).



% INEQUALITY TERMINATION - TYPE 1 %

calculate_inequality_termination_type1(
  FromMagnitudeInequality,
  FromDerivativeInequality,
  ToMagnitudeInequality
):-
  var(FromMagnitudeInequality),
  !,
  expression_from_argument(ToMagnitudeInequality, Quantity1),
  expression_to_argument(ToMagnitudeInequality, Quantity2),
  (
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_smaller_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_smaller_than)
  ->
    rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_smaller_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:magnitude_greater_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_greater_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:magnitude_smaller_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_greater_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_greater_than)
  ->
    rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition1)
  ;
    % Not something we can draw a component for.
    fail
  ),
  !,
  find_or_add_expression(
    Quantity1,
    ExpressionDefinition1,
    Quantity2,
    FromMagnitudeInequality
  ).

calculate_inequality_termination_type1(
  FromMagnitudeInequality,
  FromDerivativeInequality,
  ToMagnitudeInequality
):-
  var(FromDerivativeInequality),
  !,
  expression_from_argument(FromMagnitudeInequality, Quantity1),
  expression_to_argument(FromMagnitudeInequality, Quantity2),
  (
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_smaller_than)
  ->
    rdf_global_id(expression:derivative_smaller_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_greater_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:derivative_smaller_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_smaller_than),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:derivative_greater_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    rdfs_individual_of(ToMagnitudeInequality, expression:qq_derivative_greater_than)
  ->
    rdf_global_id(expression:derivative_greater_than, ExpressionDefinition1)
  ;
    % Not something we can draw a component for.
    fail
  ),
  !,
  find_or_add_expression(
    Quantity1,
    ExpressionDefinition1,
    Quantity2,
    FromDerivativeInequality
  ).

calculate_inequality_termination_type1(
  FromMagnitudeInequality,
  FromDerivativeInequality,
  ToMagnitudeInequality
):-
  var(ToMagnitudeInequality),
  !,
  expression_from_argument(FromMagnitudeInequality, Quantity1),
  expression_to_argument(FromMagnitudeInequality, Quantity2),
  (
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_smaller_than)
  ->
    rdf_global_id(expression:magnitude_smaller_than, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_greater_than),
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_smaller_than)
  ->
    rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_smaller_than),
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_greater_than)
  ->
    rdf_global_id(expression:magnitude_equal_to, ExpressionDefinition1)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    rdfs_individual_of(FromDerivativeInequality, expression:qq_derivative_greater_than)
  ->
    rdf_global_id(expression:magnitude_greater_than, ExpressionDefinition1)
  ;
    % Not something we can draw a component for.
    fail
  ),
  !,
  find_or_add_expression(
    Quantity1,
    ExpressionDefinition1,
    Quantity2,
    ToMagnitudeInequality
  ).


% INEQUALITY TERMINATION - TYPE 2 %

calculate_inequality_termination_type2(
  FromMagnitudeInequality,
  FromDerivative1,
  FromDerivative2,
  ToMagnitudeInequality
):-
  var(ToMagnitudeInequality),
  !,
  (
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    decreasing_derivative_expression(FromDerivative1),
    increasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_greater_than),
    decreasing_derivative_expression(FromDerivative1),
    increasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    steady_derivative_quantity_value(FromDerivative1),
    increasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_greater_than),
    steady_derivative_quantity_value(FromDerivative1),
    increasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    decreasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_greater_than),
    decreasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_smaller_than),
    increasing_derivative_expression(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    increasing_derivative_expression(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_smaller_than),
    steady_derivative_quantity_value(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    steady_derivative_quantity_value(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_smaller_than),
    increasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, ToMagnitudeInequalityDefinition)
  ;
    rdfs_individual_of(FromMagnitudeInequality, expression:qq_magnitude_equal_to),
    increasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, ToMagnitudeInequalityDefinition)
  ;
    % This should never be the case.
    fail
  ),
  expression_from_argument(FromMagnitudeInequality, FromQuantity),
  expression_to_argument(FromMagnitudeInequality, ToQuantity),
  !,
  find_or_add_expression(
    FromQuantity,
    ToMagnitudeInequalityDefinition,
    ToQuantity,
    ToMagnitudeInequality
  ).

calculate_inequality_termination_type2(
  FromMagnitudeInequality,
  FromDerivative1,
  FromDerivative2,
  ToMagnitudeInequality
):-
  var(FromMagnitudeInequality),
  !,
  (
    decreasing_derivative_expression(FromDerivative1),
    increasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    decreasing_derivative_expression(FromDerivative1),
    increasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, FromMagnitudeInequalityDefinition)
  ;
    steady_derivative_quantity_value(FromDerivative1),
    increasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    steady_derivative_quantity_value(FromDerivative1),
    increasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, FromMagnitudeInequalityDefinition)
  ;
    decreasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_smaller_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    decreasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_greater_than, FromMagnitudeInequalityDefinition)
  ;
    increasing_derivative_expression(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, FromMagnitudeInequalityDefinition)
  ;
    increasing_derivative_expression(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    steady_derivative_quantity_value(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, FromMagnitudeInequalityDefinition)
  ;
    steady_derivative_quantity_value(FromDerivative1),
    decreasing_derivative_expression(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    increasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_equal_to)
  ->
    rdf_global_id(expression:qq_magnitude_smaller_than, FromMagnitudeInequalityDefinition)
  ;
    increasing_derivative_expression(FromDerivative1),
    steady_derivative_quantity_value(FromDerivative2),
    rdfs_individual_of(OutputExpression, expression:qq_magnitude_greater_than)
  ->
    rdf_global_id(expression:qq_magnitude_equal_to, FromMagnitudeInequalityDefinition)
  ;
    % This should never be the case.
    fail
  ),
  expression_from_argument(ToMagnitudeInequality, FromQuantity),
  expression_to_argument(ToMagnitudeInequality, ToQuantity),
  !,
  find_or_add_expression(
    FromQuantity,
    FromMagnitudeInequalityDefinition,
    ToQuantity,
    FromMagnitudeInequality
  ),
  !.



% MAGNITUDE CONTINUITY %

calculate_magnitude_continuity(
  MagnitudeExpression,
  FromDerivativeExpression,
  MagnitudeExpression
):-
  var(FromDerivativeExpression),
  !,
  from_quantity_expression(Quantity, MagnitudeExpression),
  quantity_steady_derivative_quantity_value(Quantity, FromDerivativeValue),
  find_or_add_expression(
    Quantity,
    expression:qp_derivative_equal_to,
    FromDerivativeValue,
    FromDerivativeExpression
  ).
calculate_magnitude_continuity(
  FromMagnitudeExpression,
  _FromDerivativeExpression,
  ToMagnitudeExpression
):-
  var(FromMagnitudeExpression),
  !,
  FromMagnitudeExpression = ToMagnitudeExpression.
calculate_magnitude_continuity(
  FromMagnitudeExpression,
  _FromDerivativeExpression,
  ToMagnitudeExpression
):-
  var(ToMagnitudeExpression),
  !,
  ToMagnitudeExpression = FromMagnitudeExpression.



% MAGNITUDE TERMINATIONS %

calculate_magnitude_termination(
  FromMagnitudeExpression,
  FromDerivativeExpression,
  ToMagnitudeExpression
):-
  var(FromMagnitudeExpression),
  !,
  expression_from_argument(ToMagnitudeExpression, Quantity),
  expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
  (
    increasing_derivative_quantity_value(FromDerivativeValue)
  ->
    quantity_value_quantity_value(FromMagnitudeValue, ToMagnitudeValue)
  ;
    decreasing_derivative_quantity_value(FromDerivativeValue)
  ->
    quantity_value_quantity_value(ToMagnitudeValue, FromMagnitudeValue)
  ;
    % This should never be the case.
    fail
  ),
  \+(FromMagnitudeValue == ToMagnitudeValue),
  !,
  find_or_add_expression(
    Quantity,
    expression:qp_magnitude_equal_to,
    FromMagnitudeValue,
    FromMagnitudeExpression
  ).
calculate_magnitude_termination(
  FromMagnitudeExpression,
  FromDerivativeExpression,
  ToMagnitudeExpression
):-
  var(ToMagnitudeExpression),
  !,
  expression_from_argument(FromMagnitudeExpression, Quantity),
  expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
  expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
  (
    increasing_derivative_quantity_value(FromDerivativeValue)
  ->
    quantity_value_quantity_value(FromMagnitudeValue, ToMagnitudeValue)
  ;
    decreasing_derivative_quantity_value(FromDerivativeValue)
  ->
    quantity_value_quantity_value(ToMagnitudeValue, FromMagnitudeValue)
  ;
    % This should never be the case.
    fail
  ),
  \+(FromMagnitudeValue == ToMagnitudeValue),
  !,
  find_or_add_expression(
    Quantity,
    expression:qp_magnitude_equal_to,
    ToMagnitudeValue,
    ToMagnitudeExpression
  ).
calculate_magnitude_termination(
  FromMagnitudeExpression,
  FromDerivativeExpression,
  ToMagnitudeExpression
):-
  var(FromDerivativeExpression),
  expression_from_argument(FromMagnitudeExpression, Quantity),
  expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
  expression_to_argument(ToMagnitudeExpression, ToMagnitudeValue),
  (
    FromMagnitudeValue == ToMagnitudeValue
  ->
    decreasing_derivative_quantity_value(FromDerivativeValue)
  ;
    quantity_value_quantity_value(FromMagnitudeValue, ToMagnitudeValue)
  ->
    increasing_derivative_quantity_value(FromDerivativeValue)
  ;
    % This should never be the case.
    fail
  ),
  !,
  find_or_add_expression(
    Quantity,
    expression:qp_derivative_equal_to,
    FromDerivativeValue,
    FromDerivativeExpression
  ).



% QUANTITY SPACE CORRESPONDENCES %

calculate_correspondence(
  FromQuantityValueExpression,
  SupportExpression,
  ToQuantityValueExpression
):-
  nonvar(SupportExpression),
  !,
  from_quantity_expression(FromQuantity, SupportExpression),
  to_quantity_expression(ToQuantity, SupportExpression),
  calculate_correspondence(
    FromQuantityValueExpression,
    FromQuantity,
    SupportExpression,
    ToQuantityValueExpression,
    ToQuantity
  ).

calculate_correspondence(
  FromQuantityValueExpression,
  FromQuantity,
  SupportExpression,
  ToQuantityValueExpression,
  ToQuantity
):-
  var(ToQuantityValueExpression),
  !,
  calculate_correspondence1(
    FromQuantityValueExpression,
    FromQuantity,
    SupportExpression,
    ToQuantityValueExpression,
    ToQuantity
  ).
calculate_correspondence(
  FromQuantityValueExpression,
  FromQuantity,
  SupportExpression,
  ToQuantityValueExpression,
  ToQuantity
):-
  var(FromQuantityValueExpression),
  !,
  calculate_correspondence1(
    ToQuantityValueExpression,
    ToQuantity,
    SupportExpression,
    FromQuantityValueExpression,
    FromQuantity
  ).

calculate_correspondence1(
  FromQuantityValueExpression,
  FromQuantity,
  SupportExpression,
  ToQuantityValueExpression,
  ToQuantity
):-
  (
    rdfs_individual_of(SupportExpression, expression:magnitude)
  ->
    quantity_magnitude_quantity_space(FromQuantity, FromQuantitySpace),
    quantity_magnitude_quantity_space(ToQuantity, ToQuantitySpace),
    rdf_global_id(
      expression:directed_magnitude_quantity_value_correspondence,
      ToExpressionDefinition
    )
  ;
    rdfs_individual_of(SupportExpression, expression:derivative)
  ->
    quantity_derivative_quantity_space(FromQuantity, FromQuantitySpace),
    quantity_derivative_quantity_space(ToQuantity, ToQuantitySpace),
    rdf_global_id(
      expression:directed_derivative_quantity_value_correspondence,
      ToExpressionDefinition
    )
  ),
  quantity_space_to_cardinality(FromQuantitySpace, Cardinality),
  quantity_space_to_cardinality(ToQuantitySpace, Cardinality),
  expression_to_argument(FromQuantityValueExpression, FromQuantityValue),
  quantity_space_quantity_value_index(
    FromQuantitySpace,
    FromIndex,
    FromQuantityValue
  ),
  (
    inverted_expression(SupportExpression)
  ->
    ToIndex is Cardinality - FromIndex
  ;
    ToIndex = FromIndex
  ),
  quantity_space_quantity_value_index(
    ToQuantitySpace,
    ToIndex,
    ToQuantityValue
  ),
  find_or_add_expression(
    ToQuantity,
    ToExpressionDefinition,
    ToQuantityValue,
    ToQuantityValueExpression
  ).

/*
% Derivative quantity space correspondences (directional/bidirectional,
% inverted/uninverted).
calculate_correspondence(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  nonvar(SupportExpression),
  derivative_quantity_space_correspondence_expression(SupportExpression),
  (
    uninverted_expression(SupportExpression)
  ->
    (
      var(FromDerivativeExpression)
    ->
      expression_from_argument(SupportExpression, FromQuantity),
      expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
      (
        increasing_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_increasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ;
        steady_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_steady_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ;
        decreasing_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_decreasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ),
      !,
      find_or_add_expression(
        FromQuantity,
        expression:qp_derivative_equal_to,
        FromDerivativeValue,
        FromDerivativeExpression
      )
    ;
      var(ToDerivativeExpression)
    ->
      expression_to_argument(SupportExpression, ToQuantity),
      expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
      (
        increasing_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ;
        steady_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ;
        decreasing_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ),
      !,
      find_or_add_expression(
        ToQuantity,
        expression:qp_derivative_equal_to,
        ToDerivativeValue,
        ToDerivativeExpression
      )
    )
  ;
    inverted_expression(SupportExpression)
  ->
    (
      var(FromDerivativeExpression)
    ->
      expression_from_argument(SupportExpression, FromQuantity),
      expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
      (
        increasing_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_decreasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ;
        steady_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_steady_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ;
        decreasing_derivative_quantity_value(ToDerivativeValue)
      ->
        quantity_increasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
      ),
      !,
      find_or_add_expression(
        FromQuantity,
        expression:qp_derivative_equal_to,
        FromDerivativeValue,
        FromDerivativeExpression
      )
    ;
      var(ToDerivativeExpression)
    ->
      expression_to_argument(SupportExpression, ToQuantity),
      expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
      (
        increasing_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ;
        steady_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ;
        decreasing_derivative_quantity_value(FromDerivativeValue)
      ->
        quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
      ),
      !,
      find_or_add_expression(
        ToQuantity,
        expression:qp_derivative_equal_to,
        ToDerivativeValue,
        ToDerivativeExpression
      )
    )
  ).
% Magnitude quantity space correspondences (directional/bidirectional,
% inverted/uninverted).
calculate_correspondence(
  FromMagnitudeExpression,
  SupportExpression,
  ToMagnitudeExpression
):-
  nonvar(SupportExpression),
  magnitude_quantity_space_correspondence_expression(SupportExpression),
  (
    uninverted_expression(SupportExpression)
  ->
    (
      var(FromMagnitudeExpression)
    ->
      expression_to_argument(ToMagnitudeExpression, ToMagnitudeValue),
      quantity_value_index(ToMagnitudeValue, MagnitudeIndex),
      expression_from_argument(SupportExpression, FromQuantity),
      quantity_magnitude_quantity_space(FromQuantity, FromQuantitySpace),
      quantity_space_quantity_value_index(
        FromQuantitySpace,
        MagnitudeIndex,
        FromMagnitudeValue
      ),
      !,
      find_or_add_expression(
        FromQuantity,
        expression:qp_magnitude_equal_to,
        FromMagnitudeValue,
        FromMagnitudeExpression
      )
    ;
      var(ToMagnitudeExpression)
    ->
      expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
      quantity_value_index(FromMagnitudeValue, MagnitudeIndex),
      expression_to_argument(SupportExpression, ToQuantity),
      quantity_magnitude_quantity_space(ToQuantity, ToQuantitySpace),
      quantity_space_quantity_value_index(
        ToQuantitySpace,
        MagnitudeIndex,
        ToMagnitudeValue
      ),
      !,
      find_or_add_expression(
        ToQuantity,
        expression:qp_magnitude_equal_to,
        ToMagnitudeValue,
        ToMagnitudeExpression
      )
    )
  ;
    inverted_expression(SupportExpression),
    (
      var(FromMagnitudeExpression)
    ->
      expression_to_argument(ToMagnitudeExpression, ToMagnitudeValue),
      quantity_value_index(ToMagnitudeValue, ToMagnitudeIndex),
      expression_from_argument(SupportExpression, FromQuantity),
      quantity_magnitude_quantity_space(FromQuantity, FromQuantitySpace),
      quantity_space_to_cardinality(
        FromQuantitySpace,
        QuantitySpaceCardinality
      ),
      FromMagnitudeIndex is QuantitySpaceCardinality - ToMagnitudeIndex - 1,
      quantity_space_quantity_value_index(
        FromQuantitySpace,
        FromMagnitudeIndex,
        FromMagnitudeValue
      ),
      !,
      find_or_add_expression(
        FromQuantity,
        expression:qp_magnitude_equal_to,
        FromMagnitudeValue,
        FromMagnitudeExpression
      )
    ;
      var(ToMagnitudeExpression)
    ->
      expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
      quantity_value_index(FromMagnitudeValue, FromMagnitudeIndex),
      expression_to_argument(SupportExpression, ToQuantity),
      quantity_magnitude_quantity_space(ToQuantity, ToQuantitySpace),
      quantity_space_to_cardinality(
        ToQuantitySpace,
        QuantitySpaceCardinality
      ),
      ToMagnitudeIndex is QuantitySpaceCardinality - FromMagnitudeIndex - 1,
      quantity_space_quantity_value_index(
        ToQuantitySpace,
        ToMagnitudeIndex,
        ToMagnitudeValue
      ),
      !,
      find_or_add_expression(
        ToQuantity,
        expression:qp_magnitude_equal_to,
        ToMagnitudeValue,
        ToMagnitudeExpression
      )
    )
  ).


% VALUE CORRESPONDENCE %

% Magnitude value correspondence.
calculate_correspondence(
  FromMagnitudeExpression,
  SupportExpression,
  ToMagnitudeExpression
):-
  nonvar(SupportExpression),
  rdfs_individual_of(SupportExpression, expression:qp_magnitude_equal_to_correspondence),
  (
    var(FromMagnitudeExpression)
  ->
    % Check whether the support expression magnitude has been set.
    % Then this correspondence is active.
    expression_to_argument(SupportExpression, ToMagnitudeValue),
    expression_to_argument(ToMagnitudeExpression, ToMagnitudeValue),

    expression_from_argument(SupportExpression, FromMagnitudeValue),
    quantity_quantity_value(FromQuantity, FromMagnitudeValue),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_magnitude_equal_to,
      FromMagnitudeValue,
      FromMagnitudeExpression
    )
  ;
    var(ToMagnitudeExpression)
  ->
    % Check whether the support expression magnitude has been set.
    % Then this correspondence is active.
    expression_from_argument(SupportExpression, FromMagnitudeValue),
    expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),

    expression_from_argument(SupportExpression, ToMagnitudeValue),
    quantity_quantity_value(ToQuantity, ToMagnitudeValue),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_magnitude_equal_to,
      ToMagnitudeValue,
      ToMagnitudeExpression
    )
  ;
    % This should never be the case.
    fail
  ).
% Derivative value correspondence.
calculate_correspondence(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  nonvar(SupportExpression),
  rdfs_individual_of(SupportExpression, expression:qp_derivative_equal_to_correspondence),
  (
    var(FromDerivativeExpression)
  ->
    % Check whether the support expression magnitude has been set.
    % Then this correspondence is active.
    expression_to_argument(SupportExpression, ToDerivativeValue),
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),

    expression_from_argument(SupportExpression, FromDerivativeValue),
    quantity_quantity_value(FromQuantity, FromDerivativeValue),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_derivative_equal_to,
      FromDerivativeValue,
      FromDerivativeExpression
    )
  ;
    var(ToDerivativeExpression)
  ->
    % Check whether the support expression magnitude has been set.
    % Then this correspondence is active.
    expression_from_argument(SupportExpression, FromDerivativeValue),
    expression_to_argument(FromDerivativeExpression, FromDerivativeValue),

    expression_from_argument(SupportExpression, ToDerivativeValue),
    quantity_quantity_value(ToQuantity, ToDerivativeValue),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_derivative_equal_to,
      ToDerivativeValue,
      ToDerivativeExpression
    )
  ;
    % This should never be the case.
    fail
  ).
*/


% QUANTITY INFLUENCE %

calculate_quantity_influence(
  FromMagnitudeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  var(SupportExpression),
  (
    expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
    zero_magnitude_quantity_value(FromMagnitudeValue)
  ;
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    steady_derivative_quantity_value(ToDerivativeValue)
  ),
  !,
  fail.
calculate_quantity_influence(
  FromMagnitudeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  var(SupportExpression),
  expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),!,
  expression_to_argument(ToDerivativeExpression, ToDerivativeValue),!,
  (
    (
      positive_magnitude_quantity_value(FromMagnitudeValue),
      increasing_derivative_quantity_value(ToDerivativeValue)
    ;
      negative_magnitude_quantity_value(FromMagnitudeValue),
      decreasing_derivative_quantity_value(ToDerivativeValue)
    )
  ->
    rdf_global_id(expression:positive_quantity_influence, ExpressionDefinition)
  ;
    (
      positive_magnitude_quantity_value(FromMagnitudeValue),
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ;
      negative_magnitude_quantity_value(FromMagnitudeValue),
      increasing_derivative_quantity_value(ToDerivativeValue)
    )
  ->
    rdf_global_id(expression:negative_quantity_influence, ExpressionDefinition)
  ;
    % This should never be the case.
    fail
  ),
  expression_from_argument(FromMagnitudeExpression, FromQuantity),!,
  expression_from_argument(ToDerivativeExpression, ToQuantity),!,
  find_or_add_expression(
    FromQuantity,
    ExpressionDefinition,
    ToQuantity,
    SupportExpression
  ),
  !.
calculate_quantity_influence(
  FromMagnitudeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  greater_than_expression(SupportExpression),
  (
    var(FromMagnitudeExpression)
  ->
    expression_from_argument(SupportExpression, FromQuantity),
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      positive_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      zero_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      negative_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_magnitude_equal_to,
      FromMagnitudeValue,
      FromMagnitudeExpression
    )
  ;
    var(ToDerivativeExpression)
  ->
    expression_to_argument(SupportExpression, ToQuantity),
    expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
    (
      positive_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      zero_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      negative_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_derivative_equal_to,
      ToDerivativeValue,
      ToDerivativeExpression
    )
  ;
    % This should never be the case.
    fail
  ).
calculate_quantity_influence(
  FromMagnitudeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  smaller_than_expression(SupportExpression),
  (
    var(FromMagnitudeExpression)
  ->
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    expression_from_argument(SupportExpression, FromQuantity),
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      negative_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      zero_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      positive_magnitude_quantity_value(FromMagnitudeValue),
      quantity_quantity_value(FromQuantity, FromMagnitudeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_magnitude_equal_to,
      FromMagnitudeValue,
      FromMagnitudeExpression
    )
  ;
    var(ToDerivativeExpression)
  ->
    expression_to_argument(SupportExpression, ToQuantity),
    expression_to_argument(FromMagnitudeExpression, FromMagnitudeValue),
    (
      negative_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      zero_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      positive_magnitude_quantity_value(FromMagnitudeValue)
    ->
      quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      fail
    ),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_derivative_equal_to,
      ToDerivativeValue,
      ToDerivativeExpression
    )
  ;
    % This should never be the case.
    fail
  ),
  !.

% COMPETITIVE QUANTITY

/*
calculate_competitive_quantity_causality
  FromDerivativeExpression,
  FromMagnitudeExpression,
  QuantityProportionalityExpression,
  QuantityInfluenceExpression,
  ToDerivativeExpression
):-

*/

% COMPETITIVE QUANTITY INFLUENCE %

calculate_competitive_quantity_influence(
  FromInequalityExpression,
  FromMagnitudeExpression1,
  FromMagnitudeExpression2,
  SupportExpression1,
  SupportExpression2,
  ToDerivativeExpression
):-
  var(FromInequalityExpression),
  i(FromMagnitudeExpression1, SupportExpression1, ToDerivativeValue1),
  i(FromMagnitudeExpression2, SupportExpression2, ToDerivativeValue2),
  expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
  (
    increasing_derivative_quantity_value(ToDerivativeValue1),
    decreasing_derivative_quantity_value(ToDerivativeValue2)
  ->
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_greater_than, ExpressionDefinition)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_smaller_than, ExpressionDefinition)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_equal_to, ExpressionDefinition)
    ;
      % This should never be the case.
      fail
    )
  ;
    decreasing_derivative_quantity_value(ToDerivativeValue1),
    increasing_derivative_quantity_value(ToDerivativeValue2)
  ->
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_smaller_than, ExpressionDefinition)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_greater_than, ExpressionDefinition)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      rdf_global_id(expression:qq_magnitude_equal_to, ExpressionDefinition)
    ;
      % This should never be the case.
      fail
    )
  ;
    % Undefined.
    fail
  ),
  expression_from_argument(FromMagnitudeExpression1, FromQuantity1),
  expression_from_argument(FromMagnitudeExpression2, FromQuantity2),
  !,
  find_or_add_expression(
    FromQuantity1,
    ExpressionDefinition,
    FromQuantity2,
    FromInequalityExpression
  ).
calculate_competitive_quantity_influence(
  FromInequalityExpression,
  FromMagnitudeExpression1,
  FromMagnitudeExpression2,
  SupportExpression1,
  SupportExpression2,
  ToDerivativeExpression
):-
  var(ToDerivativeExpression),
  i(FromMagnitudeExpression1, SupportExpression1, ToDerivativeValue1),
  quantity_value_index(ToDerivativeValue1, ToDerivativeIndex1),
  i(FromMagnitudeExpression2, SupportExpression2, ToDerivativeValue2),
  quantity_value_index(ToDerivativeValue2, ToDerivativeIndex2),
  (
    ToDerivativeIndex1 == ToDerivativeIndex2
  ->
    ToDerivativeIndex = ToDerivativeIndex1
  ;
    rdfs_individual_of(
      FromInequalityExpression,
      expression:qq_magnitude_greater_than
    )
  ->
    ToDerivativeIndex = ToDerivativeIndex1
  ;
    rdfs_individual_of(
      FromInequalityExpression,
      expression:qq_magnitude_smaller_than
    )
  ->
    ToDerivativeIndex = ToDerivativeIndex2
  ;
    % This should never be the case.
    fail
  ),
  expression_to_argument(SupportExpression1, ToQuantity),
  quantity_derivative_quantity_space(ToQuantity, ToDerivativeQuantitySpace),
  quantity_space_quantity_value_index(
    ToDerivativeQuantitySpace,
    ToDerivativeIndex,
    ToDerivativeValue
  ),
  !,
  find_or_add_expression(
    ToQuantity,
    expression:qp_derivative_equal_to,
    ToDerivativeValue,
    ToDerivativeExpression
  ).

%% i(
%%   +MagnitudeExpression:expression,
%%   +SupportExpression:expression,
%%   -DerivativeValue:derivative_value
%% ) is det.

i(MagnitudeExpression, _SupportExpression, DerivativeValue):-
  expression_to_argument(MagnitudeExpression, MagnitudeValue),
  zero_magnitude_quantity_value(MagnitudeValue),
  steady_derivative_quantity_value(DerivativeValue),
  expression_from_argument(MagnitudeExpression, Quantity),
  quantity_quantity_value(Quantity, DerivativeValue),
  !.
i(MagnitudeExpression, SupportExpression, DerivativeValue):-
  expression_to_argument(MagnitudeExpression, MagnitudeValue),
  greater_than_expression(SupportExpression),
  (
    positive_magnitude_quantity_value(MagnitudeValue),
    increasing_derivative_quantity_value(DerivativeValue)
  ;
    negative_magnitude_quantity_value(MagnitudeValue),
    decreasing_derivative_quantity_value(DerivativeValue)
  ),
  expression_from_argument(MagnitudeExpression, Quantity),
  quantity_quantity_value(Quantity, DerivativeValue),
  !.
i(MagnitudeExpression, SupportExpression, DerivativeValue):-
  expression_to_argument(MagnitudeExpression, MagnitudeValue),
  smaller_than_expression(SupportExpression),
  (
    positive_magnitude_quantity_value(MagnitudeValue),
    decreasing_derivative_quantity_value(DerivativeValue)
  ;
    negative_magnitude_quantity_value(MagnitudeValue),
    increasing_derivative_quantity_value(DerivativeValue)
  ),
  expression_from_argument(MagnitudeExpression, Quantity),
  quantity_quantity_value(Quantity, DerivativeValue).



% QUANTITY PROPORTIONALITY %

calculate_quantity_proportionality(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  var(SupportExpression),
  (
    expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
    steady_derivative_quantity_value(FromDerivativeValue)
  ;
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    steady_derivative_quantity_value(ToDerivativeValue)
  ),
  !,
  fail.
calculate_quantity_proportionality(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  var(SupportExpression),
  expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
  expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
  (
    (
      increasing_derivative_quantity_value(FromDerivativeValue),
      increasing_derivative_quantity_value(ToDerivativeValue)
    ;
      decreasing_derivative_quantity_value(FromDerivativeValue),
      decreasing_derivative_quantity_value(ToDerivativeValue)
    )
  ->
    rdf_global_id(expression:positive_quantity_proportionality, ExpressionDefinition)
  ;
    (
      increasing_derivative_quantity_value(FromDerivativeValue),
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ;
      decreasing_derivative_quantity_value(FromDerivativeValue),
      increasing_derivative_quantity_value(ToDerivativeValue)
    )
  ->
    rdf_global_id(expression:negative_quantity_proportionality, ExpressionDefinition)
  ;
    % This should never be the case.
    fail
  ),
  expression_from_argument(FromDerivativeExpression, FromQuantity),
  expression_from_argument(ToDerivativeExpression, ToQuantity),
  !,
  find_or_add_expression(
    FromQuantity,
    ExpressionDefinition,
    ToQuantity,
    SupportExpression
  ).
calculate_quantity_proportionality(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  nonvar(SupportExpression),
  greater_than_expression(SupportExpression),
  (
    var(FromDerivativeExpression)
  ->
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    expression_from_argument(SupportExpression, FromQuantity),
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_increasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_steady_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_decreasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_derivative_equal_to,
      FromDerivativeValue,
      FromDerivativeExpression
    )
  ;
    var(ToDerivativeExpression)
  ->
    expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
    expression_to_argument(SupportExpression, ToQuantity),
    (
      increasing_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      steady_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      decreasing_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      % This should never be the case.
      fail
    ),
    expression_from_argument(SupportExpression, FromQuantity),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_derivative_equal_to,
      ToDerivativeValue,
      ToDerivativeExpression
    )
  ;
    % This should never be the case.
    fail
  ).
calculate_quantity_proportionality(
  FromDerivativeExpression,
  SupportExpression,
  ToDerivativeExpression
):-
  nonvar(SupportExpression),
  smaller_than_expression(SupportExpression),
  (
    var(FromDerivativeExpression)
  ->
    expression_to_argument(ToDerivativeExpression, ToDerivativeValue),
    expression_from_argument(SupportExpression, FromQuantity),
    (
      increasing_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_decreasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      steady_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_steady_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      decreasing_derivative_quantity_value(ToDerivativeValue)
    ->
      quantity_increasing_derivative_quantity_value(FromQuantity, FromDerivativeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      FromQuantity,
      expression:qp_derivative_equal_to,
      FromDerivativeValue,
      FromDerivativeExpression
    )
  ;
    var(ToDerivativeExpression)
  ->
    expression_to_argument(FromDerivativeExpression, FromDerivativeValue),
    expression_to_argument(SupportExpression, ToQuantity),
    (
      increasing_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_decreasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      steady_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_steady_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      decreasing_derivative_quantity_value(FromDerivativeValue)
    ->
      quantity_increasing_derivative_quantity_value(ToQuantity, ToDerivativeValue)
    ;
      % This should never be the case.
      fail
    ),
    !,
    find_or_add_expression(
      ToQuantity,
      expression:qp_derivative_equal_to,
      ToDerivativeValue,
      ToDerivativeExpression
    )
  ;
    % This should never be the case.
    fail
  ),
  !.



% RETRIEVAL %

calculate_retrieval(Expression, Expression).



% SCENARIO VALUE %

calculate_scenario_value(Expression, Expression).



% VALUE DETERMINATION %

inequality_expression_to_integer(InequalityExpression, 9/1):-
  greater_than_expression(InequalityExpression),
  !.
inequality_expression_to_integer(InequalityExpression, 5/5):-
  equal_to_expression(InequalityExpression),
  !.
inequality_expression_to_integer(InequalityExpression, 1/9):-
  smaller_than(InequalityExpression).

calculate_value_determination_type1(
  InequalityExpression,
  SupportExpression,
  MagnitudeExpression
):-
  var(MagnitudeExpression),
  expression_from_argument(SupportExpression, Quantity),
  expression_to_argument(SupportExpression, CalculusExpression),

  % Argument order.
  (
    expression_from_argument(InequalityExpression, Quantity),
    expression_from_argument(CalculusExpression, Quantity)
  ->
    X = normal
  ;
    X = inverted
  ),

  % Magnitude value.
  (
% A=B
    equal_to_expression(InequalityExpression)
  ->
    quantity_zero_magnitude_quantity_value(Quantity, MagnitudeValue)
  ;
% A > B
% A - B
    X = normal,
    greater_than_expression(InequalityExpression),
    negative_calculus_expression(CalculusExpression)
  ->
    quantity_positive_magnitude_quantity_value(Quantity, MagnitudeValue),
    !
  ;
    X = inverted,
    greater_than_expression(InequalityExpression),
    positive_calculus_expression(CalculusExpression)
  ->
    quantity_positive_magnitude_quantity_value(Quantity, MagnitudeValue),
    !
  ;
% A < B
% A - B
    X = normal,
    greater_than_expression(InequalityExpression),
    negative_calculus_expression(CalculusExpression)
  ->
    quantity_negative_magnitude_quantity_value(Quantity, MagnitudeValue),
    !
  ;
    X = inverted,
    greater_than_expression(InequalityExpression),
    negative_calculus_expression(CalculusExpression)
  ->
    quantity_negative_magnitude_quantity_value(Quantity, MagnitudeValue),
    !
  ;
    true
  ),

  % Expression.
  find_or_add_expression(
    Quantity,
    expression:qp_magnitude_equal_to,
    MagnitudeValue,
    MagnitudeExpression
  ).
