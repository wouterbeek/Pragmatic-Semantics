:- module(
  qr_verb,
  [
    expression_to_sentence_list/2 % +Expression:expression
                                  % -SentenceList:list(atom)
  ]
).

/** <module> QR Verbalizations

Predicates that verbalize QR content.

@author Wouter Beek
@version 2012/09
*/

:- use_module(ccm(ccm_api)).
:- use_module(qr(qr_api)).

:- multifile
  expression_to_sentence_list(_Expression, _SentenceList).



expression_to_sentence_list(
  Expression,
  [FromQuantityLabel, ExpressionDefinitionLabel, ToQuantityLabel]
):-
  rdfs_individual_of(Expression, expression:qq),
  !,
  expression_from_argument(Expression, FromQuantity),
  once(quantity_label(FromQuantity, FromQuantityLabel)),
  expression_to_argument(Expression, ToQuantity),
  once(quantity_label(ToQuantity, ToQuantityLabel)),
  expression_definition_expression(ExpressionDefinition, Expression),
  once(expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  )).
expression_to_sentence_list(
  Expression,
  [QuantityLabel, ExpressionDefinitionLabel, ValueLabel]
):-
  rdfs_individual_of(Expression, expression:qp),
  !,
  expression_from_argument(Expression, Quantity),
  once(quantity_label(Quantity, QuantityLabel)),
  expression_to_argument(Expression, Value),
  once(quantity_value_label(Value, ValueLabel)),
  expression_definition_expression(ExpressionDefinition, Expression),
  once(expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  )).
expression_to_sentence_list(
  Expression,
  [FromValueLabel, ExpressionDefinitionLabel, ToValueLabel]
):-
  rdfs_individual_of(Expression, expression:vv),
  !,
  expression_from_argument(Expression, FromValue),
  once(quantity_value_label(FromValue, FromValueLabel)),
  expression_to_argument(Expression, ToValue),
  once(quantity_value_label(ToValue, ToValueLabel)),
  expression_definition_expression(ExpressionDefinition, Expression),
  once(expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  )).
expression_to_sentence_list(
  Expression,
  [
    QuantityLabel,
    ExpressionDefinitionLabel,
    FromQuantityLabel,
    CalculusExpressionDefinitionLabel,
    ToQuantityLabel
  ]
):-
  rdfs_individual_of(Expression, expression:qc),
  !,
  expression_from_argument(Expression, Quantity),
  once(quantity_label(Quantity, QuantityLabel)),
  expression_to_argument(Expression, CalculusExpression),
  expression_from_argument(CalculusExpression, FromQuantity),
  once(quantity_label(FromQuantity, FromQuantityLabel)),
  expression_definition_expression(
    CalculusExpressionDefinition,
    CalculusExpression
  ),
  once(expression_definition_label(
    CalculusExpressionDefinition,
    CalculusExpressionDefinitionLabel
  )),
  expression_to_argument(CalculusExpression, ToQuantity),
  once(quantity_label(ToQuantity, ToQuantityLabel)),
  expression_definition_expression(ExpressionDefinition, Expression),
  once(expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  )).
expression_to_sentence_list(
  Expression,
  [QuantityLabel, ExpressionDefinitionLabel, QuantitySpaceLabel]
):-
  rdfs_individual_of(Expression, expression:quantity_space),
  !,
  expression_from_argument(Expression, Quantity),
  once(quantity_label(Quantity, QuantityLabel)),
  expression_to_argument(Expression, QuantitySpace),
  once(quantity_space_label(QuantitySpace, QuantitySpaceLabel)),
  expression_definition_expression(ExpressionDefinition, Expression),
  once(expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  )).
