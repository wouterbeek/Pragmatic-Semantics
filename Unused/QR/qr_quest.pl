:- module(
  qr_quest,
  [
    expression_to_question/3 % +SpaceLabel:atom
                             % +Expression:expression
                             % -QuestionSentence:atom
  ]
).

/** <module> QR_QUEST

Methods that generate questions for the QR domain.

@author Wouter Beek
@version 2012/04-2014/05
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_verb)).
:- use_module(gde(gde_exp)).
:- use_module(ile(agent)).
:- use_module(qr(qr_api)).


expression_to_question(
  SpaceLabel,
  DerivativeExpression,
  QuestionSentence
):-
  derivative_quantity_value_expression(DerivativeExpression),
  !,
  from_quantity_expression(Quantity, DerivativeExpression),!,
  quantity_label(Quantity, QuantityLabel),!,
  format(
    atom(QuestionSentence),
    'What do you expect that the derivative of ~w is in ~w?',
    [QuantityLabel, SpaceLabel]
  ).
expression_to_question(
  SpaceLabel,
  MagnitudeExpression,
  QuestionSentence
):-
  magnitude_quantity_value_expression(MagnitudeExpression),
  !,
  from_quantity_expression(Quantity, MagnitudeExpression),
  quantity_label(Quantity, QuantityLabel),
  format(
    atom(QuestionSentence),
    'What do you expect that ~w is in ~w?',
    [QuantityLabel, SpaceLabel]
  ).
expression_to_question(SpaceLabel, InequalityExpression, QuestionSentence):-
  inequality_expression(InequalityExpression),
  !,

  % Select the from quantity.
  from_quantity_expression(FromQuantity, InequalityExpression),
  quantity_label(FromQuantity, FromQuantityLabel),!,

  % Select the to quantity.
  to_quantity_expression(ToQuantity, InequalityExpression),
  quantity_label(ToQuantity, ToQuantityLabel),!,

  format(
    atom(QuestionSentence),
    'Which inequality relation do you expect between ~w and ~w in ~w?',
    [FromQuantityLabel, ToQuantityLabel, SpaceLabel]
  ).
expression_to_question(SpaceLabel, Expression, QuestionSentence):-
  rdfs_individual_of(Expression, expression:qq),
  !,

  % Select the from quantity.
  from_quantity_expression(FromQuantity, Expression),
  quantity_label(FromQuantity, FromQuantityLabel),!,

  % Select the to quantity.
  to_quantity_expression(ToQuantity, Expression),
  quantity_label(ToQuantity, ToQuantityLabel),!,

  format(
    atom(QuestionSentence),
    'Which conceptual relation do you expect between ~w and ~w in ~w?',
    [FromQuantityLabel, ToQuantityLabel, SpaceLabel]
  ).
expression_to_question(SpaceLabel, Expression, QuestionSentence):-
  rdfs_individual_of(Expression, expression:vv_expression),
  !,

  % Select the from quantity.
  from_quantity_expression(FromQuantity, Expression),
  quantity_quantity_value(FromQuantity, FromQuantityValue),
  quantity_value_label(FromQuantityValue, FromQuantityValueLabel),

  % Select the to quantity.
  to_quantity_expression(ToQuantity, Expression),
  quantity_quantity_value(ToQuantity, ToQuantityValue),
  quantity_value_label(ToQuantityValue, ToQuantityValueLabel),
  
  format(
    atom(QuestionSentence),
    'Which conceptual relation do you expect between ~w and ~w in ~w?',
    [FromQuantityValueLabel, ToQuantityValueLabel, SpaceLabel]
  ).
