:- module(
  ccm_verb,
  [
    expression_to_sentence/2, % +Expression:expression
                              % -Sentence:atom
    expression_to_sentence_list/2, % +Expression:expression
                                   % -Sentence:list(atom)
    point_to_sentence/2, % +Expression:expression
                         % -Sentence:atom
    point_to_sentence_list/2, % +Point:point
                              % -Sentence:list(atom)
    pragmatics/2, % +Explanation:explanation
                  % -Story:atom
    sentence/1, % ?Sentence:list(atom)
    sentences/0,
    sentence_list_to_sentence/2, % +SentenceList:list(atom)
                                 % -Sentence:atom
    verbalize_model/0,
    verbalize_model/1 % +Space:space
  ]).

/** <module> Verbalization for CCM.

@author Wouter Beek
@version 2011/12-2012/03
*/

:- use_module(ccm(ccm_export)).
:- use_module(ccm(ccm_api)).
:- use_module(generic(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(qr(qr_api)).
:- use_module(qr(causal_explanation)).
:- use_module(
  qr(qr_verb),
  [expression_to_sentence_list/2 as qr_expression_to_sentence_list]
).
:- use_module(
  self(self_verb),
  [expression_to_sentence_list/2 as self_expression_to_sentence_list]
).

:- discontiguous(dict/3).



%% expression_to_sentence(+Expression:expression, -Sentence:atom) is det.
% Returns a natural language sentence describing the expression's contents.
%
% @param Expression An expression.
% @param Sentence An atom natural language sentence.

% A conditional statement.
expression_to_sentence(conditional(Condition,Consequence), Sentence):-
  !,
  expression_to_sentence_list(Condition, ConditionList),
  expression_to_sentence_list(Consequence, ConsequenceList),
  append([[if], ConditionList, [then], ConsequenceList], SentenceList),
  sentence_list_to_sentence(SentenceList, Sentence).
% We can use the temporal aspect.
expression_to_sentence(Expression, Sentence):-
  point(Expression, Point),
  !,
  point_to_sentence(Point, Sentence).
% We cannot use the termporal aspect.
expression_to_sentence(Expression, Sentence):-
  expression_to_sentence_list(Expression, SentenceList),
  sentence_list_to_sentence(SentenceList, Sentence).

point_to_sentence(Point, Sentence):-
  point_to_sentence_list(Point, SentenceList),
  sentence_list_to_sentence(SentenceList, Sentence).

point_to_sentence_list(Point, SentenceList):-
  point(Expression, Point),
  expression_to_sentence_list(Expression, SentenceList).

expression_to_sentence_list(Expression, SentenceList):-
  conditional_expression(Expression),
  !,
  expression_from_argument(Expression, ConditionalExpression),
  expression_to_sentence_list(ConditionalExpression, ConditionalSentenceList),
  expression_to_argument(Expression, ConsequentialExpression),
  expression_to_sentence_list(
    ConsequentialExpression,
    ConsequentialSentenceList
  ),
  append(
    [
      ['IF['],
      ConditionalSentenceList,
      [']THEN['],
      ConsequentialSentenceList,
      [']']
    ],
    SentenceList
  ).
expression_to_sentence_list(Expression, SentenceList):-
  qr_expression_to_sentence_list(Expression, SentenceList).
expression_to_sentence_list(Expression, SentenceList):-
  self_expression_to_sentence_list(Expression, SentenceList).

pragmatics(Explanation, Sentence):-
  once(explanation_component(Explanation, Component)),
  pragmatics(Explanation, Component, Sentence).

pragmatics(_Explanation, Component, Sentence):-
  rdfs_individual_of(Component, component:magnitude_transition),

  % Magnitude input
  component_expression(
    Component,
    component:has_magnitude_input,
    MagnitudeExpression1
  ),
  once(from_quantity_expression(Quantity, MagnitudeExpression1)),
  once(quantity_label(Quantity, QuantityName)),
  once(expression_to_argument(MagnitudeExpression1, MagnitudeValue1)),
  once(quantity_value_label(MagnitudeValue1, MagnitudeValueName1)),

  % Derivative input
  component_expression(
    Component,
    component:has_derivative_input,
    DerivativeExpression
  ),
  once(expression_to_argument(DerivativeExpression, DerivativeValue)),
  once(quantity_value_label(DerivativeValue, DerivativeValueName)),

  % Magnitude output
  component_expression(
    Component,
    component:has_magnitude_output,
    MagnitudeExpression2
  ),
  once(expression_to_argument(MagnitudeExpression2, MagnitudeValue2)),
  once(quantity_value_label(MagnitudeValue2, MagnitudeValueName2)),

  append(
    [
      QuantityName, was, MagnitudeValueName1, and, DerivativeValueName,
      therefore, it, is, now, MagnitudeValueName2
    ],
    SentenceList
  ),
  sentence_list_to_sentence(SentenceList, Sentence).
pragmatics(Explanation, _Component, Sentence):-
  findall(
    PremiseSentenceAtom,
    (
      explanation_premise(Explanation, Premise),
      rdf(Premise, explanation:about_point, PremisePoint, ccm),
      point_to_sentence_list(PremisePoint, PremiseSentenceList),
      atomic_list_concat(PremiseSentenceList, PremiseSentenceAtom)
    ),
    PremiseSentenceAtoms
  ),
  atomic_list_concat(PremiseSentenceAtoms, ' and ', PremiseSentencesAtom),
  once(explanation_derivation_rule(Explanation, Derivation)),
  once(rdf(Derivation, explanation:about_point, DerivationPoint, ccm)),
  point_to_sentence_list(DerivationPoint, DerivationSentenceList),
  once(explanation_conclusion(Explanation, Conclusion)),
  once(rdf(Conclusion, explanation:about_point, ConclusionPoint, ccm)),
  point_to_sentence_list(ConclusionPoint, ConclusionSentenceList),
  append(
    [
      [PremiseSentencesAtom, and],
      DerivationSentenceList,
      [';', therefore],
      ConclusionSentenceList
    ],
    SentenceList
  ),
  sentence_list_to_sentence(SentenceList, Sentence).

%% sentence_list_to_sentence(+SentenceList:list(atom), -Sentence:atom) is det.
% Returns the natural language sentence that consists of the given list
% of atomic words.

sentence_list_to_sentence(SentenceList, Sentence):-
  atomic_list_concat(SentenceList, ' ', Sentence0),
  atomic_concat(Sentence0, '.', Sentence1),
  titlecase(Sentence1, Sentence).

verbalize_model:-
  forall(
    space(Space),
    verbalize_model(Space)
  ).

verbalize_model(Space):-
  once(space_label(Space, SpaceLabel)),
  debug(ccm_verb, 'The verbalization for space ~w:', [SpaceLabel]),
  space_to_points(Space, Points),
  forall(
    member(Point, Points),
    (
      point_to_sentence(Point, Sentence),
      debug(ccm_verb, '  [SENTENCE] ~w', [Sentence])
    )
  ).



% GRAMMAR %

sentence(Sentence):-
  sentence(ParseTree, LambdaAbstraction, Sentence, []),
  beta_conversion(LambdaAbstraction, Semantics),
  format(
    atom(GraphLabel),
    'Sentence: ~w - Meaning: ~w',
    [Sentence, Semantics]
  ),
  write_to_dot(
    write_parse_tree,
    'ccm/parse_tree',
    parse_tree,
    GraphLabel,
    ParseTree
  ),
  debug(ccm_verb, '~w', [ParseTree]).

sentences:-
  findall(
    ParseTree,
    sentence(ParseTree, _LambdaAbstraction, _Sentence, []),
    ParseTrees
  ),
  write_to_dot(
    write_parse_trees,
    'ccm/parse_trees',
    parse_tree,
    'Sentence Parses',
    ParseTrees
  ),
  debug(ccm_verb, '~w', [ParseTrees]).

copula(copula(Word), lam(P,lam(Q,lam(X,(app(P,X),app(Q,X))))), Number) -->
  [Word],
  {
    dict(copula, Word, Number)
  }.

det(determiner(a), lam(P,lam(Q,(app(P,X),app(Q,X)))), singular) -->
  [a].
det(determiner(every), lam(P,lam(Q,forall(app(P,X),app(Q,X)))), singular) -->
  [every].

ditransitive_verb(
  ditransitive_verb(Word),
  lam(P,lam(R,lam(Q,app(R,app(P,lam(X,lam(Y,Formula))))))),
  Number
) -->
  [Word],
  {
    dict(ditransitive_verb, Word, Number),
    Formula =.. [Word, Q, X, Y]
  }.

intransitive_verb(intransitive_verb(Word), lam(X, Formula), Number) -->
  [Word],
  {
    dict(intransitive_verb, Word, Number),
    Formula =.. [Word,X]
  }.

noun(noun(Word), lam(X,Formula), Number) -->
  [Word],
  {
    dict(noun, Word, Number),
    Formula =.. [Word,X]
  }.

noun_phrase(noun_phrase(T), NounPhrase, singular, _Coordination) -->
  proper_noun(T, NounPhrase).
noun_phrase(
  noun_phrase(T1,T2),
  app(Determiner,Noun),
  Number,
  _Coordination
) -->
  det(T1, Determiner, Number),
  noun(T2, Noun, Number).
noun_phrase(
  noun_phrase(T1,T2,T3),
  app(app(Copula,NP1),NP2),
  Number,
  coordination
) -->
  noun_phrase(T1, NP1, Number1, no_coordination),
  copula(T2, Copula, Number2),
  noun_phrase(T3, NP2, Number1, no_coordination),
  {
    % The coordinates cannot have the same meaning.
    NP1 \== NP2,

    % Establish the number.
    (
      Number1 = singular,
      Number2 = singular
    ->
      Number = singular
    ;
      Number = plural
    )
  }.

proper_noun(proper_noun(Word), lam(X,app(X,Word))) -->
  [Word],
  {
    dict(proper_noun, Word)
  }.

sentence(sentence(T1,T2), app(M1,M2)) -->
  noun_phrase(T1, M1, Number, _Coordination),
  verb_phrase(T2, M2, Number).

transitive_verb(
  transitive_verb(Word),
  lam(P,lam(Q,app(P,lam(X,Formula)))),
  Number
) -->
  [Word],
  {
    dict(transitive_verb, Word, Number),
    Formula =.. [Word,Q,X]
  }.

verb_phrase(verb_phrase(T), IntransitiveVerb, Number) -->
  intransitive_verb(T, IntransitiveVerb, Number).
verb_phrase(verb_phrase(T1,T2), app(TransitiveVerb, NounPhrase), Number) -->
  transitive_verb(T1, TransitiveVerb, Number),
  noun_phrase(T2, NounPhrase, _Number, _Coordination).
verb_phrase(
  verb_phrase(T1,T2,T3),
  app(app(DitransitiveVerb, NounPhrase1), NounPhrase2),
  Number
) -->
  ditransitive_verb(T1, DitransitiveVerb, Number),
  noun_phrase(T2, NounPhrase1, _Number1, _Coordination1),
  noun_phrase(T3, NounPhrase2, _Number2, _Coordination2).

dict(copula, and, plural).
dict(copula, or, singular).
dict(ditransitive_verb, offer, plural).
dict(ditransitive_verb, offers, singular).
dict(noun, banana, singular).
dict(noun, bananas, plural).
dict(noun, monkey, singular).
dict(noun, monkeys, plural).
dict(proper_noun, obama).
dict(intransitive_verb, snort, plural).
dict(intransitive_verb, snorts, signular).
dict(intransitive_verb, walk, plural).
dict(intransitive_verb, walks, singular).
dict(transitive_verb, love, plural).
dict(transitive_verb, loves, singular).

beta_conversion(E, Sol):-
  beta_conversion(E, [], Sol).

beta_conversion(X, [], Y):-
  var(X),
  !,
  Y = X.
beta_conversion(E, Stack, Sol):-
  E = app(A,B),
  !,
  nonvar(A),
  beta_conversion(A, [B | Stack], Sol).
beta_conversion(E, [X | Stack], Sol):-
  E = lam(X,F),
  !,
  beta_conversion(F, Stack, Sol).
beta_conversion(E, Stack, Sol):-
  E =.. [F | SubEs],
  findall(
    Sol1,
    (
      member(SubE, SubEs),
      beta_conversion(SubE, Stack, Sol1)
    ),
    Sols
  ),
  Sol =.. [F | Sols].

/* [TODO]
% GRAMMAR %

s(Point) -->
  {
    point(
      Expression,
      FromQuantityValue,
      Relation,
      ToQuantityValue,
      _Space,
      Point
    ),
    quantity_value(FromQuantityValue),
    quantity_value(ToQuantityValue),
    quantity_quantity_value(FromQuantity, FromQuantityValue),
    quantity_quantity_value(ToQuantity, ToQuantityValue)
  },
  ns(FromQuantity),
  np(FromQuantityValue),
  vp2(Relation, ToQuantity, ToQuantityValue).
s(Point) -->
  {
    point(
      Expression,
      FromQuantity,
      Relation,
      ToArgument,
      _Space,
      Point
    ),
    quantity(FromQuantity)
  },
  np(FromQuantity),
  vp2(Relation, ToArgument).
s(Point) -->
  {
    point(
      Expression,
      FromCalculus,
      Relation,
      ToArgument,
      _Space,
      Point
    ),
    rdfs_individual_of(FromCalculus, expression:calculus)
  },
  np(FromCalculus),
  vp2(Relation, ToArgument).
s(_Point) -->
  [dummy].

np(Quantity) -->
  {quantity(Quantity)},
  det(_Uniqueness),
  adj(AttributeValue),
  {
    entity_attribute_value(Entity, AttributeValue),
    entity_quantity(Entity, Quantity)
  },
  ns(Entity),
  n(Quantity).
np(Quantity) -->
  {quantity(Quantity)},
  det(_Uniqueness),
  ns(Entity),
  {entity_quantity(Entity, Quantity)},
  n(Quantity).
np(QuantitySpace) -->
  {rdfs_individual_of(QuantitySpace, quantity_space:quantity_space)},
  n(QuantitySpace).
np(QuantityValue) -->
  {rdfs_individual_of(QuantityValue, value:quantity_value)},
  n(QuantityValue).
np(Calculus) -->
  {
    rdfs_individual_of(Calculus, expression:calculus),
    expression_from_argument(Calculus, FromQuantity),
    rdfs_individual_of(Calculus, Relation),
    expression_to_argument(Calculus, ToQuantity)
  },
  np(FromQuantity),
  vp2(Relation, ToQuantity).
np(Relation) -->
  det(_Uniqueness),
  {rdfs_subproperty_of(Relation, rdf:'Property')},
  n(Relation).

vp2(Relation, ToArgument) -->
  v2(Relation),
  np(ToArgument).
vp2(Relation, ToQuantity, ToQuantityValue) -->
  v2(Relation),
  ns(ToQuantity),
  np(ToQuantityValue).


% DICTIONARY %

%det(nonunique) -->
%  [a].
det(unique) -->
  [the].

ns(Entity) -->
  {
    rdfs_individual_of(Entity, entity:entity),
    entity_label(Entity, EntityLanguageLabel),
    atomic_concat(EntityLanguageLabel, '\x0027s', EntityS)
  },
  [EntityS].
ns(Quantity) -->
  {
    quantity(Quantity),
    quantity_label(Quantity, QuantityLanguageLabel),
    atomic_concat(QuantityLanguageLabel, '\x0027s', QuantityS)
  },
  [QuantityS].

adj(AttributeValue) -->
  {
    rdfs_individual_of(AttributeValue, value:attribute_value),
    attribute_value_label(AttributeValue, AttributeValueLanguageLabel)
  },
  [AttributeValueLanguageLabel].

n(Quantity) -->
  {
    quantity(Quantity),
    quantity_label(Quantity, QuantityLanguageLabel)
  },
  [QuantityLanguageLabel].
n(QuantitySpace) -->
  {
    rdfs_individual_of(QuantitySpace, quantity_space:quantity_space),
    quantity_space_label(
      QuantitySpace,
      QuantitySpaceLanguageLabel
    )
  },
  [QuantitySpaceLanguageLabel].
n(QuantityValue) -->
  {
    rdfs_individual_of(QuantityValue, value:value),
    quantity_value_label(QuantityValue, QuantityValueLanguageLabel)
  },
  [QuantityValueLanguageLabel].
n(Relation) -->
  {
    rdfs_subproperty_of(Relation, rdf:'Property'),
    rdfs_label(Relation, RelationLanguageLabel)
  },
  [RelationLanguageLabel].

v2(Relation) -->
  {rdfs_label(Relation, RelationLanguageLabel)},
  [RelationLanguageLabel].
*/
