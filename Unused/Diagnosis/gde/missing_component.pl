:- module(
  missing_component,
  [
    missing_component/1 % +Agent:agent
  ]
).

/** <module> Missing component repair.

Find missing components with respect to the simulation results and a
user-given expectation.

@author Wouter Beek
@version 2012/02-2012/04
*/

:- use_module(atms(atms_build)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_export)).
:- use_module(ccm(ccm_prob)).
:- use_module(diagnosis(diagnosis)).
:- use_module(gde(gde_exp)).
:- use_module(generic(math_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).
:- use_module(qr(qr_api)).



%% missing_component_definition(
%%   ?MissingComponentDefinition:component_definition
%% ) is nondet.
% The component definition of a potentially missing component.
%
% @param MissingComponentDefinition A component definition.
% @tbd Also include aggregate components.

missing_component_definition(MissingComponentDefinition):-
  instantiable_component_definition(MissingComponentDefinition),
  \+(aggregate_component_definition(MissingComponentDefinition)).

%% missing_component(+Diagnosis:diagnosis) is det.
% The main missing component method.
%
% @param Diagnosis A diagnosis object.

missing_component(Diagnosis):-
  % Assert all points as observations (i.e., premise them).
  diagnosis_atms(Diagnosis, ATMS),
  points(Points),
  maplist(add_premise(ATMS), Points),

  % Retrieve the expectation expression.
  expectation(Diagnosis, ExpectationPoint),

  % Gather component definitions and expressions.
  rdf_transaction(
    findall(
      MissingComponentDefinition/InputPointTuple/SupportExpression/Relevance,
      (
        missing_component_definition(MissingComponentDefinition),
        missing_component1(
          ExpectationPoint,
          MissingComponentDefinition,
          InputPointTuple,
          SupportExpression,
          Relevance
        )
      ),
      Results
    )
  ),
  % Watch out: The results can contain newly calculated expressions
  % with no existing points. We do not add these points right now.

  % Order for relevance.
  predsort_with_duplicates(compare_tuples4_arg4, Results, SortedResults),
  debug(missing, 'Results: ~w', [SortedResults]),

  % Generate messages.
  flag(message_id, _ID1, 1),
  findall(
    SupportExpression/Message,
    (
      member(
        MissingComponentDefinition/InputPointTuple/SupportExpression/Relevance,
        SortedResults
      ),
      component_definition_label(
        MissingComponentDefinition,
        MissingComponentDefinitionLabel
      ),
      (
        expression(SupportExpression)
      ->
        expression_from_argument(SupportExpression, FromArgument),
        expression_to_argument(SupportExpression, ToArgument),
        (
          FromArgument == ToArgument
        ->
          fail
        ;
          expression_to_ccm_label(SupportExpression, Label)
        )
      ;
        InputPointTuple = [InputPoint|_],
        point_to_label(InputPoint, Label)
      ),

      % Oh yes, filter points from other spaces.
      space_point(Space, ExpectationPoint),
      forall(
        member(InputPoint, InputPointTuple),
        space_point(Space, InputPoint)
      ),

      flag(message_id, ID, ID + 1),
      format(
        atom(Message),
        'Suggestion ~w:\t~2f\tAdd a ~w.\t~w',
        [ID, Relevance, MissingComponentDefinitionLabel, Label]
      )
    ),
    SupportExpressionMessagePairs
  ),
  flag(message_id, _ID2, 1),

  % Communicate labels.
  new(Label, name('Repair suggestions')),
  new(Dialog, communicationDialog(
    Label,
    prolog(SupportExpressionMessagePairs)
  )),
  send(Dialog, open_centered),

  if_then(debug_mode(true), write_ccm_to_dot(ccm_missing)).

%% missing_component1(
%%   +ExpectationPoint:point,
%%   +ComponentDefinition:component_definition,
%%   -InputPointTuple:list(point),
%%   -SupportExpression:support_expression,
%%   -Relevance:number
%% ) is nondet.
%

missing_component1(
  ExpectationPoint,
  ComponentDefinition,
  InputPointTuple,
  SupportExpression,
  Relevance
):-
  point(ExpectationExpression, Space, ExpectationPoint),
  component_definition_to_behavior_rule(ComponentDefinition, BehaviorRule),
  component_definition_to_expression_point_tuples(
    Space,
    ComponentDefinition,
    InputExpressionPointTuples
  ),
  % Check which expression tuples satisfy the behavioral criteria.
  member(InputExpressionTuple/InputPointTuple, InputExpressionPointTuples),
  append(
    InputExpressionTuple,
    [SupportExpression, ExpectationExpression],
    InputExpressionTuple1
  ),
  Call =.. [BehaviorRule | InputExpressionTuple1],
  call(gde_beh:Call),
  maplist(
    point_to_output_cardinality,
    InputPointTuple,
    OutputCardinalities
  ),
  sum_list(OutputCardinalities, OutputCardinality),
  RelevanceCorrection is OutputCardinality * 0.1,
  component_definition_to_probability(
    ComponentDefinition,
    ComponentDefinitionRelevance
  ),
  Relevance is ComponentDefinitionRelevance - RelevanceCorrection.

compare_tuples4_arg4(Order, _/_/_/Weight1, _/_/_/Weight2):-
  compare(Order, Weight2, Weight1).

%% component_definition_to_expression_point_tuples(
%%   +Space:space,
%%   +ComponentDefinition:component_definition,
%%   -InputExpressionPointTuples:list(list(point))
%% ) is det.

component_definition_to_expression_point_tuples(
  Space,
  ComponentDefinition,
  InputExpressionPointTuples
):-
  findall(
    Expressions,
    (
      % Collect all input point specifications for the given
      % component definition.
      component_definition_point_specification(
        ComponentDefinition,
        PointSpecification
      ),
      point_specification_relation(
        PointSpecification,
        PointRelation
      ),
      rdfs_subproperty_of(PointRelation, component:has_input),

      % Retrieve the expression definition for expressions asserted
      % at point clouds that are at the given input point specification.
      point_specification_expression_definition(
        PointSpecification,
        ExpressionDefinition
      ),
      % Find all expressions in the current CCM that are of this
      % expression definition.
      findall(
        Expression,
        find_expression_owl(
          ExpressionDefinition,
          _FromArgument,
          _ToArgument,
          Expression
        ),
        Expressions
      )
    ),
    % The lists in this list correspond to point specifications.
    % Each list contains a list of all expressions that adhere to that
    % point specification.
    Expressionss
  ),

  % From expression tuples to point tuples.
  % Tuple match gives all combinations of expressions from
  % the expressionss list.
  findall(
    InputExpressionTuple/InputPointTuple,
    (
      tuple_match(Expressionss, InputExpressionTuple),
      maplist(point, InputExpressionTuple, InputPointTuple),
      maplist(space_point(Space), InputPointTuple)
    ),
    InputExpressionPointTuples
  ).

tuple_match([], []).
tuple_match([Es | Ess], [E | Es1]):-
  member(E, Es),
  tuple_match(Ess, Es1).

find_expression_owl(
  ExpressionDefinition,
  FromArgument,
  ToArgument,
  Expression
):-
  rdf_global_id(expression:Name1, ExpressionDefinition),
  expression_definition_translation(Name1, Names),
  findall(
    ED,
    (
      member(Name, Names),
      rdf_global_id(expression:Name, ED)
    ),
    EDs
  ),
  find_expression_owl1(
    EDs,
    FromArgument,
    ToArgument,
    Expression
  ).

find_expression_owl1(
  EDs,
  FromArgument,
  ToArgument,
  Expression
):-
  expression(Expression),
  forall(
    member(ED, EDs),
    rdfs_individual_of(Expression, ED)
  ),
  expression_from_argument(Expression, FromArgument),
  expression_to_argument(Expression, ToArgument).

expression_definition_translation(derivative_calculus, [calculus, derivative]).
expression_definition_translation(magnitude_calculus, [calculus, magnitude]).
expression_definition_translation(qp_derivative_inequality, [derivatve, inequality, qp]).
expression_definition_translation(qp_mangitude_inequality, [inequality, magnitude, qp]).
expression_definition_translation(qq_derivative_inequality, [derivative, inequality, qq]).
expression_definition_translation(qq_mangitude_inequality, [inequality, magnitude, qq]).
expression_definition_translation(X, [X]).

point_to_output_cardinality(Point, OutputCardinality):-
  point_to_output_components(Point, OutputComponents),
  length(OutputComponents, OutputCardinality).
