:- module(
  ccm_aggr,
  [
    aggregate/1 % +Diagnosis:diagnosis
  ]
).

/** <module> Component Connection Model aggregation module.

This module creates and inspects aggregations in the CCM.

Aggregations are made for three reasons:
    1. Computational: the number of environments is O(2^n).
    2. Interaction: the number of probe points needed / the number of
       questions that are asked to the learner.
    3. Explanatory: higher-level explanations.

@author Wouter Beek
@version Jan 2012 - Aug 2012
*/

:- use_module(atms(atms_build)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_db)).
:- use_module(ccm(ccm_export)).
:- use_module(diagnosis(diagnosis)).
:- use_module(gde(gde_exp)).
:- use_module(graph_theory(graphs_adv)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).
:- use_module(qr(qr_api)).
:- use_module(rdf(rdf_read)).

:- meta_predicate aggregate_read_hierarchical_(+,2,-,-).



aggregate(Diagnosis):-
  % EXPECTATIONS
  diagnosis_to_expectations(Diagnosis, Expectations),
  maplist(point_cloud, Expectations, ExpectationPointClouds),

  % AGGREGATION & SCOPING
  cyclic_graph_traversal(
    [
      ccm_aggr:point_clouds_to_aggregated_component_clouds,
      ccm_api:component_clouds_to_point_clouds
    ],
    [ExpectationPointClouds, []],
    [[], []],
    [9999, 15],
    [ScopedComponentClouds, ScopedPointClouds]
  ),

  % CONSIDER
  learner(Diagnosis, Learner),
  maplist(consider_point_cloud(Learner), ScopedPointClouds),
  maplist(consider_component_cloud(Learner), ScopedComponentClouds),

  % ASSUMPTIONS
  % Add all scoped / considered components as assumptions.
  diagnosis_atms(Diagnosis, ATMS),
  maplist(add_assumption(ATMS), ScopedComponentClouds),

  % DEBUG
  if_then(debug_mode(true), write_ccm_to_rdf),
  if_then(debug_mode(true), write_ccm_to_dot(ccm_aggr)).

aggregate_(PointClouds):-
  aggregate_read(PointClouds, AggregateDefinition, Type, ComponentClouds),
  % We have found something. Now remove the choice points and make the
  % assertion.
  !,
  aggregate_write(AggregateDefinition, Type, ComponentClouds),
  debug(aggr, 'test', []).

aggregate_read(PointClouds, AggregateDefinition, Type, ComponentClouds):-
  member(PointCloud, PointClouds),
  aggregate_component_definition(AggregateDefinition),
  rdf_datatype(
    AggregateDefinition,
    component:can_instantiate,
    boolean,
    true,
    ccm
  ),
  aggregate_definition_to_rule(
    AggregateDefinition,
    Type,
    ComponentDefinitions
  ),
  (
    Type == depth
  ->
    aggregate_read_hierarchical(PointCloud, ComponentDefinitions, ComponentClouds)
  ;
    Type == width
  ->
    aggregate_read_competitive(PointCloud, ComponentDefinitions, ComponentClouds)
  ).

aggregate_write(AggregateDefinition, Type, ComponentClouds):-
  (
    Type == depth
  ->
    aggregate_write_hierarchical(AggregateDefinition, ComponentClouds)
  ;
    Type == width
  ->
    aggregate_write_competitive(AggregateDefinition, ComponentClouds)
  ).

aggregate_write_competitive(_AggregateDefinition, _ComponentClouds).

aggregate_write_hierarchical(AggregateDefinition, ComponentClouds):-
  % First we create the aggregate support expression.
  rdf_literal(AggregateDefinition, component:summary, SummarizePredicate, ccm),
  Call =.. [SummarizePredicate, ComponentClouds, SupportExpression],
  call(Call),

  % Now we move from component clouds to components.
  component_cloud_chain_to_component_chains(ComponentClouds, Componentss),
  forall(
    member(Components, Componentss),
    (
      % Retrieve the input point.
      first(Components, FirstComponent),
      once(component_input_point(FirstComponent, InputRelation, InputPoint)),

      % Add the support point.
      space_component(Space, FirstComponent),
      find_or_add_support_point(
        Space,
        SupportExpression,
        SupportPoint,
        _SupportComponent
      ),

      % Retrieve the output point.
      last(Components, LastComponent),
      once(
        component_output_point(LastComponent, OutputRelation, OutputPoint)
      ),
      % Add the aggregate component.
      find_or_add_component(
        AggregateDefinition,
        [InputRelation/InputPoint],
        [has_support/SupportPoint],
        [OutputRelation/OutputPoint],
        AggregateComponent,
        AggregateComponentCloud
      ),
      assert_component_subsumption(Components, AggregateComponent),
      assert_component_cloud_subsumption(
        ComponentClouds,
        AggregateComponentCloud
      ),
      debug(aggr, '~w are subsumed by ~w', [Components, AggregateComponent])
    )
  ).

aggregate_definition_to_rule(AggregateDefinition, Type, List):-
  rdf_literal(AggregateDefinition, component:subsumption_type, Type, ccm),
  rdf(AggregateDefinition, component:subsumes, RDFList, ccm),
  rdf_list([recursive(true)], RDFList, List).

aggregate_read_hierarchical(
  PointCloud,
  ComponentDefinitions,
  RevertedComponentClouds
):-
  backward(PointCloud, ComponentCloud),
  last(ComponentDefinitions, ComponentDefinition),
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  backward(ComponentCloud, NewPointCloud),
  reverse(ComponentDefinitions, [_ | RevertedComponentDefinitions]),
  aggregate_read_hierarchical_(
    NewPointCloud,
    backward,
    RevertedComponentDefinitions,
    ComponentClouds
  ),
  reverse([ComponentCloud | ComponentClouds], RevertedComponentClouds).
aggregate_read_hierarchical(
  PointCloud,
  ComponentDefinitions,
  ComponentClouds
):-
  forward(PointCloud, _ComponentCloud),
  aggregate_read_hierarchical_(
    PointCloud,
    forward,
    ComponentDefinitions,
    ComponentClouds
  ).

aggregate_read_hierarchical_(_PointCloud, _Predicate, [], []).
aggregate_read_hierarchical_(
  PointCloud,
  Predicate,
  [ComponentDefinition | ComponentDefinitions],
  [ComponentCloud | ComponentClouds]
):-
  call(Predicate, PointCloud, ComponentCloud),
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  call(Predicate, ComponentCloud, NewPointCloud),
  aggregate_read_hierarchical_(
    NewPointCloud,
    Predicate,
    ComponentDefinitions,
    ComponentClouds
  ).

%% aggregate_read_competitive(
%%   +PointCloud:point_cloud,
%%   +ComponentDefinitions:list(component_definition),
%%   -ComponentClouds:list(component_cloud)
%% ) is nondet.

aggregate_read_competitive(PointCloud, ComponentDefinitions, ComponentClouds):-
  forward(PointCloud, ComponentCloud),
  member(ComponentDefinition, ComponentDefinitions),
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  forward(ComponentCloud, NewPointCloud),
  aggregate_read_competitive_(
    NewPointCloud,
    ComponentDefinitions,
    ComponentClouds
  ).
aggregate_read_competitive(PointCloud, ComponentDefinitions, ComponentClouds):-
  aggregate_read_competitive_(PointCloud, ComponentDefinitions, ComponentClouds).

aggregate_read_competitive_(PointCloud, ComponentDefinitions, ComponentClouds):-
  aggregate_read_competitive_(
    PointCloud,
    ComponentDefinitions,
    [],
    ComponentClouds
  ).

aggregate_read_competitive_(
  PointCloud,
  [ComponentDefinition | ComponentDefinitions],
  ComponentCloudHistory,
  [ComponentCloud | ComponentClouds]
):-
  backward(PointCloud, ComponentCloud),
  \+(member(ComponentCloud, ComponentCloudHistory)),
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  aggregate_read_competitive_(
    PointCloud,
    ComponentDefinitions,
    [ComponentCloud | ComponentCloudHistory],
    ComponentClouds
  ).

backward(PointCloud, ComponentCloud):-
  point_cloud(PointCloud),
  !,
  component_cloud_output_point_cloud(ComponentCloud, PointCloud),
  active_component_cloud(ComponentCloud).
backward(ComponentCloud, PointCloud):-
  component_cloud(ComponentCloud),
  component_cloud_input_point_cloud(ComponentCloud, PointCloud),
  active_component_cloud(ComponentCloud).

forward(PointCloud, ComponentCloud):-
  point_cloud(PointCloud),
  !,
  component_cloud_input_point_cloud(ComponentCloud, PointCloud),
  active_component_cloud(ComponentCloud).
forward(PointCloud, ComponentCloud):-
  component_cloud(ComponentCloud),
  !,
  component_cloud_output_point_cloud(ComponentCloud, PointCloud),
  active_component_cloud(ComponentCloud).

point_clouds_to_aggregated_component_clouds(PointClouds, ComponentClouds):-
  point_clouds_to_aggregated_component_clouds_(PointClouds, ComponentClouds).
point_clouds_to_aggregated_component_clouds(PointClouds, ComponentClouds):-
  point_clouds_to_component_clouds(PointClouds, ComponentClouds).

point_clouds_to_aggregated_component_clouds_(PointClouds, ComponentClouds):-
  aggregate_(PointClouds),
  if_then(debug_mode(true), write_ccm_to_dot(ccm_aggr)),
  point_clouds_to_aggregated_component_clouds_(PointClouds, ComponentClouds).



% SUMMARIZE SUPPORT KNOWLEDGE %

%% summarize_causality(
%%   +ComponentClouds:list(component_cloud),
%%   -Summary:expression
%% ) is det.
% Returns the expression summarizing the given component cloud sequence.
%
% @param ComponentClouds A list of component clouds.
% @param Summary An expression.

summarize_causality(ComponentClouds, Summary):-
  % Signs.
  maplist(component_cloud_to_sign, ComponentClouds, Signs),
  % There may be multiple signs for one component cloud, but we are
  % not interested in those.
  !,
  multiply_signs(Signs, Sign),

  % Input quantity.
  first(ComponentClouds, FirstComponentCloud),
  once(component_cloud_support_expression(FirstComponentCloud, FirstSupport)),
  expression_from_argument(FirstSupport, FirstQuantity),

  % Expression definition.
  expression_definition_expression(ExpressionDefinition, FirstSupport),
  (
    quantity_proportionality_expression_definition(ExpressionDefinition)
  ->
    (
      greater_than(Sign)
    ->
      positive_quantity_proportionality_expression_definition(Relation)
    ;
      smaller_than(Sign)
    ->
      negative_quantity_proportionality_expression_definition(Relation)
    )
  ;
    quantity_influence_expression_definition(ExpressionDefinition)
  ->
    (
      greater_than(Sign)
    ->
      positive_quantity_influence_expression_definition(Relation)
    ;
      smaller_than(Sign)
    ->
      negative_quantity_influence_expression_definition(Relation)
    )
  ),

  % Output quantity.
  last(ComponentClouds, LastComponentCloud),
  once(component_cloud_support_expression(LastComponentCloud, LastSupport)),
  expression_to_argument(LastSupport, LastQuantity),

  % Create the summary.
  find_or_add_expression(FirstQuantity, Relation, LastQuantity, Summary).
