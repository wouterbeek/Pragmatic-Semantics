:- module(
  gde_pred,
  [
    predict_behavior/2 % +Diagnosis:diagnosis
                       % +Environment:environment
  ]
).

/** <module> The prediction engine for diagnosis.

A (constraint-propagation-like) prediction engine for use with GDE.

@author Wouter Beek
@version Aug 2011 - Feb 2012
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_build)).
:- use_module(atms(atms_db)).
:- use_module(atms(atms_env)). % This module is added for meta-calls.
:- use_module(atms(atms_export)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_conflict)).
:- use_module(diagnosis(diagnosis)).
:- use_module(gde(gde_beh)). % This module is added for meta-calls.
:- use_module(generic(list_ext)).
:- use_module(qr(qr_api)).



%% predict_behavior(+Diagnosis:diagnosis, +Environment:environment) is det.
% Predicts behavior for the given environment.
%
% @param Diagnosis A diagnosis.
% @param Environment An environment.

predict_behavior(Diagnosis, Environment):-
  environment_assumptions(Environment, ComponentClouds),
  (
    debug_mode(true)
  ->
    environment_id(Environment, EnvironmentString),
    maplist(component_cloud_id, ComponentClouds, ComponentCloudIDs),
    atomic_list_concat(ComponentCloudIDs, ',', ComponentCloudsID),
    debug(
      gde_environment,
      '[ENVIRONMENT] ~w: [~w]',
      [EnvironmentString, ComponentCloudsID]
    )
  ;
    true
  ),
  predict_behavior_small_to_big(Diagnosis, Environment, ComponentClouds).

unknown_points(History, Max, Component):-
  component_to_point_specifications(Component, PointSpecifications),
  findall(
    PointSpecification,
    (
      member(PointSpecification, PointSpecifications),
      \+(
        point_specification_point(
          Component,
          PointSpecification,
          History,
          _Point
        )
      )
    ),
    UnknownPointSpecifications
  ),
  length(UnknownPointSpecifications, NumberOf),
  NumberOf =< Max.

predict_behavior_small_to_big(Diagnosis, Environment, ComponentClouds):-
  diagnosis_atms(Diagnosis, ATMS),
  % We make a choice point for considering every component cloud from the
  % environment OUTSIDE of this method, so we do not backtrack over this
  % select statement. This is established by the cut that ends this
  % method.
  forall(
    member(ComponentCloud, ComponentClouds),
    % Apply a constraint, if possible.
    (
      ord_del_element(
        ComponentClouds,
        ComponentCloud,
        ReducedComponentClouds
      ),
      find_or_add_environment(
        ATMS,
        ReducedComponentClouds,
        ReducedEnvironment
      ),
      (
        find_applicable_constraint(
          Diagnosis,
          ComponentCloud,
          Environment,
          ReducedEnvironment
        )
      ;
        true
      )
    ;
      true
    )
  ).

%% find_applicable_constraint(
%%   +Diagnosis:diagnosis,
%%   +Component:component,
%%   +Environment:ord_set(environment),
%%   +ReducedEnvironment:ord_set(environment)
%% ) is det.
% Find a constraint, defined by a component, that allows a previously unknown
% value to be derived.
%
%   1. Take a component from the environment, call this =|COMPONENT|=.
%   2. Take the points that are directly connected to the component.
%   3. Take the constraint of the component.
%   4. For each of the directly connected points, call it =|UNKNOWN_POINT|=
%      and the others =|KNOWN_POINTS|=, then do:
%   5. Make sure the ATMS does not contain a justification for this point
%      already, i.e.
%      =|find_or_add_justification(
%          INFORMANT,
%          [COMP | KNOWN_POINTS, UNKNOWN_POINT]
%        )|=.
%   6. Make sure the point is not yet proven by this environment, i.e.
%      =|is_in_node(UNKNOWN_POINT, ENVIRONMENT)|=.
%   7. Derive the expression for the unknown point, based on the expressions
%      of the known points and the constraint of the component. Linking
%      =|UNKNOWN_POINT|= to the derived expression makes it
%      =|DERIVED_POINT|=.
%   8. =|add_node(ATMS, DERIVED_POINT, NEW_NODE),
%        find_or_add_justification(
%          INFORMANT,
%          [COMPONENT | KNOWN_POINTS],
%          DERIVED_POINT
%        )|=
%
% @param Diagnosis A diagnosis.
% @param Component The difference between =Environment= and =RestEnvironment=.
% @param Environment An environment. These are the assumptions
%        under which we are calculating the derived points.
% @param ReducedEnvironment The
%        assumptions from environment that are not used in the present
%        application of a behavior rule.

find_applicable_constraint(
  Diagnosis,
  ComponentCloud,
  Environment,
  ReducedEnvironment
):-
  % DEBUG
  component_cloud_id(ComponentCloud, ComponentCloudID),
  (
    debug_mode(true),
    debug(gde_pred, '  [BEHAVIOR] component cloud: ~w', [ComponentCloudID]),
    (
      % Trace on envronment ID.
      environment_id(Environment, 9999)
    ;
      % Trace on environment cardinality.
      environment_assumptions(Environment, Assumptions),
      length(Assumptions, 99999)
    ;
      % Trace on component cloud IDs.
      trace_environment([2, 3], Environment)
    ;
      % Trace on component cloud ID.
      ComponentCloudID = 9999
    )
  ->
    gtrace %WB
  ;
    true
  ),

  % Get the behavior rule for the component cloud.
  component_cloud_to_behavior_rule(ComponentCloud, BehaviorRule),
  component_cloud_to_point_specifications(ComponentCloud, PointSpecifications),
  % Backtrack moment I.
  nth0(
    Index,
    PointSpecifications,
    UnknownPointSpecification,
    KnownPointSpecifications
  ),

  % Backtrack moment II.
  component_cloud_component(ComponentCloud, Component),
  % We convert the known point specifications to known points here.
  findall(
    KnownPoint,
    (
      member(KnownPointSpecification, KnownPointSpecifications),
      point_specification_point(
        Component,
        KnownPointSpecification,
        ReducedEnvironment,
        KnownPoint
      )
    ),
    KnownPoints
  ),
  same_length(KnownPointSpecifications, KnownPoints),
  maplist(point, KnownExpressions, KnownPoints),
  % Reassemble.
  nth0(Index, Expressions, UnknownExpression, KnownExpressions),
  % Do the component-specific work.
  Call =.. [BehaviorRule | Expressions],
  call(Call),

  (
    debug_mode(true)
  ->
    maplist(expression_to_ccm_label, KnownExpressions, KnownExpressionLabels),
    expression_to_ccm_label(UnknownExpression, UnknownExpressionLabel),
    format(atom(UnknownExpressionLabel1), '* ~w', [UnknownExpressionLabel]),
    nth0(
      Index,
      ExpressionLabels,
      UnknownExpressionLabel1,
      KnownExpressionLabels
    ),
    atomic_list_concat(ExpressionLabels, '\n\t', ExpressionsLabel),
    debug(gde_beh, '[BEH] ~w:\n\t~w', [BehaviorRule, ExpressionsLabel])
  ;
    true
  ),
  point_specification_relation(
    UnknownPointSpecification,
    UnknownComponentRelation
  ),
  component_space(Component, UnknownComponentRelation, UnknownSpace),
  % Create the new point and link it to the existing point cloud.
  % Diagnosis never asserts new point clouds (repair does this).
  component_cloud_component_relation(
    UnknownComponentCloudRelation,
    UnknownComponentRelation
  ),
  component_cloud_point_cloud(
    ComponentCloud,
    UnknownComponentCloudRelation,
    UnknownPointCloud
  ),
  find_or_add_point_for_point_cloud(
    UnknownSpace,
    UnknownExpression,
    UnknownPointCloud,
    UnknownPoint
  ),
  % The new fact should not already be provable in this environment by the
  % ATMS. The unknown value should not already depend on the environment
  % assumptions.
  % Why? Because we are working with an environment and not an individual
  % component here. So we need to retract untill all components of the
  % environment have been used.
  diagnosis_atms(Diagnosis, ATMS),
  \+((
    node(ATMS, UnknownPoint, UnknownPointNode),
    is_in_node(UnknownPointNode, Environment)
  )),
  % Add the new derivation to the ATMS.
  component_cloud_label(ComponentCloud, Informant),
  find_or_add_node(ATMS, UnknownPoint, UnknownNode),
  maplist(find_or_add_node(ATMS), KnownPoints, KnownNodes),
  list_to_ord_set([ComponentCloud | KnownNodes], Antecedents),
  (
    justification(Informant, UnknownNode, Antecedents, _Justification1)
  ->
    true
  ;
    add_justification(Informant, UnknownNode, Antecedents, _Justification2),
    % Print the ATMS for debugging purposes.
    (
      debug_mode(true)
    ->
      environment_label(Environment, EnvironmentLabel),
      atms_to_nogoods(ATMS, Conflicts),
      format(
        atom(Explanation),
        'Environment: ~w ; Conflicts: ~w',
        [EnvironmentLabel, Conflicts]
      ),
      print_atms(ATMS, Explanation)
    ;
      true
    )
  ),

  point_id(UnknownPoint, UnknownPointID), %DEB
  maplist(point_id, KnownPoints, KnownPointIDs), %DEB
  atomic_list_concat(KnownPointIDs, ', ', KnownPointsID), %DEB
  debug(
    gde_justification,
    '    [JUSTIFICATION-ADDED] unknown: ~w, component cloud: ~w, known: ~w',
    [UnknownPointID, ComponentCloudID, KnownPointsID]
  ),

  % Check for conflicts.
  (
    conflicting_point_cloud(UnknownPointCloud, Point1, Point2),
    known_or_unknown(Diagnosis, Environment, Point1),
    known_or_unknown(Diagnosis, Environment, Point2)
  ->
    add_conflict(ATMS, Point1, Point2)
  ;
    true
  ).

trace_environment(ComponentCloudIDs, Environment):-
  environment_assumptions(Environment, Assumptions),
  length(ComponentCloudIDs, Length),
  length(Assumptions, Length),
  forall(
    member(ComponentCloudID, ComponentCloudIDs),
    (
      member(Assumption, Assumptions),
      component_cloud_id(Assumption, ComponentCloudID)
    )
  ).
