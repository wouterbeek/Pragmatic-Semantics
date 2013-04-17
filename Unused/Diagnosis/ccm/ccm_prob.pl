:- module(
  ccm_prob,
  [
    component_definition_to_probability/2, % +ComponentDefinition:component_definition
                                           % -Probability:float
    component_cloud_to_probability/2, % +ComponentCloud:component_cloud
                                      % -Probability:float
    expression_definition_to_probability/2, % +ExpressionDefinition:expression_definition
                                            % -Probability:float
    expression_to_probability/2, % +Expression:expression
                                 % -Probability:float
    point_cloud_to_probability/2, % +PointCloud:point_cloud
                                  % -Probability:float
    set_expression_probability/2 % +Expression:expression
                                 % +Probability:float
  ]
).

/** <module> The probabilties for CCM components.

This module handles the probabilistic information for the CCM.

@author Wouter Beek
@version 2011/12-2012/03
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_unpack)).
:- use_module(generic(math_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_build)).



%% minimum_component_probability(-Probability:float) is det.
% Returns the minimum probability for a component.
% This is used if there is no other wat of calculating the probability.
%
% @param Probability A floating-point number between 0.0 and 1.0.

minimum_component_probability(0.01).



%% component_definition_to_probability(
%%    +ComponentDefinition:component_definition,
%%    -Probability:float
%% ) is det.
% Returns the probability of the given component definition.
%
% @param ComponentDefinition The URI of a component definition.
% @param Probability A floating-point number between 0.0 and 0.1.

component_definition_to_probability(ComponentDefinition, Probability):-
  rdf_datatype(ComponentDefinition, component:has_probability, float, Probability, ccm),
  % A component definition has exactly one probability.
  !.
component_definition_to_probability(_ComponentDefinition, Probability):-
  minimum_component_probability(Probability),
  % A component definition has exactly one probability.
  !.

%% component_cloud_to_probability(
%%   +ComponentCloud:component_cloud,
%%   -Probability:float
%% ) is det.
% Returns the probability of the given component cloud.
%
% @param ComponentCloud A component cloud.
% @param Probability A float representing probability between 0 and 1.

component_cloud_to_probability(ComponentCloud, 0):-
  \+(active_component_cloud(ComponentCloud)),
  !.
component_cloud_to_probability(ComponentCloud, Probability):-
  aggregate_component_cloud(ComponentCloud),
  !,
  component_cloud_to_subsumed_component_clouds(
    ComponentCloud,
    SubsumedComponentClouds
  ),
  maplist(
    component_cloud_to_probability,
    SubsumedComponentClouds,
    Probabilities
  ),
  sum_list(Probabilities, Probability).
component_cloud_to_probability(ComponentCloud, Probability):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_definition_to_probability(ComponentDefinition, Probability).

expression_definition_to_probability(
  ExpressionDefinition,
  PointCloudProbability
):-
  rdf_datatype(ExpressionDefinition, expression:has_probability, float, PointCloudProbability, ccm).

expression_to_probability(Expression, ExpressionProbability):-
  rdfs_individual_of(Expression, ExpressionDefinition),
  expression_definition_to_probability(
    ExpressionDefinition,
    ExpressionProbability
  ).

% The expressions of a point cloud all have the same probability.
point_cloud_to_probability(PointCloud, PointCloudProbability):-
  once(expression_point_cloud(Expression, PointCloud)),
  expression_to_probability(Expression, PointCloudProbability).

set_expression_probability(Type, Probability):-
  set_expression_probability(Type, Probability, _OldProbability).

set_expression_probability(Type, Probability, OldProbability):-
  rdf_retractall(Type, expression:has_probability, OldProbability, ccm),
  rdf_assert_datatype(Type, expression:has_probability, float, Probability, ccm).
