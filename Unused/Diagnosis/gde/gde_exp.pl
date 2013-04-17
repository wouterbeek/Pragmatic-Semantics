:- module(
  gde_exp,
  [
    expectation_ripple/2, % +Diagnosis:diagnosis
                          % -ExpectationRipple:compound
    expectation_ripples/2 % +Diagnosis:diagnosis
                          % -ExpectationRipples:list(compound)
  ]
).

/** <module> GDE EXP

Methods relating to expectations in the CCM.

@author Wouter Beek
@version Feb 2012, Apr 2012, Aug 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(meta_ext)).



%% expectation_ripple(
%%   +Diagnosis:diagnosis,
%%   -ExpectationRipple:compound
%% ) is det.
% Returns the riple of an expectation point of the given diagnosis.
%
% @param Diagnosis A diagnosis.
% @param ExpectationRipple A compound terms.
%        A compound consists of an expectation point,
%        then its directly related components,
%        then their directly related point clouds,
%        then their directly related components,
%        etc.
% @tbd Add a variant of cyclic_graph_traversal/3 for this.

expectation_ripple(Diagnosis, ExpectationPoint/ExpectationRipple):-
  expectation(Diagnosis, ExpectationPoint),
  point_cloud(ExpectationPoint, ExpectationPointCloud),
  p([ExpectationPointCloud], ExpectationRipple, [], []).

%% expectation_ripples(
%%   +Diagnosis:diagnosis,
%%   -ExpectationRipples:list(compound)
%% ) is det.
% Returns riples for all expectations of the given diagnosis.
%
% @param Diagnosis A diagnosis.
% @param ExpectationRipples A list of compound terms.
%        Each compound consists of an expectation point,
%        then its directly related components,
%        then their directly related point clouds,
%        then their directly related components,
%        etc.

expectation_ripples(Diagnosis, ExpectationRipples):-
  findall(
    ExpectationRipple,
    expectation_ripple(Diagnosis, ExpectationRipple),
    ExpectationRipples
  ).

%% p(
%%   +PointClouds:ord_set(point_cloud),
%%   -Riple:compound,
%%   +PointCloudHistory:ord_set(point_cloud),
%%   +ComponentCloudHistory:ord_set(component_cloud)
%% ) is det.
% From point clouds to directly related components, excluding those that
% have already been visited.
% Also updates the component history.
%
% @param PointClouds An ordered set of point clouds.
% @param Riple A compound term.
%        The term consists of the directly related components,
%        followed by their directly related point clouds,
%        etc.
% @param PointCloudHistory An ordered set of point clouds.
% @param ComponentHistory An ordered set of components.

p(PointClouds, [], _PointCloudHistory, _ComponentCloudHistory):-
  point_clouds_to_component_clouds_(PointClouds, []),
  !.
p(
  PointClouds,
  NewComponentClouds/Riple,
  PointCloudHistory,
  ComponentCloudHistory
):-
  point_clouds_to_component_clouds_(PointClouds, ComponentClouds),
  ord_subtract(ComponentClouds, ComponentCloudHistory, NewComponentClouds),
  ord_union(
    ComponentCloudHistory,
    NewComponentClouds,
    NewComponentCloudHistory
  ),
  q(NewComponentClouds, Riple, PointCloudHistory, NewComponentCloudHistory).

%% q(
%%   +ComponentClouds:ord_set(component_cloud),
%%   -Riple:compound,
%%   +PointCloudHistory:ord_set(point_cloud),
%%   +ComponentCloudHistory:ord_set(component_cloud)
%% ) is det.
% From components to directly related point clouds, excluding those that
% have alreay been visited.
%
% @param ComponentClouds An ordered set of component clouds.
% @param Riple A compound term.
%        The term consists of the directly related point clouds,
%        followed by their directly related components,
%        etc.
% @param PointCloudHistory An ordered set of point clouds.
% @param ComponentCloudHistory An ordered set of component clouds.

q(ComponentClouds, [], _PH, _CH):-
  component_clouds_to_point_clouds_(ComponentClouds, []),
  !.
q(ComponentClouds, PointClouds/Riple, PointCloudHistory, ComponentCloudHistory):-
  component_clouds_to_point_clouds_(ComponentClouds, PointClouds),
  ord_subtract(PointClouds, PointCloudHistory, NewPointClouds),
  ord_union(PointCloudHistory, NewPointClouds, NewPointCloudHistory),
  p(PointClouds, Riple, NewPointCloudHistory, ComponentCloudHistory).

component_clouds_to_point_clouds_(ComponentClouds, PointClouds):-
  maplist(component_cloud_to_point_clouds, ComponentClouds, PointClouds_),
  ord_union(PointClouds_, PointClouds).

point_clouds_to_component_clouds_(PointClouds, ComponentClouds):-
  maplist(point_cloud_to_component_clouds, PointClouds, ComponentClouds_),
  ord_union(ComponentClouds_, ComponentClouds).
