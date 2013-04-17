:- module(
  ccm_build,
  [
% COMPONENTS
    find_or_add_component/6, % +ComponentDefinition:component_definition
                             % +InputPointClouds:list(atom/point)
                             % +Supports:list(point)
                             % +OutputPointClouds:list(atom/point)
                             % -Component:component
                             % -ComponentCloud:component_cloud

% CONDITIONAL
    find_or_add_conditional_expression/3, % +Condition:expression
                                          % +Consequence:expression
                                          % -Conditional:expression
    find_or_add_conditional_point/5, % +State:state
                                     % +Condition:expression
                                     % +Consequence:expression
                                     % -Conditional:expression
                                     % -Point:point

% EXPRESSIONS
    find_or_add_expression/3, % +Property:uri
                              % +Argument:uri
                              % -Expression:expression
    find_or_add_expression/4, % +FromArgument:uri
                              % +Relation:expression_definition
                              % +ToArgument:uri
                              % -Expression:expression

% POINTS
    find_or_add_point/2, % +Expression:expression
                         % -Point:point
    find_or_add_point/3, % +Space:space
                         % +Expression:expression
                         % -Point:point
    find_or_add_point/6, % +State:state
                         % +FromArgument:uri
                         % +Relation:expression_definition
                         % +ToArgument:uri
                         % -Expression:expression
                         % -Point:point
    find_or_add_point_for_point_cloud/4, % +Space:space
                                         % +Expression:expression
                                         % +PointCloud:point_cloud
                                         % -Point:point
    find_or_add_support_point/4, % +Space:space
                                 % +SupportExpression:expression
                                 % -SupportPoint:point
                                 % -SupportComponent:component
    find_or_add_support_point/7, % +Space:space
                                 % +FromArgument:uri
                                 % +Relation:uri
                                 % +ToArgument:uri
                                 % -SupportExpression:expression
                                 % -SupportPoint:point
                                 % -SupportComponent:component

% POINT CLOUDS
    find_or_add_point_cloud/2, % +Point:point
                               % -PointCloud:point_cloud

% STATES
    find_or_add_state/2 % +StateName:atom
                        % -State:state
  ]
).

/** <module> Component Connection Model build predicates

This module is the Database for the Component Connection Model.

@author Wouter Beek
@version 2011/08-2012/08
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_conflict)).
:- use_module(ccm(ccm_db)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdfs(rdfs_read)).

:- multifile(relation_category(_Relation, _Category)).



% COMPONENTS %

%% find_or_add_component(
%%   +ComponentDefinition:component_definition,
%%   +Inputs:list(atom/point),
%%   +Supports:list(atom/point),
%%   +Outputs:list(atom/point),
%%   -Component:component,
%%   -ComponentCloud:component_cloud
%% ) is det.

% The component already exists, and the component/point-relations
% already exist. So do nothing.
find_or_add_component(
  Type,
  Inputs,
  Supports,
  Outputs,
  Component,
  ComponentCloud
):-
  maplist(component_relation, Inputs,  Inputs_),
  maplist(component_relation, Supports,  Supports_),
  maplist(component_relation, Outputs,  Outputs_),
  component(Type, Inputs_, Supports_, Outputs_, Component),
  component_cloud_component(ComponentCloud, Component),
  !.
% The component already exists, but the component/point relations
% do not yet exists.
% 1. For within-state components: Take the existing component cloud
%    and add the new component/point-relations to it.
% 2. For transition components: Add a new component cloud as well as
%    a new component.
find_or_add_component(
  Type,
  Inputs,
  Supports,
  Outputs,
  Component,
  ComponentCloud
):-
  maplist(component_cloud_relation, Inputs, Inputs_),
  maplist(component_cloud_relation, Supports, Supports_),
  maplist(component_cloud_relation, Outputs, Outputs_),
  find_component_cloud(Type, Inputs_, Supports_, Outputs_, ComponentCloud_),
  !,
  maplist(no_relation, Inputs, Inputs__),
  maplist(no_relation, Supports, Supports__),
  maplist(no_relation, Outputs, Outputs__),
  (
    state_transition_component_definition(Type)
  ->
    add_component_cloud(
      Type,
      Inputs__,
      Supports__,
      Outputs__,
      Component,
      ComponentCloud
    )
  ;
    add_component(
      ComponentCloud_,
      Type,
      Inputs__,
      Supports__,
      Outputs__,
      Component
    ),
    ComponentCloud = ComponentCloud_
  ).
% The component nor the component cloud exists yet.
% Everything is created anew.
find_or_add_component(
  Type,
  Inputs,
  Supports,
  Outputs,
  Component,
  ComponentCloud
):-
  maplist(no_relation, Inputs, Inputs_),
  maplist(no_relation, Supports, Supports_),
  maplist(no_relation, Outputs, Outputs_),
  add_component_cloud(
    Type,
    Inputs_,
    Supports_,
    Outputs_,
    Component,
    ComponentCloud
  ).

component_relation(Atom/Point, ComponentRelation/Point):-
  rdf_global_id(component:Atom, ComponentRelation),
  !.
component_relation(ComponentRelation/Point, ComponentRelation/Point).

component_cloud_relation(Atom/Point, ComponentCloudRelation/PointCloud):-
  rdf_global_id(component_cloud:Atom, ComponentCloudRelation),
  !,
  point_cloud(Point, PointCloud).
component_cloud_relation(
  ComponentCloudRelation/Point,
  ComponentCloudRelation/PointCloud
):-
  point_cloud(Point, PointCloud).

no_relation(_Relation/Point, Point).



% CONDITIONAL %

find_or_add_conditional_expression(Condition, Consequence, Conditional):-
  find_or_add_expression(
    Condition,
    expression:conditional,
    Consequence,
    Conditional
  ).

find_or_add_conditional_point(
  State,
  Condition,
  Consequence,
  Conditional,
  Point
):-
  find_or_add_conditional_expression(Condition, Consequence, Conditional),
  add_point(Conditional, State, Point).



% EXPRESSIONS %

find_or_add_expression(Property, Argument, Expression):-
  expression(Property, Argument, Expression),
  !.
find_or_add_expression(Property, Argument, Expression):-
  add_expression(Property, Argument, Expression).

find_or_add_expression(FromArgument, Relation, ToArgument, Expression):-
  expression(FromArgument, Relation, ToArgument, Expression),
  !.
find_or_add_expression(FromArgument, Relation, ToArgument, Expression):-
  add_expression(FromArgument, Relation, ToArgument, Expression).



% POINTS %

find_or_add_point(Expression, Point):-
  global_space(GlobalSpace),
  find_or_add_point(GlobalSpace, Expression, Point).

%% find_or_add_point(
%%   +Space:space,
%%   +Expression:expression,
%%   -Point:point
%% ) is det.
% Creates a new point in the given space with the given expression.
% The created point is not returned.

find_or_add_point(Space, Expression, Point):-
  expression_from_argument(Expression, FromArgument),
  expression_to_argument(Expression, ToArgument),
  expression_definition_expression(Relation, Expression),
  find_or_add_point(
    Space,
    FromArgument,
    Relation,
    ToArgument,
    Expression,
    Point
  ).

%% find_or_add_point(
%%   +Space:space,
%%   +FromArgument:uri,
%%   +Relation:expression_definition,
%%   +ToArgument:uri,
%%   -Expression:expression,
%%   -Point:point
%% ) is det.
%   1. If an expression exists and it is the exact expression we
%      want to add, then add the point.
%   2. If an expression exists that is more generic than the new
%      expression, then first check if it is also in the given space,
%      then if it is, remove the existing point and add the new one.
%   3. If an expression exists that is more specific than the new
%      expression, then first check if it is also a point in the
%      given space, then if it is, we are done; if it is not, add
%      the new point.
%   4. If there is no existing expression, then just add the new one.

find_or_add_point(
  Space,
  FromArgument,
  Relation,
  ToArgument,
  Expression,
  Point
):-
  % A similar point already exists. Now we have to inquire whether
  % this existing point is more/less generic than the point we want
  % to add. Some examples: Adding Q>=0 against Q>0 succeeds with no
  % side effect whatsoever. Adding Q>0 against Q>=0 should not only
  % add the point for Q>0 but also remove the point for Q>=0.
  point(
    ExistingExpression,
    FromArgument,
    ExistingRelation,
    ToArgument,
    Space,
    ExistingPoint
  ),
  % They are in the same ballpark.
  similar_relations(ExistingRelation, Relation),
  % Since we assume CCM DB integrity, we know that there is at most
  % one such point (given the additional argument instantiation
  % assumptions for this method this is obviously the case).
  !,
  find_or_add_point_(
    Space,
    FromArgument,
    Relation,
    ExistingRelation,
    ToArgument,
    Expression,
    ExistingExpression,
    Point,
    ExistingPoint
  ).
find_or_add_point(
  Space,
  FromArgument,
  Relation,
  ToArgument,
  Expression,
  Point
):-
  find_or_add_expression(FromArgument, Relation, ToArgument, Expression),
  add_point(Expression, Space, Point).

% If the existing point is the same or more specific, then we are done.
find_or_add_point_(
  _Space,
  _FromArgument,
  Relation,
  ExistingRelation,
  _ToArgument,
  ExistingExpression,
  ExistingExpression,
  ExistingPoint,
  ExistingPoint
):-
  rdfs_subclass_of(ExistingRelation, Relation),
  !.
% If the existing point is more generic, then remove this existing point
% and add a new point.
find_or_add_point_(
  Space,
  FromArgument,
  Relation,
  _ExistingRelation,
  ToArgument,
  Expression,
  _ExistingExpression,
  Point,
  ExistingPoint
):-
  remove_point(ExistingPoint),
  find_or_add_expression(FromArgument, Relation, ToArgument, Expression),
  add_point(Expression, Space, Point).

%% find_or_add_point_for_point_cloud(
%%   +Space:space,
%%   +Expression:expression,
%%   +PointCloud:point_cloud,
%%   -Point:point
%% ) is det.
% Creates a new point with the given parameters. Retruns the newly created
% point.
% This comes in handy when the point cloud already exists, and an alternative
% point is added, e.g. in the diagnosis diaglog.
%
% @param Space The space in which the new point is created.
% @param Expression The expression that the new point is annotated with.
% @param InputComponents A list of URIs of components.
% @param OutComponent A list of URIs of components.
% @param Point The URI of a point.

% Inside the given point cloud there already exists a point with
% the given expression.
find_or_add_point_for_point_cloud(Space, Expression, PointCloud, Point):-
  point_cloud(Expression, Space, Point, PointCloud),
  !.
find_or_add_point_for_point_cloud(Space, Expression, PointCloud, Point):-
  add_point_for_point_cloud(Space, Expression, PointCloud, Point).

find_or_add_support_point(
  Space,
  SupportExpression,
  SupportPoint,
  SupportComponent
):-
  global_space(GlobalSpace),
  find_or_add_point(
    GlobalSpace,
    SupportExpression,
    GenericPoint
  ),
  find_or_add_point(
    Space,
    SupportExpression,
    SupportPoint
  ),
  find_or_add_component(
    component:retrieval,
    [has_support_input/GenericPoint],
    [],
    [has_support_output/SupportPoint],
    SupportComponent,
    _SupportComponentCloud
  ).

find_or_add_support_point(
  Space,
  FromArgument,
  Relation,
  ToArgument,
  SupportExpression,
  SupportPoint,
  SupportComponent
):-
  find_or_add_expression(
    FromArgument,
    Relation,
    ToArgument,
    SupportExpression
  ),
  find_or_add_support_point(
    Space,
    SupportExpression,
    SupportPoint,
    SupportComponent
  ).

similar_relations(Relation1, Relation2):-
  relation_category(Relation1, RelationCategory),
  relation_category(Relation2, RelationCategory),
  !.
similar_relations(Relation, Relation).



% POINT CLOUDS %

find_or_add_point_cloud(Point, PointCloud):-
  point_cloud(PointCloudPoint, PointCloud),
  shared_locale(PointCloudPoint, Point),
  !.
find_or_add_point_cloud(_Point, PointCloud):-
  add_point_cloud(PointCloud).



% STATE %

find_or_add_state(StateName, State):-
  state_name(State, StateName),
  !.
find_or_add_state(StateName, State):-
  add_state(StateName, State).
