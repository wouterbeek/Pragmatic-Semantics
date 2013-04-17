:- module(
  ccm_conflict,
  [
% EXPRESSIONS
    conflicting_expressions/2, % ?Expression1:expression
                               % ?Expression2:expression

% EXPRESSION DEFINITIONS
    conflicting_expression_definitions/2, % +ExpressionDefinition1:expression_definition
                                          % +ExpressionDefinition2:expression_definition

% POINTS
    alternative_points/2, % ?Point1:point
                          % ?Point2:point
    conflicting_points/2, % ?Point1:point
                          % ?Point2:point
    shared_locale/2, % ?Point1:point
                     % ?Point2:point

% POINT CLOUDS
    add_conflict/3, % ?Agent:agent
                    % ?Point1:point
                    % ?Point2:point
    conflicting_point_cloud/3 % ?PointCloud:point_cloud
                              % ?Point1:point
                              % ?Point2:point
  ]
).

/** <module> CCM CONFLICT

The module that contains all methods that relate to conflicts in the CCM.
Conflicts can exist at the following levels:
- Expressions.
- Points
- Point clouds

In diagnosis, conflicting point clouds are called SYMPTOMS.

@author Wouter Beek
@version 2012/04
*/

:- use_module(atms(atms_db)).
:- use_module(ccm(ccm_api)).
:- use_module(library(semweb/rdf_db)).
:- use_module(qr(qr_api)).



% EXPRESSIONS %

%% conflicting_expressions(
%%   +Expression1:expression,
%%   +Expression2:expression
%% ) is semidet.
% Succeeds if the given expression conflict with one another.
% Expressions conflict if they only differ in one specific
% subsentential part.
% The subsentential part that causes conflicting expressions
% when changed are defined in the expression definitions.
%
% @param Expression1 An expression.
% @param Expression2 An expression.

conflicting_expressions(Expression, Expression):-
  nonvar(Expression),
  !,
  fail.
% Derivative value expressions.
conflicting_expressions(Expression1, Expression2):-
  derivative_quantity_value_expression(Expression1),
  derivative_quantity_value_expression(Expression2),
  Expression1 \== Expression2,
  from_quantity_expression(Quantity, Expression1),
  from_quantity_expression(Quantity, Expression2).
% Magnitude value expressions.
conflicting_expressions(Expression1, Expression2):-
  magnitude_quantity_value_expression(Expression1),
  magnitude_quantity_value_expression(Expression2),
  Expression1 \== Expression2,
  from_quantity_expression(Quantity, Expression1),
  from_quantity_expression(Quantity, Expression2).
% (q,qs) or quantity space expressions.
conflicting_expressions(Expression1, Expression2):-
  quantity_space_expression(Expression1),
  quantity_space_expression(Expression2),
  Expression1 \== Expression2,
  expression_from_argument(Expression1, Quantity),
  expression_from_argument(Expression2, Quantity).
% (q,q) inequality expressions.
% (p,p) inequality expressions.
% (c,c) inequality expressions.
% (q,c) inequality expressions.
% (p,c) inequality expressions.
% Support expressions.
conflicting_expressions(Expression1, Expression2):-
  expression(Expression1),
  expression(Expression2),
  Expression1 \== Expression2,
  expression_from_argument(Expression1, FromArgument),
  expression_from_argument(Expression2, FromArgument),
  expression_to_argument(Expression1, ToArgument),
  expression_to_argument(Expression2, ToArgument),
  rdfs_individual_of(Expression1, ExpressionDefinition1),
  rdfs_individual_of(Expression2, ExpressionDefinition2),
  conflicting_expression_definitions(
    ExpressionDefinition1,
    ExpressionDefinition2
  ).



% EXPRESSION DEFINITIONS %

%% conflicting_expression_definitions(
%%   ?ExpressionDefinition1:expression_definition,
%%   ?ExpressionDefinition2:expression_definition
%% ) is nondet.
% Pairs of conflicting expression definitions.
% Expression definitions have they intermediate conflicts definined
% inside their definitions.
%
% @param ExpressionDefinition1 An expression definition.
% @param ExpressionDefinition2 An expression definition.

conflicting_expression_definitions(
  ExpressionDefinition1,
  ExpressionDefinition2
):-
  rdf(
    ExpressionDefinition1,
    expression:has_conflicting_expression,
    ExpressionDefinition2,
    ccm
  ),
  !.
conflicting_expression_definitions(
  ExpressionDefinition1,
  ExpressionDefinition2
):-
  rdf(
    ExpressionDefinition2,
    expression:has_conflicting_expression,
    ExpressionDefinition1,
    ccm
  ),
  !.



% POINTS %

alternative_points(Point, Point).
alternative_points(Point1, Point2):-
  conflicting_points(Point1, Point2).

%% conflicting_points(?Point1:point, ?Point2:point) is nondet.
% Pairs of conflicting points. Points conflict if
%   1. they share the same location, and
%   2. They share the same space, and
%   3. their expressions conflict.
%
% @param Point1
% @param Point2

conflicting_points(Point1, Point2):-
  point(Expression1, Space, Point1),
  point(Expression2, Space, Point2),
  conflicting_expressions(Expression1, Expression2).

%% shared_locale(?Point1:point, ?Point2:point) is nondet.
% Pairs of points sharing the same location.
% The idea is that we can identify a shared locale before the points
% have a point cloud, so that we can use it to identify existing point
% clouds for newly added points.
%
% @param Point1 A point.
% @param Point2 A point.

shared_locale(Point1, Point2):-
  point(Expression1, Point1),
  point(Expression2, Point2),
  (
    Expression1 == Expression2
  ->
    true
  ;
    conflicting_expressions(Expression1, Expression2)
  ).



% POINT CLOUDS %

add_conflict(Agent, Point1, Point2):-
  nogood_nodes(Agent, conflict, [Point1, Point2]),
  point_id(Point1, Point1ID), %DEB
  point_id(Point2, Point2ID), %DEB
  debug(
    gde_conflict_low,
    '    [CONFLICT] Conflicting points ~w and ~w.',
    [Point1ID, Point2ID]
  ).

%% conflicting_point_cloud(
%%   ?PointCloud:point_cloud,
%%   ?Point1:point,
%%   ?Point2:point
%% ) is nondet.
% Conflicting point clouds are SYMPTOMS.
% A point cloud is conflicting if it contains at least two points
% that are conflicting.
% Since point clouds can only contain conflicting points,
% we know that all points are also conflicting.
%
% @param PointCloud A point cloud that is conflicting, i.e. a SYMPTOM.
% @param Point1 A point.
% @param Point2 A point.

conflicting_point_cloud(PointCloud, Point1, Point2):-
  point_cloud(_Expression1, Space, Point1, PointCloud),
  point_cloud(_Expression2, Space, Point2, PointCloud),
  % We do not need to check whether these points are conflicting,
  % since they must be by definition if they have the same space.
  Point1 \== Point2.
