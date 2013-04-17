:- module(
  ccm_db,
  [
% COMPONENTS
    add_component/6, % +ComponentCloud:component_cloud
                     % +Type:component_definition
                     % +Inputs:list(point)
                     % +Supports:list(point)
                     % +Outputs:list(point)
                     % -Component:component
    assert_component_subsumption/2, % +Subsumeds:list(component)
                                    % +Subsuming:component
    disable_component/1, % +Component:component

% COMPONENT CLOUDS
    add_component_cloud/6, % +Type:component_definition
                           % +Inputs:list(point)
                           % +Supports:list(point)
                           % +Outputs:list(point)
                           % -Component:component
                           % -ComponentCloud:component_cloud
    assert_component_cloud_subsumption/2, % +Subsumeds:list(component_cloud)
                                          % +Subsuming:component_cloud
    disable_component_cloud/1, % +ComponentCloud:component_cloud

% EXPRESSIONS
    add_expression/3, % +Property:expression_definition
                      % +Argument:uri
                      % -Expression:expression
    add_expression/4, % +FromArgument:uri
                      % +Relation:expression_definition
                      % +ToArgument:uri
                      % -Expression:expression

% POINTS
    add_point/3, % +Expression:expression
                 % +Space:space
                 % -Point:point
    add_point/4, % +Agent:agent
                 % +Expression:expression
                 % +Space:space
                 % -Point:point
    add_point_for_point_cloud/4, % +Space:space
                                 % +Expression:expression
                                 % +PointCloud:point_cloud
                                 % -Point:point
    remove_point/1, % +Point:point
    remove_point/3, % +Space:space
                    % +FromArgument:uri
                    % +ToArgument:uri

% POINT CLOUD
    add_point_cloud/1, % -PointCloud:point_cloud

% STATES %
    add_state/2, % +StateName:atom
                 % -State:state
    add_state/3, % +StateName:atom
                 % +Status:atom
                 % -State:state
    add_state/5, % +StateName:atom
                 % +Status:atom
                 % +FromStates:list(state)
                 % +ToStates:list(state)
                 % -State:state
    change_state_status/2, % +State:state
                           % +Status:atom

% STATE TRANSITIONS %
    add_state_transition/2, % +FromState:state
                            % +ToState:state
    add_state_transition/3, % +FromState:state
                            % +ToState:state
                            % -StateTransition:state_transition
    remove_state_transition/2, % +FromState:state
                               % +ToState:state

% SPACE TRANSITIONS %
    add_space_transition/3 % +FromSpace:space
                           % +ToSpace:space
                           % -SpaceTransition:space_transition
  ]
).

/** <module> The low level Component Connection Model Database.

This module is the database for the Component Connection Model.
Here are all the low level assertions of specific CCM elements made.
The generic CCM elements are asserted in module =ccm_kb=.
Methods that directly interface with the CCM DB belong here.

As long as the diagnosis is all fancy and the engine is all dusty and messed
up, we need to translate states into states, statements into statements, etc.
We perform these seemingly superfluous steps so that we can work with
long-term usable predicates, instead of having to design for the unaesthetic
quirks and particularities of legacy code.

There is a distinction between ICT programmers and AI programmers. It shows.

@author Wouter Beek
@version 2011/08-2012/05
*/

:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_api)).
:- use_module(generic(atom_ext), [replace/3 as atom_replace]).
:- use_module(generic(list_ext), [replace/3 as list_replace]).
:- use_module(ile(agent)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_build)).



% GENERAL %

% Until the brink of time this function will stay here and testify of the
% inefficiency and ineffectiveness of unartistic programming.

%% reset_ids is det.
% Resets all identifier generators for the various CCM element types.

reset_ids:-
  flag(component_cloud_id, _OldComponentCloudID, 0),
  flag(component_id, _OldComponentID, 0),
  flag(expression_id, _OldExpressionID, 0),
  flag(point_cloud_id, _OldPointCloudID, 0),
  flag(point_id, _OldPointID, 0),
  flag(space_id, _OldSpaceID, 3).



% COMPONENTS %

add_component(ComponentCloud, Type, Inputs, Supports, Outputs, Component):-
  flag(component_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(component:AtomicID, Component),
  rdf_assert(Component, rdf:type, Type, ccm),
  rdf_assert(Component, rdf:type, component:component, ccm),
  once(component_definition_label(Type, ComponentDefinitionLabel)),
  rdfs_assert_label(Component, ComponentDefinitionLabel, ccm),
  rdf_assert_datatype(Component, component:has_id, integer, ID, ccm),

  append([Inputs, Supports, Outputs], Points),
  findall(
    Relation/Point,
    (
      component_definition_point_specification(
        Type,
        Index,
        Relation,
        _PointSpecification
      ),
      nth0(Index, Points, Point)
    ),
    Pairs
  ),
  forall(
    member(Relation/Point, Pairs),
    rdf_assert(Component, Relation, Point, ccm)
  ),

  rdf_assert(ComponentCloud, component_cloud:has_component, Component, ccm),
  debug(
    ccm_db,
    '[ADDED COMPONENT] comp:~w\n  inputs:~w\n  supports:~w\n  outputs:~w',
    [Component, Inputs, Supports, Outputs]
  ).

assert_component_subsumption(Subsumeds, Subsuming):-
  assert_subsumption_(Subsumeds, component, Subsuming).

disable_component(Component):-
  rdf_assert(Component, rdf:type, component:disabled, ccm),
  rdf_retractall(Component, component:subsumes, _SubsumedComponent, ccm).



% COMPONENT CLOUDS %

%% add_component_cloud(
%%   +Type:component_definition,
%%   +Inputs:list(point),
%%   +Supports:list(point),
%%   +Outputs:list(point),
%%   -Component:component,
%%   -ComponentCloud:component_cloud
%% ) is det.
% Creates a new component with identifier component. The component will be of
% the given type, with the given input list, support list and output list.
%
% @param Type A component definition.
% @param Inputs A list of points. The order is relevant.
% @param Supports A list of points.
% @param Outputs A list of points. The order is relevant.
% @param Component The newly created component.

add_component_cloud(
  Type,
  Inputs,
  Supports,
  Outputs,
  Component,
  ComponentCloud
):-
  flag(component_cloud_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(component_cloud:AtomicID, ComponentCloud),
  once(component_definition_label(Type, ComponentDefinitionLabel)),
  rdf_assert(ComponentCloud, rdf:type, Type, ccm),
  rdf_assert(ComponentCloud, rdf:type, component_cloud:component_cloud, ccm),
  rdfs_assert_label(ComponentCloud, ComponentDefinitionLabel, ccm),
  rdf_assert_datatype(ComponentCloud, component_cloud:has_id, integer, ID, ccm),

  append([Inputs, Supports, Outputs], Points),
  findall(
    Relation/PointCloud,
    (
      component_definition_point_specification(
        Type,
        Index,
        Relation_,
        _PointSpecification
      ),
      nth0(Index, Points, Point),
      point_cloud(Point, PointCloud),
      rdf_global_id(component:RelationName, Relation_),
      rdf_global_id(component_cloud:RelationName, Relation)
    ),
    Pairs
  ),
  forall(
    member(Relation/PointCloud, Pairs),
    rdf_assert(ComponentCloud, Relation, PointCloud, ccm)
  ),

  add_component(ComponentCloud, Type, Inputs, Supports, Outputs, Component),
  debug(
    ccm_db,
    '[ADDED COMPONENT CLOUD] comp:~w\n  inputs:~w\n  supports:~w\n  outputs:~w',
    [Component, Inputs, Supports, Outputs]
  ).

assert_component_cloud_subsumption(Subsumeds, Subsuming):-
  assert_subsumption_(Subsumeds, component_cloud, Subsuming).

assert_subsumption_(Subsumeds, Namespace, Subsuming):-
  rdf_global_id(Namespace:subsumes, Relation),
  forall(
    member(Subsumed, Subsumeds),
    rdf_assert(Subsuming, Relation, Subsumed, ccm)
  ).

disable_component_cloud(ComponentCloud):-
  rdf_assert(ComponentCloud, rdf:type, component_cloud:disabled, ccm),
  rdf_retractall(
    ComponentCloud,
    component_cloud:subsumes,
    _SubsumedComponentCloud,
    ccm
  ).



% EXPRESSIONS %

%% add_expression(
%%    +Property:expression_definition,
%%    +Argument:uri,
%%    -Expression:expression
%% ) is det.
% Asserts new generic knowledge. The contents of the generic
% knowledge are Property(Argument).
%
% @param Property An expression definition.
% @param Argument The sole argument of the expression.
% @param Expression An expression.

add_expression(Property, Argument, Expression):-
  add_expression_(Property, Expression),

  % Assert the argument.
  rdf_assert(Expression, expression:has_argument, Argument, ccm).

%% add_expression(
%%    +FromArgument:uri,
%%    +Relation:expression_definition,
%%    +ToArgument:uri,
%%    -Expression:expression
%% ) is det.
% Asserts new generic knowledge. The contents of the generic
% knowledge are SupportRelation(FromArgument, ToArgument).
%
% @param FromArgument The first argument of the expression.
% @param Relation An expression definition.
% @param ToArgument The second argument of the expression.
% @param Expression An expression.

add_expression(FromArgument, Relation, ToArgument, Expression):-
  add_expression_(Relation, Expression),

  % Assert the arguments.
  rdf_assert(Expression, expression:has_from_argument, FromArgument, ccm),
  rdf_assert(Expression, expression:has_to_argument, ToArgument, ccm).

add_expression_(ExpressionDefinition, Expression):-
  % Create the resource.
  flag(expression_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(expression:AtomicID, Expression),

  % Assert the identifier.
  rdf_assert_datatype(Expression, expression:has_id, integer, ID, ccm),

  % Assert the instance/definition relation.
  rdf_assert(Expression, rdf:type, ExpressionDefinition, ccm),

  % Assert the natural language label.
  format(atom(ExpressionLabel), 'expression ~w', [AtomicID]),
  rdfs_assert_label(Expression, ExpressionLabel, ccm).



% POINT %

add_point(Expression, Space, Point):-
  add_point_(Expression, Point),
  % The newly added point is used to find an existing point cloud, if any,
  % containing related/conflicting points.
  find_or_add_point_cloud(Point, PointCloud),
  rdf_assert(PointCloud, point_cloud:has_point, Point, ccm),
  rdf_assert(Space, space:has_point, Point, ccm).

add_point(Agent, Expression, Space, Point):-
  add_point(Expression, Space, Point),
  add_belief(Agent, Point).

add_point_(Expression, Point):-
  flag(point_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(point:AtomicID, Point),

  % Assert the instance/definition relation.
  rdf_assert(Point, rdf:type, point:point, ccm),

  % Asser the identifier.
  rdf_assert_datatype(Point, point:has_id, integer, ID, ccm),

  % Assert the expression.
  rdf_assert(Expression, expression:has_point, Point, ccm),

  % Assert the label. This is the label from the associated expression.
  once(rdfs_label(Expression, ExpressionLabel)),
  rdfs_assert_label(Point, ExpressionLabel, ccm).

add_point_for_point_cloud(Space, Expression, PointCloud, Point):-
  add_point_(Expression, Point),
  rdf_assert(PointCloud, point_cloud:has_point, Point, ccm),
  rdf_assert(Space, space:has_point, Point, ccm).

remove_point(Point):-
  rdf_retractall(_Agent, agent:believes, Point, ccm),
  rdf_retractall(Point, rdf:type, point:point, ccm),
  rdf_retractall_datatype(Point, point:has_id, integer, _PointID, ccm),
  rdf_retractall(_Expression2, expression:has_point, Point, ccm),
  rdf_retractall_literal(Point, rdfs:label, _ExpressionLabel, ccm),
  rdf_retractall(_PointCloud1, point_cloud:has_point, Point, ccm).

remove_point(Space, FromArgument, ToArgument):-
  point(
    _Expression,
    FromArgument,
    _Relation,
    ToArgument,
    Space,
    Point
  ),
  remove_point(Point).



% POINT CLOUDS %

%% add_point_cloud(-PointCloud:point_cloud) is det.
% Adds a point cloud.
%
% @param PointCloud The newly created point cloud.

add_point_cloud(PointCloud):-
  flag(point_cloud_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(point_cloud:AtomicID, PointCloud),
  rdf_assert(PointCloud, rdf:type, point_cloud:point_cloud, ccm),
  rdfs_assert_label(PointCloud, AtomicID, ccm),
  rdf_assert_datatype(PointCloud, point_cloud:has_id, integer, ID, ccm).



% STATES %

%% add_state(+StateName:atom, -State:state) is det.
% Asserts to the deep representation that the given state exists.
% The directed from and to transitions are also asserted.
%
% @param StateName An atomic name of a state.
% @param State The identifiers of the asserted state.

add_state(StateName, State):-
  add_state(StateName, interpreted, State).

%% add_state(+StateName:atom, +Status:atom, -State:state) is det.
% Asserts to the deep representation that the given state exists.
% The directed from and to transitions are also asserted.
%
% @param StateName An atomic name of a state.
% @param Status The atomic status of the state. Possible values:
%        1. 'closed'
%        2. 'interpreted'
%        3. 'open'
%        4. 'ordered'
%        5. 'terminated'
% @param State The newly create state.

add_state(StateName, Status, State):-
  % Create the resource.
  flag(space_id, SpaceID, SpaceID + 1),
  atom_number(AtomicSpaceID, SpaceID),
  rdf_global_id(space:AtomicSpaceID, State),

  % Assert the instance/definition relationship.
  rdf_assert(State, rdf:type, space:state, ccm),

  % Assert the identifier.
  rdf_assert_datatype(State, space:has_id, integer, AtomicSpaceID, ccm),

  % Assert the engine name
  rdf_assert_datatype(State, space:has_engine_name, integer, StateName, ccm),

  % Assert the status of the state.
  rdf_assert_literal(State, space:has_status, Status, ccm),

  % Assert the natural language label.
  format(atom(StateLabel), 'state ~w', StateName),
  rdfs_assert_label(State, StateLabel, ccm),

  debug(ccm_db, 'Added state\t~w', [StateLabel]).

add_state(StateName, Status, FromStates, ToStates, State):-
  add_state(StateName, Status, State),
  forall(
    member(FromState, FromStates),
    add_state_transition(FromState, State)
  ),
  maplist(add_state_transition(State), ToStates).

%% change_state_status(+State:state, +Status:atom) is det.
% Changes the status of a state.
%
% @param State A state.
% @param Status The atomic status of the state. Possible values:
%        1. 'closed'
%        2. 'interpreted'
%        3. 'open'
%        4. 'ordered'
%        5. 'terminated'

change_state_status(State, Status):-
  rdf_retractall(State, space:has_status, _Status, ccm),
  rdf_assert_literal(State, space:has_status, Status, ccm).



% STATE TRANSITIONS %

add_state_transition(FromState, ToState):-
  add_state_transition(FromState, ToState, _StateTransition).

add_state_transition(FromState, ToState, StateTransition):-
  % Create the resource.
  flag(space_id, StateTransitionID, StateTransitionID + 1),
  atom_number(AtomicStateTransitionID, StateTransitionID),
  rdf_global_id(space:AtomicStateTransitionID, StateTransition),

  % Assert the instance/definition relationship.
  rdf_assert(StateTransition, rdf:type, space:state_transition, ccm),

  % Assert the identifier.
  rdf_assert_datatype(StateTransition, space:has_id, integer, AtomicStateTransitionID, ccm),

  % Assert the natural language label.
  state_name(FromState, FromStateName),
  state_name(ToState, ToStateName),
  format(
    atom(StateTransitionLabel),
    'state transtion from ~w to ~w',
    [FromStateName, ToStateName]
  ),
  rdfs_assert_label(StateTransition, StateTransitionLabel, ccm),

  % Assert the relations to the from and to spaces.
  rdf_assert(FromState, space:has_to_state, ToState, ccm),
  rdf_assert(FromState, space:has_to_space, StateTransition, ccm),
  rdf_assert(StateTransition, space:has_to_space, ToState, ccm).

%% remove_state_transition(+FromState:state, +ToState:state) is det.
% Removes the state transitions between the given states.

remove_state_transition(FromState, ToState):-
  state_transition(FromState, ToState, StateTransition),
  rdf_retractall(FromState, space:has_to_space, StateTransition, ccm),
  rdf_retractall(StateTransition, space:has_to_space, ToState, ccm).



% SPACE TRANSITIONS %

add_space_transition(FromSpace, ToSpace, SpaceTransition):-
  % Create the resource.
  flag(space_id, SpaceTransitionID, SpaceTransitionID + 1),
  atom_number(AtomicSpaceTransitionID, SpaceTransitionID),
  rdf_global_id(space:AtomicSpaceTransitionID, SpaceTransition),

  % Assert the instance/definition relationship.
  rdf_assert(SpaceTransition, rdf:type, space:space_transition, ccm),

  % Assert the identifier.
  rdf_assert_datatype(SpaceTransition, space:has_id, integer, AtomicSpaceTransitionID, ccm),

  % Assert the natural language label.
  space_label(FromSpace, FromSpaceLabel),
  space_label(ToSpace, ToSpaceLabel),
  format(
    atom(SpaceTransitionLabel),
    'space transtion from ~w to ~w',
    [FromSpaceLabel, ToSpaceLabel]
  ),
  rdfs_assert_label(SpaceTransition, SpaceTransitionLabel, ccm),

  % Assert the relations to the from and to spaces.
  rdf_assert(FromSpace, space:has_to_space, SpaceTransition, ccm),
  rdf_assert(SpaceTransition, space:has_to_space, ToSpace, ccm).
