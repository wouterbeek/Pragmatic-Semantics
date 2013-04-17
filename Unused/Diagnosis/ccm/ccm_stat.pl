:- module(
  ccm_stat,
  [
    count_components/1, % -ComponentCount:int
    count_components/2, % +Space:space
                        % -ComponentCount:int
    count_points/1, % -PointCount:int
    count_points/2, % +Space:space
                    % -PointCount:int
    count_spaces/1, % -SpaceCount:int
    count_state_transitions/1, % -StateTransitionCount:int
    count_states/1, % -StateCount:int
    print_counts/0
  ]
).

/** <module> The statistics module for the CCM.

This module contains predicates that keep track of various statistical
properties of a CCM.

No method in this module every changes anything in a CCM.

@author Wouter Beek
@version Dec 2011
*/

:- use_module(ccm(ccm_api)).


%% count_components(-AllComponentCount:number) is det.
% Returns the number of components.
%
% @param AllComponentCount A number greater than or equal to zero.

count_components(AllComponentCount):-
  components(AllComponents),
  length(AllComponents, AllComponentCount).

%% count_components(+Space:uri, -ComponentCount:int) is det.
% Returns the number of components in the given space.
%
% @param Space The URI of a space.
% @param ComponentCount A positive integer or zero.

count_components(Space, ComponentCount):-
  setoff(
    Component,
    space_component(Space, Component),
    Components
  ),
  length(Components, ComponentCount).

count_points(PointCount):-
  points(Points),
  length(Points, PointCount).

count_points(Space, PointCount):-
  space_to_points(Space, Points),
  length(Points, PointCount).

count_spaces(SpaceCount):-
  spaces(Spaces),
  length(Spaces, SpaceCount).

count_state_transitions(StateTransitionCount):-
  state_transitions(StateTransitions),
  length(StateTransitions, StateTransitionCount).

count_states(StateCount):-
  states(States),
  length(States, StateCount).

print_counts:-
  count_components(ComponentCount),
  count_points(PointCount),
  count_spaces(SpaceCount),
  count_states(StateCount),
  count_state_transitions(StateTransitionCount),
  format(
    'Components: ~w\nPoints: ~w\nSpaces: ~w\n\tStates: ~w\n\tState transitions: ~w\n',
    [ComponentCount, PointCount, SpaceCount, StateCount, StateTransitionCount]
  ).
