:- module(
  repair,
  [
    repair/1 % -RepairPath
  ]
).

/** <module> REPAIR

@author Wouter Beek, Sander Latour
@version Apr 2012 - May 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_export)).
:- use_module(gde(gde_exp)).
:- use_module(gde(missing_component)).
:- use_module(gde(repair_db)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).



debug_repair_node(
  Node,
  _ExpectationPoint,
  CD, 
  InputPointTuple, 
  SupportExpression
):-
  component_definition_label(CD, CDLabel),
  (
    expression(SupportExpression)
  ->
    expression_to_ccm_label(SupportExpression, Label),
  ;
    InputPointTuple = [InputPoint|_],
    point_to_label(InputPoint, Label)
  ),
  debug(missing, '[~w] Add a ~w {~w}', [Node, CDLabel, Label]).

%% evaluate_current_node(-Estimate)
% Returns the estimated costs to be made
% to go from the current node to the goal.
%
% At the moment the estimate is always 1, 
% unless a goal node is reached.
% TODO: Implement a better evaluation. %SL
evaluate_current_node(0):- goal_reached, !.
evaluate_current_node(1).

missing_component_definitions(MissingComponentDefinitions):-
  setoff(
    MissingComponentDefinition,
    missing_component:missing_component_definition(MissingComponentDefinition),
    MissingComponentDefinitions
  ).

%% expand is det.
% Expand current node into all possible next nodes.

expand(CurrentNode):-
  % Retrieve the expectation expression.
  once(diagnosis(Diagnosis)),
  expectation(Diagnosis, ExpectationPoint),
  missing_component_definitions(CDs),

  % Gather component definitions and expressions.
  % TODO: This is probably not good enough for more
  % complicated problems, since this only looks at 
  % components that are directly linked to one of the
  % expectation points. But it is possible that somewhere
  % else in the model also something needs to be added.
  findall(
    CD/InputPointTuple/SupportPoint/Relevance,
    (
      member(CD, CDs),
      % TODO: missing_component1 does not consider 
      % constraints on the spaces in which the points
      % must lie. %SL
      missing_component:missing_component1(
        ExpectationPoint,
        CD,
        InputPointTuple,
        SupportPoint,
        Relevance
      )
    ),
    RepairCandidates
  ),
  forall(
    member(CD/InputPointTuple/SupportPoint/Relevance, RepairCandidates),
    (
      % Create a new node to store the changes in
      copy_node(CurrentNode, NewNode),
      set_current_node(NewNode),

      % Add component.
      %% TODO:
      % CD -> within_state component? dan State = state van CD
      % is CD -> transition component, als exp input is dan state daarna,
      % anders daarvoor.
      space_point(Space, ExpectationPoint),
      maplist(
        point_cloud,
        [ExpectationPoint | InputPointTuple],
        [OutputPointCloud | InputPointClouds]
      ),
      debug_repair_node(
        NewNode,
        ExpectationPoint,
        CD,
        InputPointTuple,
        SupportPoint
      ),
      find_or_add_component(
        CD,
        InputPointClouds,
        [has_support/SupportPoint],
        OutputPointCloud,
        _Component,
        _ComponentCloud
      ),
      
      format(atom(File), 'missing_~w', [NewNode]),
      write_ccm_to_dot(File),

      %% TODO:
      % Calculate the heuristic to estimate
      % to distance from NewNode to a goal node
      % and store in NewEstimate.

      % Update node.
      evaluate_current_node(NewEstimate),
      set_node_estimate(NewNode, NewEstimate),

      % Reload the current node.
      load_node(CurrentNode)
    )
  ).

%% goal_reached is semidet.
% Returns whether all expectations are met in the current CCM.

goal_reached:-
  once(diagnosis(Diagnosis)),
  forall(
    expectation(Diagnosis, ExpectationPoint),
    \+(isolated_point(ExpectationPoint))
  ).

repair(RepairPath):-
  add_node(StartNode),
  set_current_node(StartNode),
  evaluate_current_node(NewEstimate),
  set_node_estimate(StartNode, NewEstimate),
  register_rdf_listeners,
  search(StartNode, RepairPath),
  unregister_rdf_listeners.

search(Node, RepairPath):-
  expand(Node),
  select_best_expansion(BestNode),
  (
    % If goal
    get_node_estimate(BestNode, 0)
  ->
    get_node_path(BestNode, RepairPath, _, _)
  ;
    load_node(BestNode),
    delete_node(Node),
    search(BestNode, RepairPath)
  ).

