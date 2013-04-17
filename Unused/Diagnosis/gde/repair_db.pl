:- module(
  repair_db,
  [
    copy_node/2,
    add_node/1,
    delete_node/1,
    get_node_estimate/2,
    get_node_path/4,
    load_node/1,
    register_rdf_listeners/0,
    select_best_node/1,
    select_best_expansion/1,
    set_current_node/1,
    set_node_estimate/2,
    unregister_rdf_listeners/0
  ]
).

:- use_module(library(semweb/rdf_db)).
    
:- dynamic current_node/1, node_path/4.

apply_execute_list([]).
apply_execute_list([assert(S,P,O,G)|ExecuteList]):-
  !, rdf_assert(S,P,O,G),
  apply_execute_list(ExecuteList).

apply_execute_list([retract(S,P,O,G)|ExecuteList]):-
  !, rdf_retractall(S,P,O,G),
  apply_execute_list(ExecuteList).

apply_execute_list([_Elem|ExecuteList]):-
  % TODO expand if necessary
  apply_execute_list(ExecuteList).

apply_revert_list([]).
apply_revert_list([assert(S,P,O,G)|RevertList]):-
  rdf_retractall(S,P,O,G),
  apply_revert_list(RevertList).

apply_revert_list([retract(S,P,O,G)|RevertList]):-
  rdf_assert(S,P,O,G),
  apply_revert_list(RevertList).

apply_revert_list([_Elem|RevertList]):-
  % TODO expand if necessary
  apply_revert_list(RevertList).

add_node(Node):-
  gensym(node, Node),
  assert(node_path(Node,[],0,0)).

%% copy_node(+FromNode, -ToNode) is det
% Copies the path stored in FromNode to ToNode.
% ToNode is created using add_node/1
copy_node(FromNode, ToNode):-
  add_node(ToNode),
  node_path(FromNode, FromPath, Estimate, Cost),
  set_node_path(ToNode, FromPath, Estimate, Cost).

delete_node(Node):-
  \+ current_node(Node),
  retractall(node_path(Node, _,_,_)).

get_node_estimate(Node, Estimate):-
  node_path(Node, _Path, _Cost, Estimate).

get_node_path(Node, Path, Cost, Estimate):-
  node_path(Node, Path, Cost, Estimate).

%% load_node(+Node) is det
% Transform to the state of Node
load_node(NewNode):-
  current_node(CurrentNode),
  node_path(CurrentNode, CurrentPath, _Estimate, _Cost),
  node_path(NewNode, NewPath, _Estimate2, _Cost2),
  path_diff(CurrentPath, NewPath, RevertList, ExecuteList),
  reverse(ExecuteList, RevExecuteList),
  reverse(RevertList, RevRevertList),
  retractall(current_node(_)),
  rdf_transaction((
    apply_revert_list(RevRevertList),
    apply_execute_list(RevExecuteList)
  ), NewNode),
  assert(current_node(NewNode)).

path_diff(P, P, [], []):- !.
path_diff(P, [], P, []):- !.
path_diff([], P, [], P):- !.
path_diff(
  [ FromHead | FromPath ],
  [ ToHead | ToPath ],
  RevertList,
  ExecuteList
):-
  length(FromPath, FromPathLength),
  length(ToPath, ToPathLength),
  ( FromPathLength > ToPathLength ->
    path_diff(FromPath, [ToHead|ToPath], RevertListRest, ExecuteList),
    RevertList = [FromHead|RevertListRest]
  ; ToPathLength > FromPathLength ->
    path_diff([FromHead|FromPath], ToPath, RevertList, ExecuteListRest),
    ExecuteList = [ToHead|ExecuteListRest]
  ;
    path_diff(FromPath, ToPath, RevertListRest, ExecuteListRest),
    RevertList = [FromHead|RevertListRest],
    ExecuteList = [ToHead|ExecuteListRest]
  ).

register_rdf_listeners:-
  rdf_monitor(store_rdf_mutation, [+assert, +retract]).

select_best_node(Node):-
  node_path(Node, _Path, Estimate, Cost),
  (
    node_path(_Node2, _Path2, Estimate2, Cost2),
    Estimate2 + Cost2 < Estimate + Cost
  ->
    fail
  ;
    true
  ).

select_best_expansion(Expansion):-
  current_node(CurrentNode),
  select_best_node(Expansion),
  Expansion \= CurrentNode.

set_current_node(Node):-
  retractall(current_node(_)),
  assert(current_node(Node)).

set_node_estimate(Node, NewEstimate):-
  node_path(Node, Path, Cost, _Estimate),
  set_node_path(Node, Path, Cost, NewEstimate).

set_node_path(Node, Path, Estimate, Cost):-
  retractall(node_path(Node, _, _, _)),
  assert(node_path(Node, Path, Estimate, Cost)).

store_rdf_mutation(Mutation):-
  current_node(Node),
  node_path(Node, Path, Estimate, Cost),
  NewCost is Cost + 1,
  set_node_path(Node, [Mutation|Path], Estimate, NewCost).

unregister_rdf_listeners:-
  rdf_monitor(store_rdf_mutation, [-all]).
