:- module(
  ragent_walker,
  [
    start_walker/1, % +Graph:atom
    walk_step/2 % +Graph:atom
                % +Ragent:ragent
  ]
).

/** <module> Ragent walker

Starts the walker for reasoning agents on a given graph.

This currently only works for one graph with one reasoning agent.

@author Wouter Beek
@version 2012/12-2013/01
*/

:- use_module(ragent(ragent)).



init_walker(Graph):-
  add_ragent(Graph, wouter).

start_walker(Graph):-
  init_walker(Graph),
  ragent(Ragent, wouter),
  walk(Graph, Ragent).

walk(_Graph, _Ragent):-
  flag(some_little_debug_flag, ID, ID + 1),
  ID > 3,
  !,
  flag(some_little_debug_flag, _OldID, 0).
walk(Graph, Ragent):-
  walk_step(Graph, Ragent),
  !,
  walk(Graph, Ragent).
% No traversal to a next location.
walk(_Graph, _Ragent).

walk_step(Graph, Ragent):-
  next_location(Graph, Ragent, Next),
  !,
  ragent(Ragent, Name),
  debug(ragent, '~w:~w has new location ~w', [Graph, Name, Next]).
