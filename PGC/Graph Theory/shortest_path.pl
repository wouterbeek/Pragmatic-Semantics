:- module(
  shortest_path,
  [
    shortest_paths1/7, % +Graph:graph
                       % :E_P
                       % :N_P
                       % +FromV:vertex
                       % +ToV:vertex
                       % +ViaV:vertex
                       % -ShortestPaths:list(list(vertex))
    shortest_paths2/6 % +Graph:graph
                      % :N_P
                      % +FromV:vertex
                      % +ToV:vertex
                      % +ViaV:vertex
                      % -ShortestLengthPathPair
  ]
).

:- use_module(graph_theory(graph_traversal)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- meta_predicate(shortest_paths1(+,2,2,+,+,?,-)).
:- meta_predicate(shortest_paths2(+,3,+,+,+,-)).

:- rdf_meta(shortest_paths1(+,:,:,r,r,r,-)).
:- rdf_meta(shortest_paths2(+,:,r,r,r,-)).



%! shortest_paths1(
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +From:vertex,
%!   +To:vertex,
%!   ?Pass:vertex,
%!   -ShortestPaths:list(path)
%! ) is det.
% Returns the shortest paths in graph Graph from vertex `From` to vertex `To`,
% passing vertex `Pass`.
%
% @arg Pass This one is optional. "Wir haben ein Abstich gemacht."

shortest_paths1(G, E_P, N_P, From, To, Pass, ShortestPaths):-
  aggregate_all(
    set(Length-Path),
    (
      traverse(
        [unique_vertex(true)],
        G,
        E_P,
        N_P,
        From,
        To,
        Length,
        Vs,
        _Es,
        Path
      ),
      member(Pass, Vs)
    ),
    KeyValuePairs
  ),
  group_pairs_by_key(KeyValuePairs, Joined),
  (
    Joined == []
  ->
    ShortestPaths = []
  ;
    Joined = [_ShortestDistance-ShortestPaths | _]
  ).

shortest_paths2(G, N_P, FromV, ToV, ViaV, ShortestLength-ShortestPath):-
  % @tbd Why sorted?
  aggregate_all(
    set(Length-Path),
    (
      traverse(G, N_P, FromV, ToV, Length, Vs, Path),
      member(ViaV, Vs)
    ),
    Pairs
  ),
  group_pairs_by_key(Pairs, Sorted),
  (
    Sorted == []
  ->
    ShortestLength = 0,
    ShortestPath = []
  ;
    Sorted = [ShortestLength-ShortestPath | _]
  ).

