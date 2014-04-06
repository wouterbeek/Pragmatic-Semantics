:- module(
  betweenness,
  [
    betweenness1/6, % +Graph:graph
                    % :V_P
                    % :E_P
                    % :N_P
                    % +Vertex:vertex
                    % -Betweenness:float
    betweenness2/5 % +Graph:graph
                   % :V_P
                   % :E_P
                   % :N_P
                   % SortedE_Sums:list
  ]
).

/** <module> BETWEENNESS

Predicates that calculate betweenness metrics.

@author Wouter Beek
@version 2013/01-2013/04, 2013/07, 2013/09
*/

:- use_module(generics(list_ext)).
:- use_module(graph_theory(shortest_path)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- meta_predicate(betweenness1(+,2,2,2,-)).
:- meta_predicate(betweenness1(+,2,2,2,+,-)).
:- meta_predicate(betweenness2(+,2,2,3,-)).

:- rdf_meta(betweenness1(+,:,:,:,-)).
:- rdf_meta(betweenness1(+,:,:,:,r,-)).



%! betweenness1(+Graph:atom, :V_P, :E_P, :N_P, -SortedPairs) is det.

betweenness1(G, V_P, E_P, N_P, SortedPairs):-
  call(V_P, G, Vs1),
  rdf_global_id(rdfs:'Class', RDFS_Class),
  once(select(RDFS_Class, Vs1, Vs2)),
  map_list_to_pairs(betweenness1(G, V_P, E_P, N_P), Vs2, Pairs1),
  call(E_P, G, Es),
  findall(
    Sum-V1/V2,
    (
      member(V1-V2, Es),
      member(X-V1, Pairs1),
      member(Y-V2, Pairs1),
      Sum is X + Y
    ),
    Pairs2
  ),
  sort([duplicates(true),inverted(true)], Pairs2, SortedPairs).

%! betweenness1(
%!   +Graph,
%!   :V_P,
%!   :E_P,
%!   :N_P,
%!   +Vertex:vertex,
%!   -Betweenness:float
%! ) is det.
% Betweenness centrality.

betweenness1(G, V_P, E_P, N_P, V, Betweenness):-
  call(V_P, G, Vs),
  findall(
    O,
    (
      member(From, To, Vs),
      shortest_paths1(UG, E_P, N_P, From, To, _, GenericShortestPaths),
      length(GenericShortestPaths, NumberOfGenericShortestPaths),
      shortest_paths1(UG, E_P, N_P, From, To, V, SpecificShortestPaths),
      length(SpecificShortestPaths, NumberOfSpecificShortestPaths),
      O is NumberOfSpecificShortestPaths / NumberOfGenericShortestPaths
    ),
    Os
  ),
  sum_list(Os, Betweenness).

betweenness2(G, V_P, E_P, N_P, SortedE_Sums):-
  call(V_P, G, Vs),
  call(E_P, G, Es),
  findall(
    V_Betweenness-V,
    (
      member(V, Vs),
      findall(
        V_Sum,
        (
          member(FromV, ToV, Vs),
          FromV \== ToV,
          shortest_paths2(G, N_P, FromV, ToV, _Cross, M-_),
          shortest_paths2(G, N_P, FromV, ToV, V, N-_),
          V_Sum is N / M
        ),
        V_Sums
      ),
      sum_list(V_Sums, V_Betweenness)
    ),
    Vs_Betweenness
  ),
  findall(
    E_Sum-V-W,
    (
      member(V-W, Es),
      member(V_Sum-V, Vs_Betweenness),
      member(W_Sum-W, Vs_Betweenness),
      E_Sum is V_Sum + W_Sum
    ),
    E_Sums
  ),
  predsort(icompare, E_Sums, SortedE_Sums).

