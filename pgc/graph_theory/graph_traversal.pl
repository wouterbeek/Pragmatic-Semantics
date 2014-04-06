:- module(
  graph_travel,
  [
    traverse/7, % +Options:list(nvpair)
                % +Graph
                % :E_P
                % :N_P
                % +First:vertex
                % +Last:vertex
                % -Distance:integer
    traverse/10, % +Options:list(nvpair)
                 % +Graph
                 % :E_P
                 % :N_P
                 % +First:vertex
                 % +Last:vertex
                 % -Distance:integer
                 % -Vertices:ordset(vertex)
                 % -Edges:ordset(edge)
                 % -History:list
    travel_min/7, % +Options:list(nvpair)
                  % +Graph
                  % :E_P
                  % :N_P
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
    travel_min/10 % +Options:list(nvpair)
                  % +Graph
                  % :E_P
                  % :N_P
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
                  % -Vertices:ordset(vertex)
                  % -Edges:ordset(edge)
                  % -History:list
  ]
).

/** <module> Graph traversal

@author Wouter Beek
@version 2013/01-2013/04, 2013/07, 2013/09, 2013/12, 2014/03
*/

:- use_remote_module(generics(list_ext)).
:- use_remote_module(generics(option_ext)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % RDF meta predicate declarations.

:- meta_predicate(traverse(+,+,2,3,+,+,-)).
:- meta_predicate(traverse(+,+,2,3,+,+,-,-,-,-)).
:- meta_predicate(traverse1(+,+,2,3,+,+,-,-,-,-,-,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-,-,-,-)).

% Graph traversal can operate on abbreviated URIs.
:- rdf_meta(traverse(+,+,:,:,r,r,-)).
:- rdf_meta(traverse(+,+,:,:,r,r,-,-,-,-)).
:- rdf_meta(traverse1(+,+,:,:,r,r,-,-,-,-,-,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-,-,-,-)).

% Meta-option.
is_meta(deb_vertex_name).



%! traverse(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   :E_P,
%!   :N_P,
%!   +FirstV:vertex,
%!   +LastV:vertex,
%!   -Distance:nonneg
%! ) is nondet.
% @see Wrapper around traverse/10.

traverse(O, G, E_P, N_P, First, Last, Distance):-
  traverse(O, G, E_P, N_P, First, Last, Distance, _Vertexs, _Edges, _History).

%! traverse(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -Distance:nonneg
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge),
%!   -History:list
%! ) is nondet.
% Lets travel through graph land.
%
% A *walk* is an alternating sequence of vertices and edges,
% ending in a vertex.
%
% A *tour* is a closed walk. Closed means that the first and the last element
% in the sequence are the same vertex.
% ~~~{.pl}
% Options = [closed(true)]
% ~~~
%
% A *trail* is a walk with unique edges.
% ~~~{.pl}
% Options = [unique_edge(true)]
% ~~~
%
% A *path* is a walk with unique vertices.
% ~~~{.pl}
% Options = [unique_vertex(true)]
% ~~~
%
% A *cycle* is a closed path trail.
% ~~~{.pl}
% Options = [closed(true),unique_edge(true),unique_vertex(true)]
% ~~~
%
% An *Euler tour* is a tour in which all edges are traversed exactly one.
% ~~~{.pl}
% Options = [closed(true),every_edge(true),unique_edge(true)]
% ~~~
%
% The following options are defined:
%   1. =|closed(boolean)|=
%   2. =|distance(oneof([edge,vertex]))|=
%      For statiscs we return either
%      the number of edges (value `edges`; the default value)
%      or the number of vertices (value `vertices`) that were traversed.
%   3. =|euler(boolean)|=
%   4. =|every_edge(boolean)|=
%   5. =|every_vertex(boolean)|=
%   6. =|graph(Graph)|=
%   7. =|unique_edge(boolean)=
%   8. =|unique_vertex(boolean)=
%   9. =|deb_vertex_name(:VertexNaming)|=
%      Debugging option for assigning readable names to vertices.
%      The predicate should have the arguments
%      =|(+Vertex,-VertexName:atom)|=.
%
% @arg Options A list of name-value pairs.
% @arg Graph
% @arg E_P Maps a graph to its edges.
% @arg N_P Maps a vertex to its neighbor vertices.
% @arg First The first vertex in the path.
% @arg Last The last vertex in the path.
% @arg Distance An integer representing a distance between the first and
%      the last vertex, counted as the number of traversed edges.
% @arg Vertices A list of vertices.
% @arg Edges A list of edges.
% @arg History

traverse(O1, G, E_P, N_P, First, Last, Distance, Vertices, Edges, History):-
  % First we make sure we have the right options set,
  % so we do not have to do this in every iteration.
  
  % Cycles, tours, and Euler tours are closed.
  add_default_option(O1, closed, false, O2),
  
  % Euler tours must use every edge.
  add_default_option(O2, every_edge, false, O3),
  
  % The default distance metric is edges.
  add_default_option(O3, distance, edge, O4),
  
  % Cycles and paths have a unique number of vertices.
  add_default_option(O4, unique_vertex, false, O5),
  
  % Trails and Euler tours has a no duplicate edges.
  add_default_option(O5, unique_edge, false, O6),
  
  meta_options(is_meta, O6, O7),
  
  traverse1(
    O7, G, E_P, N_P, First, Last,
    Distance, [First], Vertices, [], Edges, History
  ).

% Done, perform some option-dependent checks
% and calculate some option-dependent statistics.
traverse1(
  O, G, E_P, _N_P, Last, Last, Distance, SolV, SolV, SolE, SolE, [Last]
):-
  % In a closed walk (or tour) the first and the last vertex must be
  % the same.
  (option(closed(true), O) -> last(SolV, Last) ; true),
  
  % In an Euler tour all edges must be visited.
  (
    option(every_edge(true), O)
  ->
    call(E_P, G, AllEs),
    ord_subtract(AllEs, SolE, UntraversedEdges),
    ord_empty(UntraversedEdges)
  ;
    true
  ),

  % Distance metric. The statistics we return.
  (
    option(distance(edge), O)
  ->
    length(SolE, Distance)
  ;
    length(SolV, Distance)
  ).
% Recursion: traversal by visiting neighboring vertices.
traverse1(
  O, G, E_P, N_P, FirstV, Last,
  Distance, Vs, SolV, Es, SolE, [FirstV,FirstV-NextV|History]
):-
  % Neighbor
  call(N_P, G, FirstV, NextV),
  
  % Debugging.
  (
    debugging(graph_traversal),
    option(deb_vertex_name(V_Name), O)
  ->
    call(V_Name, FirstV, FirstV_Name),
    call(V_Name, NextV, NextV_Name),
    debug(graph_travel, '~w\t--->\t~w', [FirstV_Name,NextV_Name])
  ;
    true
  ),
  
  % Check the walk restriction: no duplicate vertices.
  (option(unique_vertex(true), O) -> \+ member(NextV, Vs) ; true),
  
  % Check the trail and Euler tour restriction: no duplicate edges.
  (option(unique_edge(true), O) -> \+ member(FirstV-NextV, Es) ; true),
  
  ord_add_element(Vs, NextV, NewVs),
  ord_add_element(Es, FirstV-NextV, NewEs),
  traverse1(
    O, G, E_P, N_P, NextV, Last, Distance, NewVs, SolV, NewEs, SolE, History
  ).

/*
travel2(G, N_P, FromV, ToV, Length, AllVs, Path):-
  travel2(G, N_P, FromV, ToV, Length, [FromV], AllVs, Path).

travel2(_G, _N_P, ToV, ToV, Length, AllVs, AllVs, [ToV]):-
  length(AllVs, Length), !.
travel2(G, N_P, FromV, ToV, Length, Vs, AllVs, [FromV | Path]):-
  call(N_P, FromV, G, ToVs),
  member(ViaV, ToVs),
  % Make sure we haven't be here yet.
  \+ member(ViaV, Vs),
  ord_add_element(Vs, ToV, NewVs),
  travel2(G, N_P, ViaV, ToV, Length, NewVs, AllVs, Path).
*/

%! tarvel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -MinimumDistance:integer
%! ) is det.
% @see Wrapper around travel_min/10.

travel_min(O, G, E_P, N_P, First, Last, MinimumDistance):-
  travel_min(O, G, E_P, N_P, First, Last, MinimumDistance, _Vs, _Es, _History).

%! travel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -MinimumDistance:integer,
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge),
%!   -History:list
%! ) is det.
% Returns the minimum distance between the given subject and predicate terms.
%
% @arg Options A list of name-value pairs.
%      See travel/7 for the list of supported options.
% @arg Graph
% @arg E_P
% @arg N_P
% @arg First A vertex, the first in the travel.
% @arg Last A respource, the last in the travel.
% @arg MinimumDistance An integer representing the minimum distance
%      between the first and last vertex. The kind of distances is set
%      in =Options=.
% @arg Vertices An ordered set of vertices.
% @arg Edges An ordered set of Edges.
% @arg History A list representing a minimum travel between the first and
%      last resources.

travel_min(O, G, E_P, N_P, First, Last, MinimumDistance, Vs, Es, History):-
  aggregate_all(
    set(Distance-History),
    traverse(O, G, E_P, N_P, First, Last, Distance, Vs, Es, History),
    Pairs
  ),
  first(Pairs, MinimumDistance-History).

