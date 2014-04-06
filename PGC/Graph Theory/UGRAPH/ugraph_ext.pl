:- module(
  ugraph_ext,
  [
    is_ugraph/1, % +Graph:ugraph
    ugraph_complete/1, % +Graph:ugraph
    ugraph_complete/2, % +Vs:ordset
                       % ?Graph:ugraph
    ugraph_direct_subgraph/2, % ?SubGraph:ugraph
                              % +Graph:ugraph
    ugraph_edge/2, % +Options:list(nvpair)
                   % ?Edge:edge
    ugraph_edge_induced_subgraph/3, % +Graph:ugraph
                                    % +ESubG:list(edge)
                                    % -SubG:ugraph
    ugraph_empty/1, % ?Graph:ugraph
    ugraph_harary/3, % +K:integer
                     % +N:integer
                     % -H:ugraph
    ugraph_line_graph/2, % +Graph:ugraph
                         % -LineG:ugraph
    ugraph_maximum_components/2, % +Graph:ugraph
                                 % -MaximumComponents:list(ugraph)
    ugraph_neighbor/3, % ?Vertex:vertex
                       % ?Graph:atom
                       % ?Neighbor:vertex
    ugraph_subgraph/2, % ?SubGraph:ugraph
                       % +Graph:ugraph
    ugraph_unsymmetric_edges/2, % +Graph:ugraph
                                % -UnsymmetricEdges:ordset(edge)
    ugraph_vertex/2, % +Graph:ugraph
                     % ?Vertex:vertex
    ugraph_vertex_induced_subgraph/3 % +Graph:ugraph
                                     % +Vertices:ordset(vertex)
                                     % -SubGraph:ugraph
  ]
).
:- reexport(
  library(ugraphs),
  [
    add_edges/3 as ugraph_add_edges, % +Graph
                                     % +Edges
                                     % -NewGraph
    add_vertices/3 as ugraph_add_vertices, % +Graph
                                           % +Vertices
                                           % -NewGraph
    complement/2 as ugraph_complement, % +Graph
                                       % -NewGraph
    compose/3 as ugraph_compose, % +LeftGraph
                                 % +RightGraph
                                 % -NewGraph
    del_edges/3 as ugraph_del_edges, % +Graph
                                     % +Edges
                                     % -NewGraph
    del_vertices/3 as ugraph_del_vertices, % +Graph
                                           % +Vertices
                                           % -NewGraph
    edges/2 as ugraph_edges, % +Graph
                             % -Edges
    neighbors/3 as ugraph_neighbors, % +Vertex
                                     % +Graph
                                     % -Vertices
    reachable/3 as ugraph_reachable, % +Vertex
                                     % +Graph
                                     % -Vertices
    top_sort/2 as ugraph_top_sort, % +Graph
                                   % -Sort
    top_sort/3 as ugraph_top_sort, % +Graph
                                   % -Sort0
                                   % -Sort
    transitive_closure/2 as ugraph_transitive_closure, % +Graph
                                                       % -Closure
    transpose/2 as ugraph_transpose, % +Graph
                                     % -NewGraph
    vertices/2 as ugraph_vertices, % +Graph
                                   % -Vertices
    vertices_edges_to_ugraph/3 as ugraph_vertices_edges_to_ugraph, % +Vertices
                                                                   % +Edges
                                                                   % -Graph
    ugraph_union/3 % +Graph1
                   % +Graph2
                   % -Graph
  ]
).

/** <module> UGRAPH_EXT

Methods that extend the SWI-Prolog builtin library for undirected graphs.

This uses the SWI-Prolog library =ugraph=, originally
written by Richard O'Keefe. Also implemented in YAP and
SICStus Prolog.

Searching for a pair takes $2|V(G)|$ instead of $|V(G)|^2$.

All edges $\tuple{v, w}$ occur twice in a ugraph,
i.e. $v-[\ldots, w, \ldots]$ and $w-[\ldots, v, \ldots]$.

# Types

## 2D vertex

A 2D vertex (or =|vertice_coorinate|=) is a compound term representing
a 2D representation of a vertex.
It has the form =|vertex_coordinate(<vertive>, <2d_coordinate>)|=.

@author Wouter Beek
@version 2012/09-2013/02, 2013/07, 2014/03
*/

:- use_module(generics(list_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(math(math_ext)).

:- meta_predicate(ugraph_harary(+,2,+,-)).



is_ugraph(UG):-
  is_list(UG),
  maplist(is_ugraph_edge, UG).

is_ugraph_edge(V-Ws):-
  atomic(V),
  is_list(Ws).

%! ugraph_complete(+Graph:ugraph) is semidet.
% Succeeds for complete graphs.
%
% @see Wrapper around ugraph_complete/2.

ugraph_complete(UG):-
  ugraph_vertices(UG, Vs),
  ugraph_complete(Vs, UG).

%! ugraph_complete(+Vs:ordset, ?Graph:ugraph) is semidet.
% Succeeds if the given graph is complete, or generates the complete graph
% from the given vertices.
%
% *Definition*: A complete graph is one in which all different vertices
%               are connected.
%
% @arg Vs An ordered set of vertices.
% @arg Graph A ugraph, i.e., a list of S-expressions.

ugraph_complete(Vs, UG):-
  ugraph_complete_(Vs, Vs, UG).

ugraph_complete_(_Vs, [], []).
ugraph_complete_(Vs, [FromV | FromVs], [FromV-ToVs | UG]):-
  ord_del_element(Vs, FromV, ToVs),
  ugraph_complete_(Vs, FromVs, UG).

ugraph_direct_subgraph(DirSubG, G):-
  ugraph_vertices(G, Vs),
  ugraph_edges(G, Es1),
  select(V-W, Es1, Es2),
  V > W,
  select(W-V, Es2, Es3),
  ugraph_vertices_edges_to_ugraph(Vs, Es3, DirSubG).

%! ugraph_edge(+UG:ugraph, ?Edge:edge) is nondet.
% Edges in an undirected graph.
%
% @tbd Undirected behavior?

ugraph_edge(Graph, From-To):-
  ugraph_edges(Graph, Edges),
  member(From-To, Edges).

%! ugraph_edge_induced_subgraph(
%!   +Graph:ugraph,
%!   +ESubG:list(edge),
%!  -SubG:ugraph
%! ) is det.
% Returns the edge-induced subgraph.

ugraph_edge_induced_subgraph(G, ESubG, SubG):-
  ugraph_edges(G, Es),
  ord_subtract(Es, ESubG, DelEs),
  del_edges(G, DelEs, SubG).

%! ugraph_empty(?Graph:ugraph) is semidet.
% Succeeds on the empty graph or returns the empty graph.

ugraph_empty([]).

%! ugraph_harary(+K:integer, +N:integer, -H:ugraph) is det.
% Generates a Harary graph that is K-connected and that has N vertices.
%
% *Definition*: A Harary graph is a K-connected simple graph with
%               N vertices and the minimal number of edges.
%
% @arg K The connectedness of the Harary graph.
% @arg N The number of vertices.
% @arg H An undirected Harary graph.

ugraph_harary(K, N, H):-
  even(K), !,
  V_Last is N - 1,
  numlist(0, V_Last, Vs),
  Half_K is K / 2,
  findall(
    V-Neighbors,
    (
      member(V, Vs),
      Min is (V - Half_K) mod N,
      Max is (V + Half_K) mod N,
      cyclic_numlist(Min, Max, N, Ns_),
      select(V, Ns_, Neighbors)
    ),
    H
  ).
ugraph_harary(K, N, H):-
  even(N), !,
  NewK is K - 1,
  ugraph_harary(NewK, N, Graph),
  ugraph_harary(Graph, =, N, H).
ugraph_harary(K, N, H):-
  NewK is K - 1,
  ugraph_harary(NewK, N, Graph),
  ugraph_harary(Graph, pred, N, H).

ugraph_harary(G, P, N, H):-
  call(P, N, NewN),
  findall(
    V-Ns,
    (
      member(V-Ms, G),
      W is V + (NewN / 2),
      (
        W =< N
      ->
        Ns = [V | Ms]
      ;
        Ns = Ms
      )
    ),
    H
  ).

%! ugraph_line_graph(+Graph:ugraph, -LineG:ugraph) is det.
% Returns the line graph for the given graph.
%
% *Definition*: The line graph G' of graph G has V(G') = E(G) and
%               $E(G') = \  {\tuple{\tuple{x, y},\tuple{y, z}} \vert
%               \tuple{x, y}, \tuple{y, z} \in E(G)}$.
%
% *Representation*: Vertex $V \in V(LineG)$ that is based on edge
%                   $\tuple{X, Y} \in E(G)$ is represented in the following
%                   way: =|V = X/Y|=, where $X < Y$.
%
% *Restriction*: Line graphs can only be created for undirected graphs,
%                see undirected/1.
%
% *Restriction*: Line graphs can only consist of vertices that adhere to
%                the sorting relation <.
%
% @tbd Allow a comparator argument, so that vertices that do not compare
%      with < are allowed as well.

ugraph_line_graph(UG, LineG):-
  ugraph_edges(UG, EG),
  findall(
    V/W-Neighbors,
    (
      member(V-W, EG),
      V < W,
      findall(
        X/Y,
        (
          member(X-Y, EG),
          X < Y,
          (
            X == V
          ;
            X == W
          ;
            Y == V
          ;
            Y == W
          ),
          X/Y \== V/W
        ),
        Neighbors
      )
    ),
    LineG
  ).

%! ugraph_maximum_components(+G:ugraph, -MaxComps:list(ugraph)) is det.

ugraph_maximum_components(G, MaxComps):-
  ugraph_maximum_components_([G], MaxComps).
ugraph_maximum_components_([], []).
ugraph_maximum_components_([H | T], [H | Sol]):-
  connected(ugraph_vertices, ugraph_edges, H), !,
  ugraph_maximum_components_(T, Sol).
ugraph_maximum_components_([H | T], Sol):-
  findall(
    DSG,
    ugraph_direct_subgraph(DSG, H),
    DSGs
  ),
  append(T, DSGs, NewT),
  ugraph_maximum_components_(NewT, Sol).

%! ugraph_neighbor(+Vertex:vertex, +Graph:ugraph, -Neighbor:vertex) is nondet.
% Neighboring vertex.

ugraph_neighbor(Vertex, UG, Neighbor):-
  ugraph_edge(UG, Vertex-Neighbor).

%! ugraph_subgraph(?SubGraph:ugraph, +Graph:ugraph) is nondet.
% `SubGraph` is a subgraph of `Graph`.

ugraph_subgraph(SubG, G):-
  ugraph_subgraph_(SubG, G, [], []).
ugraph_subgraph_([], [], _In, _Out).
ugraph_subgraph_([V-SubNs | SubG], [V-Ns | G], In, Out):-
  sublist(SubNs_, Ns),
  ord_subtract(SubNs_, Out, SubNs),
  ord_union(In, SubNs, NewIn),
  ugraph_subgraph_(SubG, G, NewIn, Out).
ugraph_subgraph_(SubG, [V | G], In, Out):-
  \+ member(V, In),
  ord_add_element(Out, V, NewOut),
  ugraph_subgraph_(SubG, G, In, NewOut).

%! ugraph_unsymmetric_edges(
%!   +Graph:ugraph,
%!   -UnsymmetricEdges:ordset(edge)
%! ) is det.
% Returns the unsymmetric edges for the given edges.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

ugraph_unsymmetric_edges(G, UnsymmetricEs):-
  ugraph_edges(G, Es),
  aggregate_all(
    set(V-W),
    (
      member(V-W, Es),
      V < W
    ),
    UnsymmetricEs
  ).

%! ugraph_vertex(+Graph:ugraph, ?Vertex:vertex) is nondet.
% Vertices in a graph.

ugraph_vertex(G, V):-
  ugraph_vertices(G, Vs),
  member(V, Vs).

%! ugraph_vertex_induced_subgraph(
%!   +Graph:ugraph,
%!   ?Vertices:list(vertex),
%!   ?SubGraph:ugraph
%! ) is det.
% Returns the vertex-induced subgraph.

ugraph_vertex_induced_subgraph(G, SubVs, SubG):-
  ugraph_vertices(G, Vs),
  ord_subtract(Vs, SubVs, DelVs),
  ugraph_del_vertices(G, DelVs, SubG).

