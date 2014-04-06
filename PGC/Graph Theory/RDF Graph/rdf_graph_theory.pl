:- module(
  rdf_graph_theory,
  [
    rdf_edge/2, % +Graph:atom
                % ?Edge:edge
    rdf_edge/3, % +Options:list(nvpair)
                % +Graph:atom
                % ?Edge:edge
    rdf_edges/2, % +Graph:atom
                 % ?Edges:ordset(edge)
    rdf_edges/3, % +Options:list(nvpair)
                 % +Graph:atom
                 % ?Edges:ordset(edge)
    rdf_edges_to_vertices/2, % +Edges:ordset(edge)
                             % -Vertices:ordset(vertex)
    rdf_graph_to_ugraph/2, % +Graph:atom
                           % -UG:ugraph
    rdf_neighbor/3, % +Graph:atom
                    % ?Vertex:vertex
                    % ?Neighbor:vertex
    rdf_neighbors/3, % +Graph:atom
                     % +Vertex:vertex
                     % -Neighbors:ordset(vertex)
    rdf_triples_to_edges/2, % +Triples:list(rdf_triple)
                            % -Edges:ordset(rdf_term)
    rdf_triples_to_vertices/2, % +Triples:list(rdf_triple)
                               % -Vertices:ordset(rdf_term)
    rdf_vertex/2, % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex/3, % +Options:list(nvpair)
                  % +Graph:atom
                  % ?Vertex:vertex
    rdf_vertex_equivalence/2, % +Resource1:uri
                              % +Resource2:uri
    rdf_vertices/2, % +Graph:atom
                    % -Vertices:ordset(vertex)
    rdf_vertices/3 % +Options:list(nvpair)
                   % +Graph:atom
                   % -Vertices:ordset(vertex)
  ]
).

/** <module> RDF graph theory

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
 edges (as defined by RDF abstract syntax) in one triple can be nodes in
 another.
This means that the definitions 'edge' and 'vertex' for graph theoretic
 operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03, 2013/08, 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_edge(+,r)).
:- rdf_meta(rdf_edge(+,+,r)).
:- rdf_meta(rdf_edges(+,r)).
:- rdf_meta(rdf_edges(+,+,r)).
:- rdf_meta(rdf_neighbor(+,r,r)).
:- rdf_meta(rdf_neighbors(+,r,-)).
:- rdf_meta(rdf_vertex(+,r)).
:- rdf_meta(rdf_vertex(+,+,r)).
:- rdf_meta(rdf_vertex_equivalence(r,r)).



rdf_bnode_to_var(S, _):-
  rdf_is_bnode(S), !.
rdf_bnode_to_var(X, X).

rdf_edge(G, E):-
  rdf_edge([], G, E).

%! rdf_edge(+Options:list(nvpair), +Graph:atom, ?Edge:edge) is nondet.
% RDF edges between resources.
%
% The following options are supported:
%   1. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%
% @arg Options A list of name-value pairs.
% @arg Graph The atomic name of an RDF graph.
% @arg Edge An edge, either `FromV-ToV` or `FromV-P-ToV`.

rdf_edge(O, G, FromV-P-ToV):- !,
  rdf(FromV, P, ToV, G),
  % Make sure the vertices pass the vertex filter.
  rdf_vertex_check(O, FromV),
  rdf_vertex_check(O, ToV).
rdf_edge(O, G, FromV-ToV):-
  rdf_edge(O, G, FromV-_-ToV).

rdf_edges(G, Es):-
  rdf_edges([], G, Es).

rdf_edges(O, G, Es):-
  aggregate_all(
    set(E),
    rdf_edge(O, G, E),
    Es
  ).

rdf_edges_to_vertices(Es, Vs):-
  aggregate_all(
    V,
    (
      member(V-_W1, Es)
    ;
      member(_-V, Es)
    ),
    Vs
  ).

%! rdf_graph_to_ugraph(+Graph:atom, -UGraph:ugraph) is det.
% Returns the UG representation of a loaded RDF graph.
%
% @arg G The atomic name of a loaded RDF graph.
% @arg UG:ugraph A UG datastructure.

rdf_graph_to_ugraph(G, UG):-
  aggregate_all(
    set(From-Neighbors),
    (
      rdf_vertex(G, From),
      aggregate_all(
        set(To),
        rdf_edge(G, From-To),
        Neighbors
      )
    ),
    UG
  ).

rdf_literal_to_value(literal(lang(_Lang,LitVal)), LitVal):- !.
rdf_literal_to_value(literal(type(_Type,LitVal)), LitVal):- !.
rdf_literal_to_value(literal(LitVal), LitVal):- !.

%! rdf_neighbor(+Graph:atom, ?Vertex:vertex, ?Neighbor:vertex) is nondet.
% Neighboring vertices.

rdf_neighbor(G, V1, V2):-
  rdf_edge(G, V1-V2).

rdf_neighbors(G, V, Ns):-
  aggregate_all(
    set(N),
    rdf_neighbor(G, V, N),
    Ns
  ).

rdf_triples_to_edges(Ts, Es):-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_P,ToV), Ts),
    Es
  ).

rdf_triples_to_vertices(Ts, Vs):-
  aggregate_all(
    set(V),
    (
      member(rdf(V1,_P,V2), Ts),
      (
        V = V1
      ;
        V = V2)
      ),
    Vs
  ).

rdf_vertex(G, V):-
  rdf_vertex([], G, V).

%! rdf_vertex(+Options:list(nvpair), +Graph:atom, ?Vertex:rdf_term) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% The following options are supported:
%   1. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%   2. `rdf_list(+Include:boolean)`
%      Whether vertices that are part of an RDF list should be included
%      in full (`true`, default) or in a concise way (`false`).
%
% @arg Options A list of name-value pairs.
% @arg Graph The atomic name of an RDF graph.
% @arg Vertex An RDF term.

rdf_vertex(O, G, V):-
  (rdf(V, _, _, G) ; rdf(_, V, _, G) ; rdf(_, _, V, G)),
  rdf_vertex_check(O, V).

% Typed literals are only included when `literals=all`.
rdf_vertex_check(O, literal(type(_Datatype,_Value))):-
  option(literals(all), O, all), !.
% Untyped literals are included:
%   * if the language tag is matched, under option `literals=preferred_label`.
%   * never, under option `literals=none`.
%   * always, under option `literals=all`
rdf_vertex_check(O, Literal):-
  rdf_is_literal(Literal), !,
  option(literals(IncludeLiterals), O, all),

  % No literal is allowed as vertex under option `literals=none`.
  IncludeLiterals \== none,

  % Under option `literal=preferred_label`,
  % the given literal must be the preferred literal
  % for the subject-predicate pair involved.
  (
    IncludeLiterals == preferred_label
  ->
    % We must know the subject and predicate terms
    % that occur in the same triple as the given literal.
    rdf_literal(S, P, LexicalForm1, DatatypeIri, LanguageTag1, _),

    % Make sure the same literal with the preferred language tag does
    % not exist.
    % This excludes `literal(aap)` and `literal(aap,nl)` form
    % being both displayed.
    (
      rdf_equal(xsd:string, DatatypeIri),
      var(LanguageTag1)
    ->
      \+ ((
        rdf_literal(S, P, LexicalForm2, DatatypeIri, LanguageTag2, _),
	LexicalForm2 \== LexicalForm1,
        nonvar(LanguageTag2)
      ))
    ;
       true
    ),

    % Only preferred labels are allowed as vertices.
    option(language(Lang), O, en),
    % The given literal must be the preferred literal,
    % otherwise this predicate should fail.
    rdf_preferred_language_tagged_string(Lang, S, P, LexicalForm1, _, _)
  ;
    % All literals are vertices.
    true
  ).
% Non-literal RDF terms.
% No restriction on RDF lists.
rdf_vertex_check(O, _V):-
  option(rdf_list(true), O, true), !.
% With setting `rdf_list=false` RDF terms should not be part of an RDF list.
rdf_vertex_check(O, V):-
  option(rdf_list(false), O, true),
  % The vertex must have some occurrence outside an RDF list.
  once((
    rdf(V, _, _)
  ;
    rdf(_, P, V),
    \+ rdf_memberchk(P, [rdf:first,rdf:rest])
  )).

% @tbd What is this?
rdf_vertex_equivalence(X, Y):-
  % Subject
  forall(
    rdf_has(X, P, O),
    rdf_has(Y, P, O)
  ),
  forall(
    rdf_has(Y, P, O),
    rdf_has(X, P, O)
  ),
  % Predicate
  forall(
    rdf_has(S, X, O),
    rdf_has(S, Y, O)
  ),
  forall(
    rdf_has(S, Y, O),
    rdf_has(S, X, O)
  ),
  % Object
  forall(
    rdf_has(S, P, X),
    rdf_has(S, P, Y)
  ),
  forall(
    rdf_has(S, P, Y),
    rdf_has(S, P, X)
  ).

rdf_vertices( G, Vs):-
  rdf_vertices([], G, Vs).

rdf_vertices(O, G, Vs):-
  aggregate_all(
    set(V),
    rdf_vertex(O, G, V),
    Vs
  ).

