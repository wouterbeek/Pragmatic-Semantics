:- module(
  skos_read,
  [
    skos_broader/3, % ?Broader:uri
                    % ?Narrower:uri
                    % ?Graph:atom
    skos_export_hierarchy/1, % +Tree:tree
    skos_narrower/3, % ?Narrower:uri
                     % ?Broader:uri
                     % ?Graph:atom
    tree_to_nodes/2, % +Tree:tree
                     % -Nodes:ordset(uri)
    tree_to_triples/2 % +Tree:tree
                      % -Triples:ordset(triple)
  ]
).

/** <module> SKOS READ

@author Wouter Beek
@tbd rdf_beam/4 should return triples i.o. edges and vertices
     (from which it is difficult to build an RDF graph).
@version 2013/04-2013/05, 2013/07
*/

:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(library(aggregate)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_hierarchy)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').

:- rdf_meta(skos_export_hierarchy(r)).



skos_broader(Broader, Narrower, Graph):-
  rdf(Broader, skos:broader, Narrower, Graph).

skos_export_hierarchy(Root):-
  rdf_global_id(skos:broader, Predicate),
  rdf_export_hierarchy(Root, Predicate).

skos_narrower(Narrower, Broader, Graph):-
  rdf(Narrower, skos:narrower, Broader, Graph).

tree_to_triples(Nodes, AllTriples):-
  aggregate_all(
    set(rdf(Node, skos:broader, DownNode)),
    (
      member(Node, Nodes),
      rdf(Node, skos:broader, DownNode)
    ),
    Triples
  ),
  aggregate_all(
    set(DownNode),
    member(rdf(_Node, _, DownNode), Triples),
    DownNodes
  ),
  tree_to_triples(DownNodes, DownTriples),
  ord_union(Triples, DownTriples, AllTriples).

tree_to_nodes(Nodes, AllNodes):-
  aggregate_all(
    set(DownNode),
    (
      member(Node, Nodes),
      skos_broader(Node, DownNode, _Graph)
    ),
    DownNodes
  ),
  tree_to_nodes(DownNodes, AllDownNodes),
  ord_union(Nodes, AllDownNodes, AllNodes).

