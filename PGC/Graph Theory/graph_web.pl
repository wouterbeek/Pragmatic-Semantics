:- module(
  graph_web,
  [
    circle_graph_web//1, % +AnyGraph:compound
    graph_web//1, % +AnyGraph:compound
    harary_web//2, % +K:nonneg
                   % +N:nonneg
    random_graph_web//1, % +AnyGraph:compound
    spring_embedding_web//2, % +AnyGraph:compound
                             % +Iterations:nonneg
    table_graph_web//1, % +AnyGraph:compound
    vertex_web//1 % +Vertex:iri
  ]
).

/** <module> Graph Web

Web front-end for generic graph visualizations.

@author Wouter Beek
@tbd All these methods should become graph datatype-independent.
@version 2013/01-2013/02, 2014/03
*/

:- use_module(generics(atom_ext)).
:- use_module(graph_theory(circular_graph_representation)).
:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_export_svg)).
:- use_module(graph_theory(graph_generic)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(graph_theory(spring_embedding)).
:- use_module(graph_theory(vertex_coordinate)).
:- use_module(gv(gv_file)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_graph(rdf_graph_theory)).
:- use_module(server(web_error)).
:- use_module(ugraph(ugraph_ext)).
:- use_module(xml(xml_dom)).

:- rdf_meta(vertex_web(r,?,?)).



%! circle_graph_web(+AnyGraph:compound)// is det.

circle_graph_web(AnyGraph) -->
  {
    export_graph(
      [edge_labels(all)],
      circular_vertex_coordinate,
      AnyGraph,
      GraphTerm
    ),
    graph_to_svg_dom([], GraphTerm, SvgDom)
  },
  xml_dom_to_atom(SvgDom),
  table_graph_web(AnyGraph).


%! graph_web(+Graph:graph)// is det.
% Generates the graph with the given name to a file in GraphViz DOT format.

graph_web(G) -->
  {export_graph_svg(G, SvgDom)},
  xml_dom_as_atom(SvgDom).


%! harary_web(+K:nonneg, +N:nonneg)// is det.
% Generates markup represening the K-connected Harary graph with N vertices.

harary_web(K, N) -->
  {ugraph_harary(K, N, Harary)},
  circle_graph_web(Harary).


%! random_graph_web(+Graph:graph)// is det.
% Generates a Web representation of the RDF graph with the given title.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.

random_graph_web(G) -->
  {random_vertex_coordinate([], G, rdf_vertices, VCoords)},
  vertice_coordinates_web(G, VCoords),
  vertice_coordinates_table(VCoords).


%! spring_embedding_web(+Graph:graph, ?Iterations:nonneg)// is det.
% Returns the markup for the spring embedding of the given graph.
%
% @arg Iterations An integer, representing the number of iterations
%      of spring embedding, i.e., the number of subsequent function
%      applications.
%      Default: `50`.

spring_embedding_web(Graph, I) -->
  {
    default(50, I),
    default_spring_embedding(
      Graph,
      ugraph_vertices,
      ugraph_edges,
      ugraph_neighbors,
      I,
      _FinalVerticeCoordinates,
      History
    )
  },
  spring_embedding_history(Graph, History).

spring_embedding_history(_, []) --> [].
spring_embedding_history(G, [H|T]) -->
  vertice_coordinates_web(G, H),
  spring_embedding_history(G, T).


%! table_graph_web(+AnyGraph:compound)// is det.

table_graph_web(AnyGraph) -->
  {
    export_graph(
      [
        colorscheme(none),
        edge_labels(all),
        in(rdf),
        literals(collapse),
        out(html_table)
      ],
      AnyGraph,
      GraphTerm
    ),
    graph_to_svg_dom([], GraphTerm, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).


%! vertex_web(+Vertex:iri)// is det.
% Generates a DOM representation for the given vertex in the given graph.

vertex_web(V) -->
  {
    export_vertex(
      [edge_labels(all),literals(preferred_label)],
      rdf_neighbors,
      V,
      GTerm
    ),
    export_graph_svg(GTerm, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).

