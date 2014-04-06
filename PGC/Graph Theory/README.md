# PGC: Graph Theory

Generic Prolog modules implementing (aspects of) graph theory.

The following graph representation formatas are supported:
  1. DGRAPH
  2. RDF Graph
  3. UGRAPh

## Exporting

In order to reduce the amount of code that is needed for exporting graphs
in various representation formats, we introduce the Graph Interchange Format
(partially abbreviated as _GIFormat_).

The following layout formats for exports are supported:
  1. DOT
  2. NEATO

The following export formats are supported:
  1. GIF
  2. HTML table
  3. JPG
  4. PDF
  5. SVG

## Contents

The Graph Theory subcollection of the PGC contains the following
Prolog modules:
  * [circular_graph_representation.pl] Assigns coordinates to the vertices
    of a graph so that it has a circular layout.
  * DGRAPH
    * 
  * [export_graph_html.pl] Converts a term in GIFormat to an HTML table.
  * [export_graph_svg.pl] Converts a term in GIFormat to an SVG graphic.
  * [graph_export.pl] Predicates that are shared by the various
    export formats.
  * [graph_generic.pl] Predicates that apply to all internal
    graph representation formats (using meta-prediactes).
    Consider splitting this into smaller modules.
  * [graph_trav.pl] Needs to be cleaned and possibly integrated with
    other predicates for traversing graphs.
  * [graph_web.pl] Display of exported graphs inside modern Web browsers.
    Needs to be cleaned.
  * [random_vertex_coordinates.pl] Assigns random coordinates for vertices
    in graph export.
  * [spring_embedding.pl] Not yet finished.
  * UGRAPH
    * [ugraph_export.pl] Represents a UGRAPH using the GIFormat.
    * [ugraph_ext.pl] Support for undirected graphs, based on
      the UGRAPH representation format.
    * [ugraph_web.pl] Unify with modules [graph_generic.pl]
      and [graph_web.pl].
  * [vertex_coordinate.pl] Assigns coordinates to the veretices of a graph
    according to various approaches.
    Relocate each approach to a separate module.

@author Wouter Beek
@version alpha2
