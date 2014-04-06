:- module(
  rdf_tabular_graph,
  [
    rdf_tabular_graph//1, % +RdfGraph:atom
    rdf_tabular_graphs//0
  ]
).

/** <module> RDF tabular graph

Generates HTML tables for overviews of a single and for multiple
RDF graphs.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_web(rdf_tabular_class)).
:- use_module(rdf_web(rdf_tabular_datatype)).
:- use_module(rdf_web(rdf_tabular_property)).
:- use_module(rdf_web(rdf_term_html)).



%! rdf_tabular_graph(+RdfGraph:atom)// is det.
% Generates an HTML table describing the contents of the given RDF graph.
%
% The generated HTML consists of overviews of:
%   * All RDFS classes in the graph.
%   * All RDF properties in the graph.
%   * All datatype IRIs in the graph.

rdf_tabular_graph(Graph) -->
  rdf_tabular_classes(Graph),
  rdf_tabular_properties(Graph),
  rdf_tabular_datatypes(Graph).


%! rdf_tabular_graphs// is det.
% Generates an HTML table describing the currently loaded RDF graphs.
%
% The RDF graphs are orderd by the number of triples they contain.
%
% We exclude TMS-es from this overview.

rdf_tabular_graphs -->
  rdf_graphs_html(rdf_tabular).

