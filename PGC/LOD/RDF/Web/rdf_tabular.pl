:- module(
  rdf_tabular,
  [
    rdf_tabular_triples//4 % ?Subject:or([bnode,iri])
                           % ?Predicate:iri
                           % ?Object:or([bnode,iri,literal])
                           % ?Graph:atom
  ]
).

/** <module> RDF tabular

Generated RDF HTML tables.

@author Wouter Beek
@tbd Add blank node map.
@tbd Add namespace legend.
@tbd Add local/remote distinction.
@tbd Include images.
@version 2013/12-2014/04
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_ext)).
:- use_module(generics(uri_query)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_parse)).
:- use_module(rdf_web(rdf_tabular_graph)).
:- use_module(rdf_web(rdf_tabular_term)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_term_html)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(tabular), rdf_tabular, [priority(-1)]).

user:web_module('RDF Tabular', rdf_tabular).



%! rdf_tabular(+Request:list(nvpair)) is det.
% Serves an HTML page describing RDF data.
%
% The following variants are supported, based on the URL search string:
%   * =|graph=Graph|=
%     Serves a description of the given RDF graph.
%   * =|term=Term|=
%     Serves a description of the given RDF term.
%     The atom `Term` is parsed by a grammar rule
%     that extract the corresponding RDF term.
%     This also allows atomic renditions of prefix-abbreviated IRIs as input,
%     e.g. `'dbpedia:Monkey'`.
%   * No search string.
%     Serves a description of all currently loaded RDF graphs.

% RDF term.
rdf_tabular(Request):-
  request_query_read(Request, term, T), !,
  
  % Parse the tern atom to extract the corresponding RDF term.
  once(dcg_phrase(rdf_parse_term(T1), T)),
  rdf_global_id(T1, T2),

  % The graph parameter is optional
  % (in which case it is left uninstantiated).
  ignore(request_query_read(Request, graph, G)),

  reply_html_page(
    app_style,
    title([
      'Overview of RDF resource ',
      \rdf_term_name([graph(G)], T2)
    ]),
    [
      h1([
        'Description of RDF term ',
        \rdf_term_in_graph_html(rdf_tabular, T2, G)
      ]),
      \rdf_tabular_term(G, T2)
    ]
  ).
% RDF graph.
rdf_tabular(Request):-
  request_query_read(Request, graph, Graph), !,
  reply_html_page(
    app_style,
    title(['Overview of RDF graph ',\rdf_graph_name(Graph)]),
    html([
      h1([
        'Description of RDF graph ',
        \rdf_graph_html(rdf_tabular, Graph),
        '.'
      ]),
      \rdf_tabular_graph(Graph)
    ])
  ).
% Default: RDF graphs.
rdf_tabular(_Request):-
  reply_html_page(
    app_style,
    title('Overview of RDF graphs'),
    html([
      h1('Overview of RDF graphs'),
      \rdf_tabular_graphs
    ])
  ).


%! rdf_tabular_triples(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   ?Graph:atom
%! )// is det.

rdf_tabular_triples(S, P, O, G) -->
  {
    aggregate_all(
      set([S,P,O,G]),
      rdf(S, P, O, G),
      Rows1
    ),
    % Restrict the number of rows in the table arbitrarily.
    list_truncate(Rows1, 50, Rows2)
  },
  rdf_html_table(
    [graph(G),header_row(spog)],
    rdf_tabular_triples_caption(S, P, O, G),
    Rows2
  ).


rdf_tabular_triples_caption(S, P, O, G) -->
  {
    nonvar(S),
    var(P),
    var(O),
    nonvar(G)
  }, !,
  html([
    'RDF triples in which ',
    \rdf_term_html(rdf_tabular, S, G),
    ' occurs in the subject position.'
  ]).
rdf_tabular_triples_caption(S, P, O, G) -->
  {
    var(S),
    nonvar(P),
    var(O),
    nonvar(G)
  }, !,
  html([
    'RDF triples in which ',
    \rdf_term_html(rdf_tabular, P, G),
    ' occurs in the predicate position.'
  ]).
rdf_tabular_triples_caption(S, P, O, G) -->
  {
    var(S),
    var(P),
    nonvar(O),
    nonvar(G)
  }, !,
  html([
    'RDF triples in which ',
    \rdf_term_html(rdf_tabular, O, G),
    ' occurs in the object position.'
  ]).
rdf_tabular_triples_caption(_, _, _, _) -->
  html('RDF triples').

