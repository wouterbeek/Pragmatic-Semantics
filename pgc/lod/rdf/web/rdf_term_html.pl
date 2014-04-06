:- module(
  rdf_term_html,
  [
    rdf_graph_html//2, % +LocationId:compound
                       % +RdfGraph:atom
    rdf_graphs_html//1, % +LocationId:compound
    rdf_literal_html//5, % +LocationId:compound
                         % +LexicalForm:atom
                         % +DatatypeIri:iri
                         % +LanguageTag:atom
                         % ?RdfGraph:atom
    rdf_term_html//2, % +LocationId:compound
                      % +RdfTerm:or([bnode,iri,literal])
    rdf_term_html//3, % +LocationId:compound
                      % +RdfTerm:or([bnode,iri,literal])
                      % ?RdfGraph:atom
    rdf_term_in_graph_html//3, % +LocationId:compound
                               % +RdfTerm:or([bnode,iri,literal])
                               % ?RdfGraph:atom
    rdf_triple_html//4, % +LocationId:compound
                        % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
    rdf_triple_html//5 % +LocationId:compound
                       % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:or([bnode,iri,literal])
                       % ?RdfGraph:atom
  ]
).

/** <module> RDF term HTML

HTML generation for RDF terms.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_remote_module(generics(typecheck)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(generics(uri_query)).
:- use_remote_module(html(html_list)).
:- use_remote_module(html(html_tuple)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(pl_web(html_pl_term)).
:- use_remote_module(rdf(rdf_graph)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(server(web_ui)).
:- use_remote_module(tms(tms)).
:- use_remote_module(xml(xml_namespace)).

:- rdf_meta(rdf_literal_html(+,+,r,+,?)).
:- rdf_meta(rdf_term_html(+,o,?,?)).
:- rdf_meta(rdf_term_html(+,o,?,?,?)).
:- rdf_meta(rdf_term_in_graph_html(+,o,?,?,?)).
:- rdf_meta(rdf_triple_html(+,r,r,o,?,?)).
:- rdf_meta(rdf_triple_html(+,r,r,o,?,?,?)).



% TERM %

rdf_term_html(LocationId, RdfTerm) -->
  rdf_term_html(LocationId, RdfTerm, _).


% RDF graphs.
rdf_term_html(LocationId, Graphs, _) -->
  {
    Graphs \= [],
    maplist(rdf_graph, Graphs)
  }, !,
  html_list([ordered(false)], rdf_term_html(LocationId), Graphs).
% RDF graph.
rdf_term_html(LocationId, Graph1, _) -->
  {rdf_is_graph(Graph1, Graph2)}, !,
  rdf_graph_html(LocationId, Graph2).
rdf_term_html(LocationId, RdfTerm, _) -->
  {
    rdf_is_resource(RdfTerm),
    rdfs_individual_of(RdfTerm, rdf:'List')
  }, !,
  rdf_list_html(LocationId, RdfTerm).
% Blank node.
rdf_term_html(LocationId, RdfTerm, _) -->
  {rdf_is_bnode(RdfTerm)}, !,
  rdf_blank_node_html(LocationId, RdfTerm).
% Literal.
rdf_term_html(LocationId, RdfTerm, Graph) -->
  {rdf_literal(RdfTerm, LexicalForm, Datatype, LangTag)}, !,
  rdf_literal_html(LocationId, LexicalForm, Datatype, LangTag, Graph).
% IRI.
rdf_term_html(LocationId, RdfTerm, Graph) -->
  rdf_iri_html(LocationId, RdfTerm, Graph).
% Prolog term.
rdf_term_html(_, PlTerm, _) -->
  html(span(class='prolog-term', \html_pl_term(PlTerm))).


%! rdf_term_in_graph_html(
%!   +LocationId:compound,
%!   +RdfTerm:or([bnode,iri,literal]),
%!   +RdfGraph:atom
%! )// is det.

rdf_term_in_graph_html(LocationId, T, G) -->
  rdf_term_html(LocationId, T),
  rdf_in_graph_html(LocationId, G).



% GRAPH %

rdf_graph_html(LocationId, Graph) -->
  {
    http_location_by_id(LocationId, Location1),
    uri_query_add(Location1, graph, Graph, Location2)
  },
  html(span(class='rdf-graph', a(href=Location2, Graph))).


rdf_graphs_html(LocationId) -->
  {
    findall(
      NumberOfTriples-Graph,
      (
        rdf_graph(Graph),
        \+ tms(Graph),
        rdf_statistics(triples_by_graph(Graph,NumberOfTriples))
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    pairs_keys(Pairs2, Keys),
    sum_list(Keys, Triples),
    reverse(Pairs2, Pairs3),
    findall(
      [Graph,NumberOfTriples],
      member(NumberOfTriples-Graph, [Triples-'All'|Pairs3]),
      Rows
    )
  },
  rdf_html_table(
    [header_row(true),location(LocationId)],
    html('RDF graphs (non-TMS)'),
    [['Graph','Number of triples']|Rows]
  ).


rdf_in_graph_html(_, Graph) -->
  {var(Graph)}, !.
rdf_in_graph_html(LocationId, Graph) -->
  html([' in graph ',\rdf_graph_html(LocationId, Graph)]).



% RDF LIST %

rdf_list_html(LocationId, RDF_List) -->
  html(div(class='rdf-list', \rdf_term_name(LocationId, RDF_List))).



% BLANK NODE %

rdf_blank_node_html(LocationId, BNode) -->
  {
    atom(BNode),
    atom_prefix(BNode, '__'), !,
    http_location_by_id(LocationId, Location1),
    uri_query_add(Location1, term, BNode, Location2)
  },
  html(span(class='blank-node', a(href=Location2, BNode))).



% LITERAL %

rdf_language_tag_html(LangTag) -->
  html(span(class='language-tag', atom(LangTag))).

xsd_lexical_form(LexicalForm) -->
  html(span(class='rdf-lexical-form', LexicalForm)).

% Simple literal.
rdf_literal_html(LocationId, LexicalForm, Datatype, LangTag, Graph) -->
  {var(Datatype)}, !,
  rdf_literal_html(LocationId, LexicalForm, xsd:string, LangTag, Graph).
% Language-tagged string.
rdf_literal_html(_, LexicalForm, rdf:langString, LangTag, _) -->
  {nonvar(LangTag)}, !,
  html(
    span(class='language-tagged-string', [
      \xsd_lexical_form(LexicalForm),
      '@',
      \rdf_language_tag_html(LangTag)
    ])
  ).
% XSD datatypes.
rdf_literal_html(LocationId, LexicalForm, Datatype, _, Graph) -->
  html(
    span(class='rdf-literal', [
      '"',
      \xsd_lexical_form(LexicalForm),
      '"^^',
      \rdf_iri_html(LocationId, Datatype, Graph)
    ])
  ).



% IRI %

% E-mail.
rdf_iri_html(_, IRI1, _) -->
  {
    uri_components(IRI1, uri_components(Scheme, _, IRI2, _, _)),
    Scheme == mailto
  }, !,
  html(span(class='e-mail', a(href=IRI1, IRI2))).
% Image.
rdf_iri_html(_, IRI, _) -->
  {is_image_url(IRI)}, !,
  html(span(class='image', a(href=IRI, img(src=IRI, [])))).
% IRI.
rdf_iri_html(LocationId, IRI1, Graph) -->
  {rdf_global_id(IRI2, IRI1)},
  (
    {IRI2 = Prefix:Postfix}
  ->
    {
      (
        xml_current_namespace(Prefix, _), !
      ;
        existence_error('XML namespace', Prefix)
      ),
      http_location_by_id(LocationId, Location1),
      uri_query_add(Location1, term, IRI1, Location2),
      (
        var(Graph)
      ->
        Location3 = Location2
      ;
        uri_query_add(Location2, graph, Graph, Location3)
      )
    },
    html(
      span(class='iri', [
        a(href=Location3, [
          span(class=prefix, Prefix),
          ':',
          span(class=postfix, Postfix)
        ]),
        ' ',
        \external_link(IRI1)
      ])
    )
  ;
    {is_of_type(iri, IRI1)}
  ->
    html(span(class='iri', a(href=IRI1, IRI1)))
  ).



% TRIPLE %

%! rdf_triple_html(
%!   +LocationId:compound,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! )// is det.

rdf_triple_html(LocationId, S, P, O) -->
  html_triple(rdf_term_html(LocationId), S, P, O).


%! rdf_triple_html(
%!   +LocationId:compound,
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +RdfGraph:atom
%! )// is det.

rdf_triple_html(LocationId, S, P, O, G) -->
  html_quadruple(rdf_term_html(LocationId), S, P, O, G).

