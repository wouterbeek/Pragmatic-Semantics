:- module(rdf_man_literals, []).

/** <module> RDF management: literals

Support for managing RDF literals.

@author Wouter Beek
@version 2014/03-2014/04
*/

:- use_remote_module(generics(uri_query)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf(rdf_stat)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(rdf_web(rdf_term_html)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(xsd(xsd)).
:- use_remote_module(xsd(xsd_clean)).

http:location(rdf,     root(rdf), []).
http:location(rdf_man, rdf(man),  []).
:- http_handler(rdf_man(literals), rdf_man_literals, []).

user:web_module('RDFm literals', rdf_man_literals).



has_inconsistent_literal(G, P-TDatatype):-
  rdf_literal(_, P, LexicalForm, ADatatype, G),
  \+ xsd_convert_value(ADatatype, LexicalForm, TDatatype, _).


rdf_man_literals(Request):-
  request_query_read(Request, graph, G), !,
  reply_html_page(
    app_style,
    title([
      'RDFm literals - RDF graph ',
      \rdf_graph_name(G)
    ]),
    html(\rdf_man_literals(G))
  ).
rdf_man_literals(_Request):-
  reply_html_page(
    app_style,
    title('RDFm literals - RDF graphs'),
    html(\rdf_graphs_html(rdf_man_iterals))
  ).


rdf_man_literals(G) -->
  rdf_man_incorrect_literals(G),
  rdf_man_property_literals(G).


% Not needed now.
rdf_man_correct_literals(G) -->
  {
    aggregate_all(
      set([S,P,O,PlValue]),
      (
        rdf(S, P, O, G),
        rdf_literal(O, LexicalForm, Datatype, LangTag),
        rdf_literal_map(LexicalForm, Datatype, LangTag, PlValue)
      ),
      CorrectRows
    )
  },
  rdf_html_table(
    [header_row(true),location(rdf_man_literals)],
    html([
      'Overview of correct RDF literals in all RDF graph .',
      \rdf_graph_html(rdf_man_literals, G)
    ]),
    [['Subject','Predicate','Literal','Prolog value']|CorrectRows]
  ).


rdf_man_property_literals(G) -->
  {
    findall(
      P-Range,
      (
        % @tbd Use rdfs_domain/4 when its done.
        rdf(P, rdfs:range, Range, G),
        xsd_datatype(Range)
      ),
      Pairs
    ),
    partition(has_inconsistent_literal(G), Pairs, Inconsistent, Consistent)
  },
  rdf_man_property_inconsistent_literals(G, Inconsistent),
  rdf_man_property_consistent_literals(Consistent).


rdf_man_property_consistent_literals(Pairs) -->
  {
    findall(
      [P,D],
      member(P-D, Pairs),
      Rows
    )
  },
  rdf_html_table(
    [header_row(true),indexed(true),location(rdf_man_literals)],
    html('Overview of RDF properties with consistent literals.'),
    [['Property','Datatype']|Rows]
  ).


rdf_man_property_inconsistent_literals(G, Pairs) -->
  {
    findall(
      [S,P,Literal,G,TDatatype],
      (
        member(P-TDatatype, Pairs),
        rdf_literal(S, P, LexicalForm, ADatatype, G),
        \+ xsd_convert_value(ADatatype, LexicalForm, TDatatype, _),
        rdf_literal(Literal, LexicalForm, ADatatype)
      ),
      Rows
    )
  },
  rdf_html_table(
    [header_row(true),indexed(true),location(rdf_man_literals)],
    html([
      'Overview of inconsistent literals for RDF property ',
      \rdf_term_in_graph_html(rdf_man_literals, P, G),
      '.'
    ]),
    [['Subject','Predicate','Literal','Graph','ABox datatype']|Rows]
  ).


rdf_man_incorrect_literals(G) -->
  {
    aggregate_all(
      set([S,P,L,G]),
      (
        rdf(S, P, L, G),
        rdf_literal(L, LexicalForm, Datatype, LangTag),
        \+ rdf_literal_map(LexicalForm, Datatype, LangTag, _)
      ),
      IncorrectRows
    )
  },
  rdf_html_table(
    [header_row(splg),location(rdf_man_literals)],
    html([
      'Overview of incorrect RDF literals in RDF graph ',
      \rdf_graph_html(rdf_man_literals, G),
      '.'
    ]),
    IncorrectRows
  ).

