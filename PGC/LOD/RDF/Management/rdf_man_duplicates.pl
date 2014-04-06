:- module(rdf_man_duplicates, []).

/** <module> RDF duplicates

Support for visualizing and managing duplicates in an RDF store.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(server(web_modules)).

http:location(rdf,     root(rdf), []).
http:location(rdf_man, rdf(man),  []).
:- http_handler(rdf_man(duplicates), rdf_man_duplicates, []).

user:web_module('RDFm duplicates', rdf_man_duplicates).



rdf_man_duplicates(_Request):-
  reply_html_page(
    app_style,
    title('RDFm duplicates'),
    html(\rdf_man_duplicates)
  ).

rdf_man_duplicates -->
  {
    % Find all duplicate triples.
    aggregate_all(
      set([S,P,O]),
      (
        rdf(S, P, O, G1),
        rdf(S, P, O, G2),
        G1 @< G2
      ),
      DuplicateTriples
    ),
    findall(
      [S,P,O,Gs2],
      (
        member([S,P,O], DuplicateTriples),
        aggregate_all(
          set(G1),
          rdf(S, P, O, G1),
          Gs1
        ),
        % Remove line numbers / indices
        % if a triple is not a duplicate within a graph.
        once(maplist(remove_graph_index(Gs1), Gs1, Gs2))
      ),
      Rows
    )
  },
  html(
    \rdf_html_table(
      [header_row(spog),indexed(true),location(rdf_man_duplicates)],
      html('Overview of RDF duplicates accross all RDF graphs.'),
      Rows
    )
  ).


remove_graph_index(Graphs, Graph:Index1, Graph):-
  \+ ((
    member(Graph:Index2, Graphs),
    Index1 \= Index2
  )).
remove_graph_index(_, Graph, Graph).

