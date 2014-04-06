:- module(rdf_mat_web, []).

/** <module> RDF materialization Web

Web-interface for RDF materialization.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/09, 2013/11-2014/01, 2014/03
*/

:- use_remote_module(generics(uri_query)).
:- use_remote_module(html(html_dropdown_list)).
:- use_remote_module(html(html_form)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_remote_module(rdf_term(rdf_term)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_reasoning(rdf_mat)).
:- use_remote_module(rdf_web(rdf_html_table)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(mat), rdf_mat_web, []).



rdf_mat_web(Request):-
  request_query_read(Request, graph, Graph),
  request_query_read(Request, regime, Graph), !,
  
  % Run materialization.
  materialize(
    [entailment_regimes([Regime]),multiple_justifications(false)],
    G
  ),

  % Display the triples in an HTML table.
  reply_html_page(
    app_style,
    title(['RDF Materialize -- Graph:',Graph,' Regime:',Regime]),
    [\recent_triples,\blank_node_legend]
  ).
rdf_mat_web(Request):-
  {
    findall(
      Graph-Graph,
      rdf_graph(Graph),
      GraphOptions
    ),
    findall(
      Regime-Regime,
      user:regime(Regime),
      RegimeOptions
    )
  },
  reply_html_page(
    app_style,
    title('RDF Materialize'),
    submission_form(
      rdf_mat_web,
      [
        html_dropdown_list(graph, `RDF graph`, GraphOptions),
        html_dropdown_list(regime, `Reasoning regime`, RegimeOptions),
        submit_button
      ]
    )
  ).

% Collect the legend for the blank nodes that occur in
%  at least one of the recently deduced triples.
blank_node_legend -->
  {
    aggregate_all(
      set([B,R,G]),
      (
        rdf_mat:recent_triple(S, _, O, G),
        
        % Blank nodes can occur in the subject and object positions.
        (
          B = S
        ;
          B = O
        ),
        rdf_is_bnode(B),
        
        % Look in the blank node-to-resource mapping for a mapped resource
        % for this blank node.
        b2r(G, B, R)
      ),
      L
    )
  },
  % Display the blank node mapping in an HTML table.
  html(
    \rdf_html_table(
      [],
      html('The blank node mapping that is used in the above results.'),
      [['Blank node','Mapped to','Graph']|L]
    )
  ).

% Collect all recently deduced triples.
recent_triples -->
  {
    aggregate_all(
      set([S,P,O,G]),
      rdf_mat:recent_triple(S, P, O, G),
      Quadruples
    )
  },
  html(
    \rdf_html_table(
      [header_row(spog)],
      html('The triples that were added in the last materialization run.'),
      Quadruples
    )
  ).

