:- module(rdf_tabular_webqr, []).

/** <module> RDF Tabular WebQR

WebQR-specific extensions for RDF Tabular.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular)).
:- use_module(rdf_web(rdf_tabular_graph)).
:- use_module(server(web_modules)).
:- use_module(webqr(webqr_generic)).

:- http_handler(rdf_tabular(webqr), rdf_tabular_webqr, []).

user:web_module('WebQR Tabular', rdf_tabular_webqr).



rdf_tabular_webqr(Request):-
  % Retrieve the user's global graph.
  request_to_user_name(Request, User),
  reply_html_page(
    app_style,
    title(['WebQR - User ',User]),
    html([
      \rdf_tabular_webqr_global(User),
      \rdf_tabular_webqr_local(User)
    ])
  ).


rdf_tabular_webqr_global(User) -->
  {webqr_global_graph(User, GlobalGraph)},
  
  % Enumerate the user's public data -- or QR model -- in an HTML table.
  html([
    h2(['Public data of user ',User]),
    \rdf_tabular_triples(_, _, _, GlobalGraph)
  ]).


rdf_tabular_webqr_local(User) -->
  {webqr_local_graph(User, LocalGraph)},
  
  % Enumerate the user's private data in an HTML table.
  html([
    h2(['Private data of user ',User]),
    \rdf_tabular_triples(_, _, _, LocalGraph)
  ]).

