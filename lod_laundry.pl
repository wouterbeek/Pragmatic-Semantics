:- module(lod_laundry, []).

/** <module> LOD laundry

@author Wouter Beek
@version 2014/05
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(rdf_file(rdf_serial)).
:- use_module(server(app_ui)). % HTML style.
:- use_module(server(web_modules)). % Web module registration.

http:location(ll_web, root(ll), []).
:- http_handler(ll_web(.), ll_web_home, [prefix]).

user:web_module('LOD Laundry', ll_web_home).

:- initialization(init_ll).

init_ll:-
  absolute_file_name(
    data('http/datahub.io/catalog'),
    File,
    [access(read),file_type(turtle)]
  ),
  rdf_load_any([], File, Pairs),
gtrace,
  write(Pairs).



ll_web_home(_Request):-
  reply_html_page(
    app_style,
    title('LOD Laundry'),
    html(['LOD Laundry'])
  ).

