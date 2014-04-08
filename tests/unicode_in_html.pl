:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).
:- module(test, []).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).

:- http_handler(/, www, [prefix]).

:- http_server(http_dispatch, [port(8080)]).

www(_):-
  reply_html_page(title('Test'), [p('🔗'),p('↨')]).

