:- module(pl_dev, []).

/** <module> plDev

Web-based tools for Prolog development.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(uri_query)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(pl_web(pl_module)).
:- use_module(pl_web(pl_predicate)).
:- use_module(server(web_modules)).

:- meta_predicate(pl_dev_view(//,?,?)).

http:location(pl, root(pl), []).

:- http_handler(pl(dev), pl_dev, []).

user:web_module('plDev', pl_dev).



pl_dev(Request):-
  request_query_read(Request, module, Module), !,
  reply_html_page(
    app_style,
    \pl_dev_head(module(Module)),
    \pl_dev_body(module(Module))
  ).
pl_dev(Request):-
  request_query_read(Request, predicate, Predicate), !,
  reply_html_page(
    app_style,
    \pl_dev_head(predicate(Predicate)),
    \pl_dev_body(predicate(Predicate))
  ).
pl_dev(_Request):-
  reply_html_page(app_style, \pl_dev_head, \pl_dev_body).


pl_dev_backlink -->
  {http_absolute_location(pl(dev), Location, [])},
  html(div(id=backlink, a(href=Location, plDev))).


pl_dev_body -->
  pl_modules.

pl_dev_body(module(Module)) -->
  pl_dev_view(pl_module(Module)).
pl_dev_body(predicate(Predicate)) -->
  pl_dev_view(pl_predicate(Predicate)).


pl_dev_head -->
  html(title('plDev')).


pl_dev_head(Content) -->
  html(title(['plDev - ',\pl_dev_head_content(Content)])).


pl_dev_head_content(module(Module)) -->
  html(['Module ',Module]).
pl_dev_head_content(predicate(Predicate)) -->
  html(['Predicate ',Predicate]).

pl_dev_view(DCG) -->
  html(\pl_dev_backlink),
  DCG.

