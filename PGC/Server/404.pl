:- module(
  '404',
  [
    '404'/1 % +Request:list
  ]
).

/** <module> 404

Support for displaying 404 Web pages.

@author Wouter Beek
@version 2013/10, 2014/01
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

% All paths that do not have handlers are 404's.
:- http_handler(root(.), '404', [prefix,priority(100)]).



'404'(_Request):-
  http_absolute_uri(root(.), Home),
  reply_html_page(
    default_style,
    [title('404')],
    [h1('404'),a(href=Home,'Back to homepage')]
  ).

