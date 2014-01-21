:- module(semuri, []).

/** <module> Semantic URIs

@author Wouter Beek
@version 2014/01
*/

:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(semuri(semuri_script)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- http_handler(root(semuri), semuri, []).

:- web_module_add('Semantic URIs', semuri).

:- dynamic(row/1).



semuri(_Request):-
  findall(
    Row,
    row(Row),
    Table
  ),
  reply_html_page(
    app_style,
    title('Semantic URIs'),
    \html_table(
      [caption('The Semantic Web Hoax'),header(true),indexed(true)],
      [['Resource','Package','Organization','People','Tags','Download','Archive']|Table]
    )
  ).

