:- module(semuri, []).

/** <module> Semantic URIs

@author Wouter Beek
@version 2014/01
*/

:- use_module(html(html_pl_term)).
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
  Header = [
    'Res',
    'Pack',
    'Org',
    'Users',
    'Tags',
    'Download',
    'MIME',
    'Arch',
    'Triples',
    'Steven'
  ],
  reply_html_page(
    app_style,
    title('Semantic URIs'),
    \html_table(
      [header(true),indexed(true)],
      `The Semantic Web Hoax`,
      ap_term,
      [Header|Table]
    )
  ).



% AP TERMS %

ap_message(Message) -->
  html(span(class=ap_message, html_pl_term(Message))).

ap_status(Status) -->
  html(span(class=ap_status, Status)).

ap_term(ap(status(Status),Message)) --> !,
  html(
    div(class=ap, [
      \ap_status(Status),
      \ap_message(Message)
    ])
  ).
ap_term(PL_Term) -->
  html_pl_term(PL_Term).

