:- module(semuri, []).

/** <module> Semantic URIs

@author Wouter Beek
@version 2014/01
*/

:- use_module(ap(html_ap_term)). % Used in meta-argument of html_table//4.
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(semuri(semuri_script)). % Make script predicates top-level accessible.
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
    'Arch',
    'MIME',
    'Triples',
    'VoID',
    'Compress',
    'CompressRnd'
  ],
  reply_html_page(
    app_style,
    title('Semantic URIs'),
    \html_table(
      [header(true),indexed(true)],
      `The Semantic Web Hoax`,
      html_ap_term,
      [Header|Table]
    )
  ).

