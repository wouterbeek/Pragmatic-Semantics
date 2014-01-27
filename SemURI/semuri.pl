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

ap_term(ap(status(Status),Message)) --> !,
  {atomic_list_concat([ap,Status], '_', Class)},
  html(div(class=Class, \ap_message(Message))).
ap_term(PL_Term) -->
  html_pl_term(PL_Term).


ap_message(download(File)) --> !,
  html(
    div(class=download, [
      div(class=action ,'downloaded'),
      \file(File)
    ])
  ).
ap_message(error(Formal,Context)) --> !,
  html_pl_term(error(Formal,Context)).
ap_message(extract_archive(OnFiles)) --> !,
  html(
    div(class=extract_archive, [
      div(class=action, 'extracted archive'),
      \on_files(OnFiles)
    ])
  ).
ap_message(mime(OfFiles)) --> !,
  html(
    div(class=mime, [
      div(class=action, 'MIME'),
      \of_files(OfFiles)
    ])
  ).
ap_message(Message) -->
  html(span(class=ap_message, html_pl_term(Message))).


file(File) -->
  html(span(class=file, File)).

nvpair(Property,Value) -->
  html([
    span(class=property, Property),
    '=',
    span(class=value, Value)
  ]).

of_file(of_file(File,nvpair(Property,Value))) -->
  html(
    div(class=of_file, [
      \file(File),
      ':',
      \nvpair(Property,Value)
    ])
  ).

of_files([]) --> !, [].
of_files([H|T]) -->
  of_file(H),
  of_files(T).

on_file(on_file(File,Operation)) -->
  html(
    div(class=on_file, [
      \operation(Operation),
      '@',
      \file(File)
    ])
  ).

on_files([]) --> !, [].
on_files([H|T]) -->
  html([
    \on_file(H),
    \on_files(T)
  ]).

operation(Operation) -->
  html(span(class=operation, Operation)).

