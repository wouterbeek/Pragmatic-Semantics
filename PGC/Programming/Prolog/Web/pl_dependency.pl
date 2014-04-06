:- module(pl_dependencies, []).

/** <module> Prolog dependencies

Web interface to Prolog dependencies.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(uri_query)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(prolog_xref)).
:- use_module(pl_web(html_pl_term)).
:- use_module(server(web_modules)).

http:location(pl, root(pl), []).

:- http_handler(pl(dependencies), pl_dependencies, []).

user:web_module('plDep', pl_dependencies).



pl_dependencies(Request):-
  request_query_read(Request, source, Source), !,
  reply_html_page(
    app_style,
    title(['Prolog dependencies - Source ',Source]),
    \pl_dependencies(Source)
  ).
pl_dependencies(_Request):-
  reply_html_page(app_style, title('Prolog dependencies'), \pl_dependencies).


pl_dependencies -->
  {
    % Set of files for which we have cross-references.
    aggregate_all(
      set(Source),
      xref_defined(Source, _, _),
      Sources
    ),
    % Annotate files with the number of cross-references callables.
    findall(
      [Source,NumberOfCallables],
      (
        member(Source, Sources),
        aggregate_all(
          set(Callable),
          xref_defined(Source, Callable, _),
          Callables
        ),
        length(Callables, NumberOfCallables)
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html('Overview of cross-referenced sources'),
    pl_dependencies_term_html,
    [['Source','Number of callables']|Rows]
  ).


pl_dependencies(Source) -->
  {
    % The set of callables defined in source.
    aggregate_all(
      set(Callable),
      xref_defined(Source, Callable, _),
      Callables
    ),
    % For each callables, the files that import them.
    findall(
      [Callable,ImportingFiles],
      (
        member(Callable, Callables),
        aggregate_all(
          set(ImportingFile),
          (
            xref_defined(Source, Callable, How),
            How = imported(ImportingFile)
          ),
          ImportingFiles
        )
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html(['Overview of callables defined in source ',Source,'.']),
    pl_dependencies_term_html,
    [['Callable','Importing files']|Rows]
  ).


pl_dependencies_term_html(Source) -->
  {
    xref_current_source(Source), !,
    http_absolute_location(pl(dependencies), Location1, []),
    uri_query_add(Location1, source, Source, Location2)
  },
  html(span(class=source, a(href=Location2, Source))).
pl_dependencies_term_html(X) -->
  html_pl_term(X).

