:- module(
  pl_predicates,
  [
    pl_predicate//1, % :Predicate
    pl_predicates//2 % +Modules:atom
                     % +Predicates:list
  ]
).

/** <module> Prolog predicates

Web interface to Prolog predicates.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(uri_query)).
:- use_module(html(html_list)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(prolog_xref)).
:- use_module(pl_web(html_pl_generic)).
:- use_module(pl_web(html_pl_term)).
:- use_module(server(web_modules)).

:- meta_predicate(pl_predicate(:,?,?)).

http:location(pl, root(pl), []).
:- http_handler(pl(predicates), pl_predicates, []).

user:web_module('plPred', pl_predicates).



pl_predicate(Module:Functor/Arity) -->
  {
    length(Args, Arity),
    Predicate =.. [Functor|Args],
    % Enumerate all predicate properties.
    findall(
      [Name,Value],
      (
        predicate_property(Module:Predicate, Property),
        Property =.. [Name|T],
        (
          T == []
        ->
          Value = true
        ;
          T = [Value]
        )
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html([
      'Overview of predicate ',
      \html_predicate(Module, Functor, Arity),
      '.'
    ]),
    html_pl_term,
    Rows
  ).


% A single predicate term.
pl_predicates(Request):-
  request_query_read(Request, predicate, Predicate), !,
  reply_html_page(
    app_style,
    title(['Prolog predicates - Predicate ',Predicate]),
    \pl_predicate(Predicate)
  ).
pl_predicates(_Request):-
  reply_html_page(app_style, title('Prolog predicates'), html('TODO')).

pl_predicates(Module, [H|T]) -->
  pl_predicate(Module:H),
  pl_predicates(Module, T).
pl_predicates(_, []) --> [].

