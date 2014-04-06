:- module(
  html_tuple,
  [
    html_pair//2, % +Element1
                  % +Element2
    html_pair//3, % :WriteMethod
                  % +Element1
                  % +Element2
    html_pairs//2, % :WriteMethod
                   % +Pairs:list
    html_quadruple//4, % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_quadruple//5, % :WriteMethod
                       % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_triple//3, % +Element1
                    % +Element2
                    % +Element3
    html_triple//4, % :WriteMethod
                    % +Element1
                    % +Element2
                    % +Element3
    html_tuple//1, % +Elements:list
    html_tuple//2 % :WriteMethod
                  % +Elements:list
  ]
).

/** <module> HTML tuple

DCG rules for generating HTML descriptions of tuples,
including dedicated support for pairs, quadruples, and triples.

@author Wouter Beek
@version 2014/03
*/

:- use_module(dcg(dcg_meta)).
:- use_module(generics(pair_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(pl_web(html_pl_term)).

:- meta_predicate(html_pair(3,+,+,?,?)).
:- rdf_meta(html_pair(:,o,o,?,?)).

:- meta_predicate(html_pairs(3,+,?,?)).
:- rdf_meta(html_pairs(:,t,?,?)).

:- meta_predicate(html_quadruple(3,+,+,+,+,?,?)).
:- rdf_meta(html_quadruple(:,o,o,o,o,?,?)).

:- meta_predicate(html_triple(3,+,+,+,?,?)).
:- rdf_meta(html_triple(:,o,o,o,?,?)).

:- meta_predicate(html_tuple(3,+,?,?)).
:- meta_predicate(html_tuple(+,3,+,?,?)).
:- rdf_meta(html_tuple(:,t,?,?)).
:- rdf_meta(html_tuple(+,:,t,?,?)).

:- meta_predicate(html_tuple_elements(3,+,?,?)).
:- rdf_meta(html_tuple_elements(:,o,?,?)).



%! html_pair(+Element1, +Element2)// is det.
% @see html_pair//3

html_pair(E1, E2) -->
  html_pair(html_pl_term, E1, E2).


%! html_pair(:WriteMethod, +Element1, +Element2)// is det.
% Generates an HTML representation for a pair.

html_pair(Dcg, E1, E2) -->
  html_tuple(pair, Dcg, [E1,E2]).


%! html_pairs(:WriteMethod, +Pairs:list(pair))// is det.

html_pairs(_, []) --> [].
html_pairs(Dcg, [H|T]) -->
  {term_to_pair(H, X-Y)},
  html_pair(Dcg, X, Y),
  html_pairs(Dcg, T).


%! html_quadruple(+Element1, +Elememt2, +Element3, +Element4)// is det.
% @see html_quadruple//5

html_quadruple(E1, E2, E3, E4) -->
  html_quadruple(html_pl_term, E1, E2, E3, E4).


%! html_quadruple(:Dcg, +Element1, +Elememt2, +Element3, +Element4)// is det.
% Generates an HTML representation for a triple.

html_quadruple(Dcg, E1, E2, E3, E4) -->
  html_tuple(quadruple, Dcg, [E1,E2,E3,E4]).


%! html_triple(:Dcg, +Elements:list)// is det.
% @see html_tripl//2

html_triple(E1, E2, E3) -->
  html_triple(html_pl_term, E1, E2, E3).


%! html_triple(:Dcg, +Elements:list)// is det.
% Generates an HTML representation for a triple.

html_triple(Dcg, E1, E2, E3) -->
  html_tuple(triple, Dcg, [E1,E2,E3]).


%! html_tuple(+Elements:list)// is det.
% @see html_tuple//2

html_tuple(L) -->
  html_tuple(html_pl_term, L).


%! html_tuple(:Dcg, +Elements:list)// is det.
% Generates an HTML representation for a tuple.

html_tuple(Dcg, L) -->
  html_tuple(tuple, Dcg, L).


%! html_tuple(
%!   +Class:oneof([pair,quadruple,triple,tuple]),
%!   :Dcg,
%!   +Elements:list
%! )// is det.

html_tuple(Class, Dcg, L) -->
  html(
    span(class=Class, [
      &(lang),
      \html_tuple_elements(Dcg, L),
      &(rlang)
    ])
  ).


%! html_tuple_elements(:Dcg, +Elements:list)// is det.

html_tuple_elements(_, []) --> !, [].
html_tuple_elements(Dcg, [H]) --> !,
  html(span(class=element, \dcg_call(Dcg, H))).
html_tuple_elements(Dcg, [H|T]) -->
  html([
    span(class=element, \dcg_call(Dcg, H)),
    ',',
    \html_tuple_elements(Dcg, T)
  ]).

