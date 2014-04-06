:- module(
  pair_ext,
  [
    ordsets_to_pairs/2, % +Sets:list(ordset)
                        % -Pairs:ordset(pair)
    pairs_to_members/2, % +Pairs:list(pair)
                        % -Members:list
    pairs_to_ordsets/2, % +Pairs:list(pair(iri))
                        % -Sets:list(ordset(iri))
    term_to_pair/2 % @Term
                   % -Pair:pair
  ]
).

/** <module> Pair extensions

Support predicates for working with pairs.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12, 2014/03
*/

:- use_remote_module(generics(list_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).



%! ordsets_to_pairs(+Sets:list(ordset), -Pairs:ordset(pair)) is det.

ordsets_to_pairs(Sets, Pairs):-
  ordsets_to_pairs(Sets, [], Pairs).

ordsets_to_pairs([], Sol, Sol).
ordsets_to_pairs([H|T], L1, Sol):-
  findall(
    X-Y,
    (
      member(X, Y, H),
      % No reflexive cases.
      X \== Y
    ),
    L2
  ),
  ord_union(L1, L2, L3),
  ordsets_to_pairs(T, L3, Sol).



%! pairs_to_members(+Pairs:list(pair), -Members:list) is det.

pairs_to_members(Pairs, Members):-
  pairs_keys_values(Pairs, Keys1, Values1),
  maplist(sort, [Keys1,Values1], [Keys2,Values2]),
  ord_union(Keys2, Values2, Members).



%! pairs_to_ordsets(
%!   +Pairs:list(pair(iri)),
%!   -Sets:ordset(ordset(iri))
%! ) is det.
% For instance, the following pairs:
% ~~~
% <a,b>
% <a,c>
% <d,e>
% ~~~
% result in the following set:
% ~~~
% {{a,b,c},{d,e}}
% ~~~

pairs_to_ordsets(Pairs, Sets):-
  pairs_to_ordsets(Pairs, [], Sets).

pairs_to_ordsets([], Sol, Sol).
% Connect two sets.
pairs_to_ordsets([X-Y|T], Sets1, Sol):-
  member(OldSet1, Sets1),
  member(X, OldSet1),
  member(OldSet2, Sets1),
  OldSet1 \== OldSet2,
  member(Y, OldSet2), !,
  ord_union(OldSet1, OldSet2, NewSet),
  ord_del_element(Sets1, OldSet1, Sets2),
  ord_del_element(Sets2, OldSet2, Sets3),
  ord_add_element(Sets3, NewSet, Sets4),
  pairs_to_ordsets(T, Sets4, Sol).
% Add to an existing set.
pairs_to_ordsets([X-Y|T], Sets1, Sol):-
  member(OldSet, Sets1),
  (
    member(X, OldSet)
  ->
    ord_add_element(OldSet, Y, NewSet)
  ;
    member(Y, OldSet)
  ->
    ord_add_element(OldSet, X, NewSet)
  ), !,
  ord_del_element(Sets1, OldSet, Sets2),
  ord_add_element(Sets2, NewSet, Sets3),
  pairs_to_ordsets(T, Sets3, Sol).
% New set.
pairs_to_ordsets([X-Y|T], Sets1, Sol):-
  list_to_ord_set([X,Y], NewSet),
  ord_add_element(Sets1, NewSet, Sets2),
  pairs_to_ordsets(T, Sets2, Sol).


%! term_to_pair(@Term, -Pair:pair) is det.
% Retrusn the pair notation `First-Second` if the given term
% can be interpreted as a pair.
%
% The following pair notations are recognized:
%   1. `X-Y`
%   2. `X=Y`
%   3. `[X,Y]`
%   4. `X(Y)`

term_to_pair(X-Y, X-Y):- !.
term_to_pair(X=Y, X-Y):- !.
term_to_pair([X,Y], X-Y):- !.
term_to_pari(Compound, X-Y):-
  Compound =.. [X,Y].



:- begin_tests(pair_ext).

% Base case.
pairs_to_ord_sets_example([], []).
% No multisets.
pairs_to_ord_sets_example([a-b,a-b], [[a,b]]).
% Reflexive case.
pairs_to_ord_sets_example([a-a], [[a]]).
% Symmetric case.
pairs_to_ord_sets_example([a-b,b-a], [[a,b]]).
% Separate sets.
pairs_to_ord_sets_example([a-b,c-d], [[a,b],[c,d]]).
% Merging sets.
pairs_to_ord_sets_example([a-b,c-d,d-b], [[a,b,c,d]]).

test(
  pairs_to_ordsets,
  [forall(pairs_to_ord_sets_example(Pairs,Sets)),true]
):-
  pairs_to_ordsets(Pairs, Sets).

:- end_tests(pair_ext).

