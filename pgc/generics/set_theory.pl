:- module(
  set_theory,
  [
    cardinality/2, % +Set:oneof([list,ordset])
                   % -Cardinality:integer
    delete_supersets/4, % +Original:ordset(ordset)
                        % +Compare:ordset(ordset)
                        % -Result:ordset(ordset)
                        % -Rest:ordset(ordset)
    equinumerous/2, % +Set1:oneof([list,ordset])
                    % +Set2:oneof([list,ordset])
    is_minimal/2, % +Set:ordset(object)
                  % +Sets:ordset(ordset(object))
    random_subset/2, % +Set:ordset
                     % -Subset:ordset
    subsets/2, % +Universe:list(object)
               % -Subsets:list(list(boolean))
    superset/2 % +Super:ordset
               % +Sub:ordset
  ]
).

/** <module> Set theory

Extra set functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/11-2011/12, 2012/02, 2012/08, 2012/10, 2013/05, 2013/12, 2014/03
*/

:- use_remote_module(generics(list_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_remote_module(pl(pl_mode)).



%! cardinality(+Set:oneof([list,ordset]), -Cardinality:integer) is det.
% Returns the cardinality of the given set.
%
% @arg Set An ordered set or a list.
% @arg Cardinality An integer representing the number of elements in the set.

cardinality(Set, Cardinality):-
  is_ordset(Set), !,
  length(Set, Cardinality).
cardinality(List, Cardinality):-
  is_list(List), !,
  list_to_ord_set(List, Set),
  cardinality(Set, Cardinality).


%! delete_supersets(
%!   +Original:ordset(ordset),
%!   +Compare:ordset(ordset),
%!   -Result:ordset(ordset),
%!   -Rest:ordset(ordset)
%! ) is det.
% Slits the `Original` sets into those that are and those that are not
%  a superset of some member of `Compare`.
%
% @arg Original The original sets.
% @arg Compare The sets we compare with.
% @arg Result The original sets that are supersets of some set in `Compare`.
% @arg Rest The original sets that are not in `Result`.

delete_supersets(Original, Compare, Result, Rest):-
  partition(contains_superset_of(Compare), Original, Rest, Result).
contains_superset_of(Compare, Set):-
  member(Superset, Compare),
  superset(Superset, Set), !.


%! equinumerous(
%!   +Set1:oneof([list,ordset]),
%!   +Set2:oneof([list,ordset])
%! ) is semidet.
% Succeeds if the given sets are equinumerous, i.e.,
% if they have the same cardinality.
%
% @arg Set1 An ordered set or a list.
% @arg Set2 An ordered set or a list.

% @see cardinality/2 takes care of the list-to-set conversion.
equinumerous(Set1, Set2):-
  cardinality(Set1, Cardinality),
  cardinality(Set2, Cardinality).


%! is_minimal(+Minimal:ordset, +Compare:ordset(ordset)) is semidet.
% Succeeds for minimal sets with respect to a set of sets.
%
% A minimal set has no subset.
%
% @arg Set A minimal set?
% @arg Sets A set of sets.

is_minimal(Minimal, Compare):-
  \+((
    member(Subset, Compare),
    subset(Subset, Minimal)
  )).


%! random_subset(+Set:ordset, -Subset:ordset) is det.

random_subset(S1, S2):-
  random_sublist(S1, S2).


%! subsets(+Set:ordset, -Subsets:list(list(bit))) is det.
% Returns all subsets of the given set as a list of binary lists.
%
% @arg Set An ordered set.
% @arg Subsets A list of bitlists representing subsets of =Set=.

subsets(Set, Subsets):-
  cardinality(Set, Cardinality),
  repeating_list(0, Cardinality, BinarySet),
  call_complete(next_subset, BinarySet, BinarySubsets),
  maplist(binary_overlay(Set), BinarySubsets, Subsets).

%! binary_overlay(+Original:list, +Overlay:list, -Result:list) is det.
%! binary_overlay(+Original:list, -Overlay:list, +Result:list) is det.
%! binary_overlay(+Original:list, ?Overlay:list, ?Result:list) is nondet.
% The result is the sublist of the original list,
%  where the overlay decides on which elements are kept (`1`)
%  and which are not (`0`).

binary_overlay(Original, Overlay, Result):-
  enforce_mode(
    '_binary_overlay'(Original, Overlay, Result),
    [Original,Overlay,Result],
    [[+,+,+]-semidet,[+,+,-]-det,[+,-,+]-det]
  ).
'_binary_overlay'([], [], []).
'_binary_overlay'([H|T1], [1|Overlay], [H|T2]):-
  '_binary_overlay'(T1, Overlay, T2).
'_binary_overlay'([_|T1], [0|Overlay], T2):-
  '_binary_overlay'(T1, Overlay, T2).


%! next_subset(+Subset:list(bit), -NextSubset:list(bit)) is det.
% Returns the next subset.
%
% Subsets are represented as lists of bits.
%
% Positions in the list correspond to potential elements in the set.
% For example
% ~~~{.pl}
% [1,0,0,1,1,0]
% ~~~
% may denote the set
% ~~~{.txt}
% {a,d,e}
% ~~~
%
% @arg Subset A list of bits.
% @arg NextSubset A list of bits.

next_subset([0|T], [1|T]).
next_subset([1|T1], [0|T2]):-
  next_subset(T1, T2).


%! superset(+Super:ordset, +Sub:ordset) is semidet.
% Mainly used with `library(apply)`.
%
% @see Inverse parameter order of ord_subset/2.

superset(Super, Sub):-
  ord_subset(Sub, Super).

