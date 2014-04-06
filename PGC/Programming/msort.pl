:- module(
  msort,
  [
    msort/3,		% accept a comparison predicate, keep duplicates
    sort/3,		% accept a comparison predicate, discard duplicates
    merge/4,		% combine sorted lists keeping duplicates
    union/4,		% combine sorted lists discarding duplicates
    key_compare/3,	% so msort can do keysort.
    key_compare/4,	% generalisation of key_compare/3,
    reverse_compare/3,	% compare/3 with last two arguments swapped.
    reverse_key_compare/3,
    reverse_key_compare/4
  ]
).

/** <module> msort

The plain predicates can be recovered by passing compare/3:

~~~{.pl}
msort(L, S):- msort(compare, L, S).
sort( L, S):- sort(compare, L, S).
merge(L1, L1, M):- merge(compare, L1, L2, M).
ord_union(S1, S2, U):- union(compare, S1, S2, U).

"duplicates" = "things that compare equal".

# Comparator

Comparators are tertiary predicates whose first parameter is
  either `>`, `<`, or `=`, and whose second and third paramters
  are the compared terms.

# Concepts

  * *Stability*
    The property of a sorting algorithm that ensures that
    non-sorted elements occur in the same order in the result
    as in the input.

@author Richard O'Keefe
@inventor John von Neumann, 1945.
*/

:- meta_predicate
  msort(3, +, ?),		sort(3, +, ?),
  msort(+, +, -, -, 3),	sort(+, +, -, -, 3),
  merge(3, +, +, ?),		union(3, +, +, ?),
  merge_0(+, +, -, 3),	union_0(+, +, -, 3),
  merge_1(+, +, -, 3, +),	union_1(+, +, +, -, 3),
  merge_2(+, +, -, 3, +),	union_2(+, +, +, +, +, -, 3),
  merge_12(+, +, -, 3, +, +, +).

/* pred
	msort(+Cmp, +List, ?List),
	  msort(+integer, +List, -List, -List, +Cmp),
	sort( +Cmp, +List, ?List),
	  sort( +integer, +List, -List, -List, +Cmp),
        sort2(+order, +T, +T, -List),
	merge(+Cmp, +List, +List, ?List),
	  merge_0(+List, +List, -List, +Cmp),
		merge_1(+List, +List, -List, +Cmp, +T),
		merge_2(+List, +List, -List, +Cmp, +T),
		merge_12(+order, +List, -List, +Cmp, +T, +List, +T),
	union(+Cmp, +List, +List, ?List),
      union_0(+List, +List, -List, +Cmp),
        union_1(+List, +T, +List, -List, +Cmp),
          union_2(+order, +T, +List, +T, +List, -List, +Cmp),
    key_compare(-order, +pair(X,Y), +pair(X,Y)),
    key_compare(+integer, -order, T, T),
    reverse_compare(-order, +T, +T),
    reverse_key_compare(-order, +pair(X,Y), +pair(X,Y)),
    reverse_key_compare(+integer, -order, +pair(X,Y), +pair(X,Y))
   where
    Cmp = void(-order, +T, +T),
    List = list(T).
*/

%! msort(+Comparator, +List, ?SortedList)
% Merge sort without removing duplicates.
%
% @author Richard O'Keefe
% @comp O(n log n)
% @see http://en.wikipedia.org/wiki/Merge_sort

msort(C, List, Sorted):-
  length(List, N),
  (
    N >= 2
  ->
    msort(N, List, _, S, C),
    Sorted = S
  ;
    Sorted = List
  ).

%! msort(+Length:nonneg, +List, -List, +List, +Comparator)

msort(1, [X|Xs],   Xs, [X], _):- !.
msort(2, [X,Y|Xs], Xs, S,   C):- !,
  call(C, R, X, Y),
  % Enforce the order `<=`
  (
    R = (>)
  ->
    S = [Y,X]
  ;
    S = [X,Y]
  ).
msort(N, Xs0, Xs, S, C):-
  % Rounding towards zero.
  A is N // 2,
  Z is N - A,
  msort(A, Xs0, Xs1, S1, C),
  msort(Z, Xs1, Xs,  S2, C),
  merge_0(S1, S2, S, C).

merge(C, S1, S2, L):-
  merge_0(S1, S2, T, C),
  L = T.

%! merge_0(Xs, Ys, L, C)
% Here we know that the input lists Xs and Ys are sorted.

merge_0([],   L,  L, _).
merge_0([X|Xs], Ys, L, C):-
  % Take the head of the first merge list,
  % and use it as the first argument for the comparator.
  merge_1(Ys, Xs, L, C, X).

% merge_1(Ys, Xs, L, C, X) -- list 1 has been taken apart
% @arg X The first from Xs.

merge_1([],   Xs, [X|Xs], _, X).
merge_1([Y|Ys], Xs,   L,  C, X):-
  % Compare the first X with the first Y.
  call(C, R, X, Y),
  merge_12(R, Xs, L, C, X, Ys, Y).

% X is bigger than Y,
% so go back and keep looking for an Y that is bigger than X.
merge_12(>, Xs, [Y|L], C, X, Ys, Y):-
  merge_1(Ys, Xs, L, C, X).

merge_12(<, Xs, [X|L], C, X, Ys, Y):-
  merge_2(Xs, Ys, L, C, Y).
merge_12(=, Xs, [X|L], C, X, Ys, Y):-
  merge_2(Xs, Ys, L, C, Y).

% merge_2(Xs, Ys, L, C, Y) -- list 2 has been taken apart

merge_2([],   Ys, [Y|Ys], _, Y).
merge_2([X|Xs], Ys,   L,  C, Y):-
  call(C, R, X, Y),
  merge_12(R, Xs, L, C, X, Ys, Y).

sort(C, List, Sorted):-
  length(List, N),
  (
    N >= 2
  ->
    sort(N, List, _, S, C),
    Sorted = S
  ;
    Sorted = List
  ).

sort(1, [X|Xs],   Xs, [X], _):- !.
sort(2, [X,Y|Xs], Xs, S,   C):- !,
  call(C, R, X, Y),
  sort2(R, X, Y, S).
sort(N, Xs0, Xs, S, C):-
  A is N // 2,
  Z is N - A,
  sort(A, Xs0, Xs1, S1, C),
  sort(Z, Xs1, Xs,  S2, C),
  union_0(S1, S2, S, C).

sort2(<, X, Y, [X,Y]).
sort2(>, X, Y, [Y,X]).
sort2(=, X, _, [X]).

union(C, S1, S2, U):-
  union_0(S1, S2, S, C),
  U = S.

union_0([],    U,  U, _).
union_0([H1|T1], S2, U, C):-
  union_1(S2, H1, T1, U, C).

union_1([], H1, T1, [H1|T1], _).
union_1([H2|T2], H1, T1, U, C):-
  call(C, R, H1, H2),
  union_2(R, H1, T1, H2, T2, U, C).

union_2(<, H1, T1, H2, T2, [H1|U], C):-
  union_1(T1, H2, T2, U, C).
union_2(>, H1, T1, H2, T2, [H2|U], C):-
  union_1(T2, H1, T1, U, C).
union_2(=, H1, T1, _,  T2, [H1|U], C):-
  union_0(T1, T2, U, C).


%   Utility predicate to sort in reverse without reversing the
%   list.  Since this treats as equivalent only terms that are
%   identical,
%	msort(reverse_compare, L, S) <=> msort(compare, L, T), reverse(T, S)
%	sort( reverse_compare, L, S) <=> sort( compare, L, T), reverse(T, S).
%   For comparisons with other notions of equality, this is NOT true.
%   For example, the corresponding equivalences involving
%   key_compare/3 and reverse_key_compare/3 do NOT hold.

reverse_compare(R, X, Y):-
  compare(R, Y, X).

key_compare(R, X-_, Y-_):-
  compare(R, X, Y).

key_compare(N, R, A, B):-
  arg(N, A, X),
  arg(N, B, Y),
  compare(R, X, Y).

reverse_key_compare(R, X-_, Y-_):-
  compare(R, Y, X).

reverse_key_compare(N, R, A, B):-
  arg(N, A, X),
  arg(N, B, Y),
  compare(R, Y, X).

