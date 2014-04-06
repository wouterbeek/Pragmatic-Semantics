:- module(
  sort_ext,
  [
    predsort_with_duplicates/3 % :Goal:atom
                               % +List:list
                               % -SortedList:list
  ]
).

/** <module> Sort extensions

Extensions for sorting lists.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12
*/

:- meta_predicate(predmerge_with_duplicates(2,+,+,-)).
:- meta_predicate(predmerge_with_duplicates(2,+,+,+,+,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-,-,-)).




%! predmerge_with_duplicates(+Predicate, +List1, +List2, -Solution)
% Merges the given lists based on the given sort predicate.
% @precondition It is assumed that both lists are themselves sorted.
% @arg Predicate The sort predicate. It should be tertiary, of the form
% <{ <, =, > }, Element1, Element2>.
%
% @arg List1 An ordered list.
% @arg List2 An ordered list.
% @arg Solution An ordered list.

predmerge_with_duplicates(_Predicate, [], MergeResult, MergeResult):- !.
predmerge_with_duplicates(_Predicate, MergeResult, [], MergeResult):- !.
predmerge_with_duplicates(Predicate, [H1 | T1], [H2 | T2], Result):-
  call(Predicate, Delta, H1, H2),
  predmerge_with_duplicates(Delta, Predicate, H1, H2, T1, T2, Result).

%! predmerge_with_duplicates(
%!   +Delta,
%!   +Predicate,
%!   +ElementHalf1,
%!   +ElementHalf2,
%!   +SortedListHalf1,
%!   +SortedListHalf2,
%!   -SortedList
%! ) is det.

% H1 > H2, so place H2 in front of the result, and run again with H1.
predmerge_with_duplicates(>, Predicate, H1, H2, T1, T2, [H2 | Result]):-
  predmerge_with_duplicates(Predicate, [H1 | T1], T2, Result).
% H1 = H2, so place both H1 and H2 in the result (the order does not matter).
predmerge_with_duplicates(=, Predicate, H1, H2, T1, T2, [H1, H2 | Result]):-
  predmerge_with_duplicates(Predicate, T1, T2, Result).
% H1 < H2, so place H1 in front of the result, and run again with H2.

predmerge_with_duplicates(<, Predicate, H1, H2, T1, T2, [H1 | Result]):-
  predmerge_with_duplicates(Predicate, T1, [H2 | T2], Result).

%! predsort_with_duplicates(
%!    +Predicate:atom,
%!    +UnsortedList:list,
%!    -SortedList:list
%! ) is det.
% Variation of the standard predicate predsort/3 that does keeps any
% duplicates (instead of removing them).
%
% @arg Predicate An atomic predicate name of a tertiary predicate.
% @arg UnsortedList ...
% @arg SortedList ...
% @see Slight alteration of predsort/3.

predsort_with_duplicates(Predicate, UnsortedList, SortedList):-
  length(UnsortedList, Length),
  predsort_with_duplicates(
    Predicate,
    Length,
    UnsortedList,
    _,
    SortedList
  ).

%! predsort_with_duplicates(
%!   +Predicate:atom,
%!   +Length:integer,
%!   -SortedListHalf:list(term),
%!   -UnsortedListHalf:list(term),
%!   -SortedList:ordset(term)
%! ) is det.
% The division between =SortedListHalf1= and =UnsortedListHalf2= is defined
% by =Length=, which is the approximate length of both lists.
% The =SortedListHalf= is sorted in this predicate. The
% =UnsortedListHalf= will be sorted in the next iteration.
%
% @arg Predicate The atomic name of a binary semideterministic predicate.
% @arg Length An integer.
% @arg SortedListHalf A list of terms that are already sorted.
% @arg UnsortedListHalf A list of terms that are not yet sorted.
% @arg SortedList An ordered set of terms.

% There are 2 more unsorted terms.
predsort_with_duplicates(
  Predicate,
  2,
  [H1,H2|TailUnsortedList],
  TailUnsortedList,
  SortedList
):- !,
  % We perform one last call to finalize the sorting.
  call(Predicate, Delta, H1, H2),
  sort_with_duplicates(Delta, H1, H2, SortedList).
% There is 1 more unsorted term.
predsort_with_duplicates(
  _Predicate,
  1,
  [H|UnsortedList],
  UnsortedList,
  [H]
):- !.
% There are no more unsorted terms.
predsort_with_duplicates(_Predicate, 0, UnsortedList, UnsortedList, []):- !.
% The recursive case.
predsort_with_duplicates(Predicate, Length, L1, L3, SortedList):-
  % Rounded division of the given length.
  HalfLength1 is Length // 2,
  plus(HalfLength1, HalfLength2, Length),
  predsort_with_duplicates(Predicate, HalfLength1, L1, L2, Result1),
  predsort_with_duplicates(Predicate, HalfLength2, L2, L3, Result2),

  % The two results are themselves ordered, but when put together they may
  % be not sorted anymore. This is what the merge does.
  predmerge_with_duplicates(Predicate, Result1, Result2, SortedList).

%! sort_with_duplicates(+Delta, +Element1, +Element2, -SortedList:list)
% Returns the sorted list of the two given elements according to Delta.

sort_with_duplicates(<, H1, H2, [H1,H2]).
sort_with_duplicates(=, H1, H2, [H1,H2]).
sort_with_duplicates(>, H1, H2, [H2,H1]).

