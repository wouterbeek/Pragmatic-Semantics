:- module(
  dcg_peek,
  [
    dcg_peek//1, % :DCG_Body:dcg
    dcg_peek//2 % +Length:integer
                % ?Codes:list(code)
  ]
).

/** <module> DCG peek

Peeking in difference lists.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- meta_predicate(dcg_peek(//,?,?)).
:- meta_predicate(dcg_peek(?,?,?,?)).



%! dcg_peek(:DCG)// is det.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(DCG), DCG -->
  DCG.

%! dcg_peek(+Length:integer, ?Peek:list(code))// is nondet.

dcg_peek(Length, Peek), Peek -->
  {length(Peek, Length)},
  Peek.

