:- module(test, []).

:- meta_predicate(dcg_multi(//,+,-,?,?)).
:- meta_predicate(dcg_multi(//,+,+,-,?,?)).

a(a) --> "a".
b(b) --> "b".
ab(X) --> a(X).
ab(X) --> b(X).

dcg_multi(DCG, Min-Max, Args) -->
  dcg_multi(DCG, 0, Min-Max, Args).

dcg_multi(_DCG, Max, _Min-Max, []) --> [].
dcg_multi(DCG, C0, Min-Max, [H|T]) -->
  call(DCG, H),
  {C1 is C0 + 1},
  dcg_multi(DCG, C1, Min-Max, T).
dcg_multi(_DCG, C, Min-_Max, []) --> {C >= Min}.

:- initialization(test).
test:-
  dcg_multi(ab, 3-3, Args1, `aba`, []),
  writeln(Args1),
  dcg_multi((a ; b), 3-3, Args2, `aba`, []),
  writeln(Args2).
