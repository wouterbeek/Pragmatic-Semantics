:- module(disjunction_dcg, []).
:- use_module(library(apply)).

:- meta_predicate(p(3,*,?,?)).
p(_DCG, [], Y, Y).
p(DCG, [H|T], Y1, Y3):-
  call(DCG, H, Y1, Y2),
  p(DCG, T, Y2, Y3).

:- meta_predicate(';'(2,2,?,?)).
';'(DCG1, _DCG2, Y1, Y2):-
  call(DCG1, Y1, Y2).
';'(_DCG1, DCG2, Y1, Y2):-
  call(DCG2, Y1, Y2).

q(C) --> [C].
r(D) --> [C], {D is C - 32}.

:- initialization(test).
test:-
  atom_codes(swiprolog, Codes),
  phrase(p((q ; r), Result), Codes),
  writeln(Result).

