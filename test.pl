p(0, X, X).
p(N, A, B):-
  NN is N - 1,
  NA is A + 1,
  p(NN, NA, B).

q(0, X, X):-
  !.
q(N1, A1, B):-
  N2 is N1 - 1,
  A2 is A1 + 1,
  !,
  q(N2, A2, B).

s1([], 0).
s1([H|T], S2):-
  s1(T, S1),
  S2 is S1 + H.

s2(L, S):-
  s2(L, 0, S).

s2([], Sol, Sol).
s2([H|T], Temp1, Sol):-
  Temp2 is Temp1 + H,
  s2(T, Temp2, Sol).

go:-
  go1,
  go2.

go1:-
  findall(N, between(0, 10000000, N), Ns),
  time(s1(Ns, S1)),
  write(S1), nl.

go2:-
  findall(N, between(0, 10000000, N), Ns),
  time(s2(Ns, S2)),
  write(S2), nl.

