:- initialization(test).

portray(Term):-
  blob(Term, stream), !,
  write(monkey).

test:-
  open('README.md', read, Stream),
  write_term(Stream, [blob(portray)]),
  nl,
  halt.
