:- initialization(test).

portray(Term):-
  blob(Term, Type),
  Type \== text, !,
  write(monkey).

test:-
  open('README.md', read, Stream),
  write_term(Stream, [blobs(portray)]),
  nl,
  halt.
