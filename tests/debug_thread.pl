:- initialization(go1).

go1:-
  thread_create(go2, _, []).

go2:-
  guitracer,
  gtrace, %DEB
  format(user_output, 'Monkey\n', []).
