legal_state(flow:zero/inc,amount:zero/inc).
test:-
%  L=[legalstate(flow:zero/inc,amount:zero/inc)],

  legal_state(B) =.. L,
  print(B).