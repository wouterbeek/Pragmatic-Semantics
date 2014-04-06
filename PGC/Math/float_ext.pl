:- module(
  float_ext,
  [
    float_div/3, % +In1:float
                 % +In2:float
                 % -Out:float
    float_mod/3, % +In1:float
                 % +In2:float
                 % -Out:float
    float_parts/3, % +Float:float
                   % ?IntegerPart:integer
                   % ?FractionalPart:between(0.0,1.0)
    float_plus/3, % ?X:float
                  % ?Y:float
                  % ?Z:float
    to_float/2 % +Atomic:or([atom,float])
               % -Float:float
  ]
).

/** <module> Floating-point number extensions

Support predicates for floating point values.

@author Wouter Beek
@version 2013/08
*/



float_div(X, Y, Z):-
  Z is X / Y.


float_fractional_part2(N, N_F):-
  atom_number(N_A, N),
  sub_atom(N_A, N_I_Length, 1, _, '.'),
  succ(N_I_Length, Skip),
  sub_atom(N_A, Skip, _, 0, N_F_A),
  atom_number(N_F_A, N_F).


float_integer_part2(N, I):-
  I is integer(float_integer_part(N)).


float_mod(X, Y, Z):-
  float_div(X, Y, DIV),
  Z is X - DIV * Y.


float_parts(F, F_I, F_F):-
  float_integer_part2(F, F_I),
  float_fractional_part2(F, F_F).


%! float_plus(?X:number, ?Y:number, ?Z:number) is det.
% Calculates the sum Z = X + Y as long as at least two arguments are
% instantiated.
%
% @see The builin plus/3 only works for integers.

float_plus(X, Y, Z):-
  nonvar(X), nonvar(Y), !,
  Z is X + Y.
float_plus(X, Y, Z):-
  nonvar(X), nonvar(Z), !,
  Y is Z - X.
float_plus(X, Y, Z):-
  nonvar(Y), nonvar(Z), !,
  X is Z - Y.


%! to_float(+Atomic:or([atom,float]), -Float:float) is det.

to_float(Float, Float):-
  float(Float), !.
to_float(Atom, Float):-
  atom(Atom),
  atom_number(Atom, Number),
  to_float(Number, Float).

