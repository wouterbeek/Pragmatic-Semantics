:- module(
  rational_ext,
  [
    rational_div/3, % +In1:decimal
                    % +In2:decimal
                    % -Out:decimal
    rational_mod/3, % +In1:decimal
                    % +In2:decimal
                    % -Out:decimal
    rational_parts/3 % +Decimal:decimal
                     % -IntegerPart:integer
                     % -FractionalPart:between(0.0,1.0)
  ]
).

/** <module> RATIONAL_EXT

@author Wouter Beek
@version 2013/08
*/



rational_div(X, Y, Z):-
  Z is floor(X rdiv Y).

rational_mod(X, Y, Z):-
  rational_div(X, Y, DIV),
  Z is X - DIV * Y.

rational_parts(D, I, F):-
  rational_div(D, 1, I),
  rational_mod(D, 1, F).

