:- module(test_arithmetic1, []).

:- use_module(library(arithmetic)).

:- arithmetic_function(xsd_div/2).

xsd_div(X, Y, Z):-
  Z is floor(X / Y).

