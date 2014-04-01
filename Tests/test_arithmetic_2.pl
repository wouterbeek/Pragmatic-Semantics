:- module(test_arithmetic2, [xsd_div/3]).

:- use_module(library(arithmetic)).

:- arithmetic_function(xsd_div/2).

xsd_div(X, Y, Z):-
  Z is floor(X / Y).

