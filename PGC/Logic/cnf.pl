:- module(
  cnf,
  [
    cnf/2 % +In:compound
          % -Out:compound
  ]
).

/** <module> Conjunctive Normal Form

@author Wouter Beek
@version 2014/02
*/



cnf(Formula1, CNF):-
  cnf_rule(Formula1, Formula2),
  cnf(Formula2, CNF).
cnf(and(X1,Y1), and(X2,Y2)):-
  cnf(X1, X2),
  cnf(Y1, Y2),
  (X1 \== X2 ; Y1 \== Y2).
cnf(or(X1,Y1), or(X2,Y2)):-
  cnf(X1, X2),
  cnf(Y1, Y2),
  (X1 \== X2 ; Y1 \== Y2).
cnf(not(X1),not(X2)):-
  cnf(X1, X2),
  X1 \== X2.
cnf(CNF, CNF).


% Eliminate EQUIVALENCES.
cnf_rule(equiv(X,Y), and(imp(X,Y),imp(Y,X))).

% Eliminate IMPLICATIONS.
cnf_rule(imp(X,Y), or(not(X),Y)).

% Move NOT inwards.
cnf_rule(not(or(X,Y)), and(not(X),not(Y))).
cnf_rule(not(and(X,Y)), or(not(X),not(Y))).
cnf_rule(not(not(X)), X).

% Distribute OR inwards.
cnf_rule(or(X,and(Y,Z)), and(or(X,Y),or(X,Z))).

