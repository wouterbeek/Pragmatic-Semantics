:- module(
  self_beh,
  [
    lefter/4, % +In1:integer
              % +In2:integer
              % +In3:integer
              % +Out1:integer
    middler/5, % +In1:integer
               % +In2:integer
               % +In3:integer
               % +Out1:integer
               % +Out2:integer
    righter/4 % +In1:integer
              % +In2:integer
              % +Out1:integer
              % +Out2:integer
  ]
).

/** <module> Self Behavior

Behavior rules for components in Self1993.

@author Wouter Beek
@version Sep 2012
*/



lefter(1, In2, In3, Out1):-
  Out1 is In2 - 1 - In3.
lefter(0, In2, In3, Out1):-
  Out1 is In2 - In3.

middler(1, In2, In3, Out1, Out2):-
  In2_ is In2 - 1,
  righter(In2_, In3, Out1, Out2).
middler(0, In2, In3, Out1, Out2):-
  righter(In2, In3, Out1, Out2).

righter(In1, In2, 0, Out2):-
  In1 >= In2,
  !,
  Out2 is In1 - In2.
righter(In1, In2, 1, Out2):-
  Out2 is 10 + In1 - In2.

