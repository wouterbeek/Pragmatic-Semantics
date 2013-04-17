:- module(
  ilp_web,
  [
    ilp/1 % +File:atom
  ]
).

/** <module> ILP Web

Methods that form the Web interface to the ILP algorithms.

@author Wouter Beek
@version 2012/12
*/

:- use_module(ilp(ilp_script)).



ilp_web(File):-
  ilp(File).
