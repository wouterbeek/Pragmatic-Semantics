:- module(atms_debug, []).

/** <module> ATMS_DEB

Debug methods for ATMSs.

@author Wouter Beek
@version 2011
*/

:- use_module(atms(atms_db)).
:- use_module(atms_export).


test1:-
  find_or_add_atms(test1, ATMS),
  find_or_add_node(ATMS, a, A),
  find_or_add_node(ATMS, b, B),
  find_or_add_node(ATMS, c, C),
  find_or_add_node(ATMS, d, D),
  find_or_add_node(ATMS, e, E),
  find_or_add_node(ATMS, f, F),
  assume_node(A),
  assume_node(B),
  assume_node(C),
  find_or_add_justification(j1, D, [A, B], _J1),
  find_or_add_justification(j2, E, [B, C], _J2),
  find_or_add_justification(j3, F, [D, E], _J3),
  print_atms(ATMS, ''),
  nogood_nodes(ATMS, 'lala', [A, B]),
  print_atms(ATMS, 'Nogood added {A, B}').

test2:-
  find_or_add_atms(test2, ATMS),
  find_or_add_node(ATMS, a, A),
  find_or_add_node(ATMS, b, B),
  find_or_add_node(ATMS, c, C),
  find_or_add_node(ATMS, 'x=1', X),
  find_or_add_node(ATMS, 'y=x', YX),
  find_or_add_node(ATMS, 'x=z', XZ),
  find_or_add_node(ATMS, 'y=1', Y),
  find_or_add_node(ATMS, 'z=1', Z),
  assume_node(A),
  assume_node(B),
  assume_node(C),
  find_or_add_justification(j1, X, [A], _J1),
  find_or_add_justification(j2, YX, [B], _J2),
  find_or_add_justification(j3, XZ, [C], _J3),
  print_atms(ATMS, ''),
  nogood_nodes(ATMS, 'nogood', [A, B]),
  print_atms(ATMS, 'Now register nogood {A,B}'),
  find_or_add_justification(j4, Y, [X, YX], _J4),
  print_atms(ATMS, 'x=1, y=x => y=1'),
  find_or_add_justification('Premise', Z, [], _JP),
  print_atms(ATMS, 'We have a premise z=1'),
  find_or_add_justification(j5, X, [Z, XZ], _J5),
  print_atms(ATMS, 'z=1, x=z => x=1').
