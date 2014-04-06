:- module(
  boston,
  [
    line/1, % ?Line:atom
    station/4 % ?Station:atom
              % ?Lines:list(atom)
              % ?X_Coordinate:float
              % ?Y_Coordinate:float
  ]
).

/** <module> Boston

Use case for the subways problem.

@author Wouter Beek
@see Taken from the book _|Building Problem Solvers|_.
@version 2013/09
*/



line('Red-Line').
line('Green-Line').
line('Orange-Line').
line('Blue-Line').

station('Airport',           ['Blue-Line'],                 4.0,   1.0 ).
station('Aquarium',          ['Blue-Line'],                 3.75,  0.1 ).
station('Wood-Island',       ['Blue-Line'],                 5.0,   2.0 ).
station('State',             ['Blue-Line','Orange-Line'],   3.1,  -0.75).
station('Park-Street',       ['Green-Line','Red-Line'],     2.5,  -0.5 ).
station('Government-Center', ['Green-line','Blue-line'],    2.9,  -0.25).
station('Copley-Square',     ['Green-Line'],                1.0,  -1.0 ).
station('Boston-U',          ['Green-Line'],               -1.0,  -1.0 ).
station('North-Station',     ['Green-Line','Orange-Line'],  2.5,   0.75).
station('Haymarket',         ['Orange-Line','Green-Line'],  2.75,  0.5 ).
station('South-Station',     ['Red-Line'],                  3.0,  -1.0 ).
station('Washington',        ['Red-Line','Orange-Line'],    2.75, -0.75).
station('Kendall-Square',    ['Red-Line'],                  1.0,   0.0 ).
station('Central-Square',    ['Red-Line'],                 -1.0,   0.0 ).
station('Harvard-Square',    ['Red-Line'],                 -2.0,   1.0 ).

