:- module(
  subways,
  [
    subway_problem/2, % +InitialStation:atom
                      % +GoalStation:atom
    subway_problem/4 % +InitialStation:atom
                     % +GoalStation:atom
                     % -NumberExamined:nonneg
                     % -SolutionPath:list(term)
  ]
).

/** <module> Subways

A search problem for the Classical Problem Solver.

@author Wouter Beek
@version 2013/09
*/

:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_remote_module(math(math_ext)).
:- use_remote_module(ps(boston)).
:- use_remote_module(ps(cps)).



goal_recognizer(GoalState, State):-
  subway_states_identical(State, GoalState).

print_path_element(ToStation, take_line(_FromStation,Line,ToStation), Atom):-
  format(atom(Atom), 'Take the ~w to ~w.', [Line,ToStation]).

prune_subway_path([_,take_line(_,Line,_),_,take_line(_,Line,_)|_Path]).

state_printer(Station, Station).

subway_distance(
  [CurrentStation1|_Path1],
  [CurrentStation2|_Path2],
  EclideanDistance
):-
  station(CurrentStation1, _Lines1, X1, Y1),
  station(CurrentStation2, _Lines2, X2, Y2),
  euclidean_distance(
    coordinate(2,[X1,Y1]),
    coordinate(2,[X2,Y2]),
    EclideanDistance
  ).

subway_problem(InitialState, GoalState):-
  subway_problem(InitialState, GoalState, _NumberExamined, SolutionPath),
  with_output_to(
    current_output,
    print_answer(state_printer, print_path_element, SolutionPath)
  ).

subway_problem(InitialState, GoalState, NumberExamined, SolutionPath):-
  bsolve(
    goal_recognizer(GoalState),
    state_printer,
    [take_line],
    subway_operator_finder,
    prune_subway_path,
    subway_states_identical,
    InitialState,
    NumberExamined,
    SolutionPath
  ).

subway_operator_finder(
  Station1,
  take_line,
  [take_line(Station1,Line1,Station2),Station2]
):-
  station(Station1, Lines1, _X1, _Y1),
  member(Line1, Lines1),
  station(Station2, Lines2, _X2, _Y2),
  Station2 \== Station1,
  member(Line1, Lines2).

subway_states_identical(State, State).



:- begin_tests(subways).

travel('Airport', 'North-Station', _, _).

test(travel1, [forall(travel(From,To,Examined,Path)),true]):-
  subway_problem(From, To, Examined, Path).

:- end_tests(subways).

