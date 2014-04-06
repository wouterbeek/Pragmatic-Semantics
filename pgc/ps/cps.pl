:- module(
  cps,
  [
    bsolve/9, % :GoalRecognizer
              % :StatePrinter
              % +Operators:list(callable)
              % :OperatorApplier
              % :PathFilter
              % :StatesIdentical
              % +InitialState:term
              % -NumberExamined:nonneg
              % -SolutionPath:list(term)
    dsolve/9, % :GoalRecognizer
              % :StatePrinter
              % +Operators:list(callable)
              % :OperatorApplier
              % :PathFilter
              % :StatesIdentical
              % +InitialState:term
              % -NumberExamined:nonneg
              % -SolutionPath:list(term)
    print_answer/3 % :StatePrinter,
                   % :SolutionElementPrinter,
                   % +Path:list(term)
  ]
).

/** <module> CPS

Classical Problem Solving.

@author Wouter Beek
@see Kenneth D. Forbus and Johan de Kleer, 1993, Building Problem Solvers.
@version 2013/09
*/

:- use_module(library(debug)).
:- use_module(library(lists)).

:- meta_predicate(bsolve(1,2,+,3,1,2,+,-,-)).
:- meta_predicate(bdsolve(+,1,2,+,3,1,2,+,-,-)).
:- meta_predicate(dsolve(1,2,+,3,1,2,+,-,-)).
:- meta_predicate(extend_path(+,3,1,2,+,-)).
:- meta_predicate(path_has_loop(2,+)).
:- meta_predicate(print_answer(2,3,+)).
:- meta_predicate(print_answer_(3,+)).



%! bsolve(
%!   :GoalRecognizer,
%!   :StatePrinter,
%!   +Operators:list(callable),
%!   :OperatorApplier,
%!   :PathFilter,
%!   :StatesIdentical,
%!   +InitialState:term,
%!   -NumberExamined:nonneg,
%!   -SolutionPath:list(term)
%! ) is semidet.

bsolve(
  GoalRecognizer,
  StatePrinter,
  Operators,
  OperatorApplier,
  PathFilter,
  StatesIdentical,
  InitialState,
  NumberExamined,
  SolutionPath
):-
  flag('number-examined', _, 0),
  bdsolve(
    breadth,
    GoalRecognizer,
    StatePrinter,
    Operators,
    OperatorApplier,
    PathFilter,
    StatesIdentical,
    [[InitialState]],
    NumberExamined,
    SolutionPath
  ).

%! dsolve(
%!   :GoalRecognizer,
%!   :StatePrinter,
%!   +Operators:list(callable),
%!   :OperatorApplier,
%!   :PathFilter,
%!   :StatesIdentical,
%!   +InitialState:term,
%!   -NumberExamined:nonneg,
%!   -SolutionPath:list(term)
%! ) is semidet.

dsolve(
  GoalRecognizer,
  StatePrinter,
  Operators,
  OperatorApplier,
  PathFilter,
  StatesIdentical,
  InitialState,
  NumberExamined,
  SolutionPath
):-
  flag('number-examined', _, 0),
  bdsolve(
    depth,
    GoalRecognizer,
    StatePrinter,
    Operators,
    OperatorApplier,
    PathFilter,
    StatesIdentical,
    [[InitialState]],
    NumberExamined,
    SolutionPath
  ).

% Search space exhausted.
bdsolve(
  _Mode,
  _GoalRecognizer,
  _StatePrinter,
  _Operators,
  _OperatorApplier,
  _PathFilter,
  _StatesIdentical,
  [],
  _NumberExamined,
  _SolutionPath
):- !,
  fail.
% Goal state reached.
bdsolve(
  _Mode,
  GoalRecognizer,
  StatePrinter,
  _Operators,
  _OperatorApplier,
  _PathFilter,
  _StatesIdentical,
  [[CurrentState|Path]|_Paths],
  NumberExamined,
  [CurrentState|Path]
):-
  % No cut; nondet.
  call(GoalRecognizer, CurrentState),
  call(StatePrinter, CurrentState, CurrentStateMsg),
  debug(cps, 'Found goal state: ~w', [CurrentStateMsg]),
  flag('number-examined', NumberExamined, NumberExamined).
% Expand a state.
bdsolve(
  Mode,
  GoalRecognizer,
  StatePrinter,
  Operators,
  OperatorApplier,
  PathFilter,
  StatesIdentical,
  [[CurrentState|Path]|Paths],
  NumberExamined,
  SolutionPath
):-
  % DEB: Print the currently considered path
  %      (we only show the path's current state).
  call(StatePrinter, CurrentState, CurrentStateMsg),
  debug(cps, 'State explored: ~w', [CurrentStateMsg]),

  % Extend the currently considered path.
  extend_path(
    Operators,
    OperatorApplier,
    PathFilter,
    StatesIdentical,
    [CurrentState|Path],
    ExtendedPaths
  ),

  % DEB: Print the extended paths.
  debug(cps, 'New operator instances: ', []),
  % DEB: The `PrintNewPaths` predicate may have an intelligent way
  %      of representing multiple paths in one message.
  %      We therefore run it once for all extended paths,
  %      rather than running it once for each extended path.
  print_new_paths(ExtendedPaths),

  % This is the breadth-first search.
  (
    Mode == breadth
  ->
    append(Paths, ExtendedPaths, NewPaths)
  ;
    Mode == depth
  ->
    append(ExtendedPaths, Paths, NewPaths)
  ),

  % Update statistics.
  flag('number-examined', Id, Id+1),

  bdsolve(
    Mode,
    GoalRecognizer,
    StatePrinter,
    Operators,
    OperatorApplier,
    PathFilter,
    StatesIdentical,
    NewPaths,
    NumberExamined,
    SolutionPath
  ).

%! extend_path(
%!   +Operators:list(callable),
%!   :OperatorApplier,
%!   :PathFilter,
%!   :StatesIdentical,
%!   +Path:list(term),
%!   -ExtendedPaths:list(list(term))
%! ) is det.

extend_path(
  Operators,
  OperatorApplier,
  PathFilter,
  StatesIdentical,
  [CurrentState|Path],
  ExtendedPaths
):-
  findall(
    [NextState,OperatorInstance,CurrentState|Path],
    (
      member(Operator, Operators),
      call(
        OperatorApplier,
        CurrentState,
        Operator,
        [OperatorInstance,NextState]
      ),
      \+ path_has_loop(StatesIdentical, [NextState,CurrentState|Path]),
      \+ call(PathFilter, [NextState,CurrentState|Path])
    ),
    ExtendedPaths
  ).

%! path_has_loop(:StatesIdentical, +Path:list(term)) is semidet.
% Succeeds if the given path contains at least two states
% that are identical according to the given predicate.

path_has_loop(_StatesIdentical, []):- !,
  fail.
path_has_loop(StatesIdentical, [State,_OperatorInstance|Path]):-
  call(StatesIdentical, State, Path), !.
path_has_loop(StatesIdentical, [_State,_OperatorInstance|Path]):-
  path_has_loop(StatesIdentical, Path).

%! print_answer(
%!   :StatePrinter,
%!   :SolutionElementPrinter,
%!   +Path:list(term)
%! ) is det.

print_answer(StatePrinter, SolutionElementPrinter, Path1):-
  reverse(Path1, [InitialState|Path2]),
  call(StatePrinter, InitialState, InitialStateMsg),
  format('\nInitial state: ~w.', [InitialStateMsg]),
  flag(step, _, 0),
  print_answer_(SolutionElementPrinter, Path2).
print_answer_(_SolutionElementPrinter, []):- !,
  format('\n Done.', []).
print_answer_(SolutionElementPrinter, [OperatorInstance,State|Path]):-
  call(
    SolutionElementPrinter,
    State,
    OperatorInstance,
    OperatorInstanceStateMsg
  ),
  flag(step, Step, Step+1),
  format('\n~w.  ~w', [Step,OperatorInstanceStateMsg]),
  print_answer_(SolutionElementPrinter, Path).

%! print_new_paths(+Paths:list(term)) is det.

print_new_paths(Paths):-
  maplist(print_new_path, Paths),
  debug(cps, '.', []).
print_new_path([_CurrentState,OperatorInstance|_Path]):-
  debug(cps, '  ~w', [OperatorInstance]).

