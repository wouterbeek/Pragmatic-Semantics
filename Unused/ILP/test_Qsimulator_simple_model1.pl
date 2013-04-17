:- module(test_Qsimulator_simple_model1, []).

/** <module> QSIM Bratko

Implementation of the QSIM engine from Bratko's book on
Prolog for AI programmers.

QSIM is an interpreter for Qualitative Differential Equations

@author Sander Jetten
@version May 2012
*/

:- op( 100, xfx, ..).
:- op( 500, xfx, :).


% landmarks( Domain, [ Land1, Land2, ... ]
%   Land1, Land2 etc. are landmarks for domain Domain
%   This is part of qualitative model definition, user-defined

%  correspond( Constraint):
%    Constraint specifies corresponding values for a type of constr.

correspond( sum( _:zero, _:zero, _:zero)).

correspond( sum( Dom1:L, _:zero, Dom1:L)):-
  qmag( Dom1:L), L \== zero, not(L = _.._).   % L is nonzero landmark in Dom1

correspond( sum( _:zero, Dom2:L, Dom2:L)):-
  qmag( Dom2:L), L \== zero, not(L = _.._).   % L is nonzero landmark in Dom2

correspond( sum( V1, V2, V3)):-
  correspond( V1, V2, V3).                     % User-defined corr. values

% The following is a dummy definition of correspond/3
% just to avoid undefined-predicate error with some Prologs

correspond( dummy, dummy, dummy).

%  qmag( Domain:QualMagnitude)

qmag( Domain:Qm):-
  landmarks( Domain, Lands),
  qmag( Lands, Qm).

qmag( Lands, L):-
  member( L, Lands),
  L \== minf, L \== inf.           % A finite landmark

qmag( Lands, L1..L2):-           % Interval
  append( _, [L1,L2 | _], Lands).    % Two adjacent landmarks


%  relative_qmag( Domain1:QM, Domain2:Landmark, Sign):
%    Sign is the sign of the difference between QM and Landmark
%    if QM < Landmark then Sign = neg, etc.

relative_qmag( Domain:Ma.._, Domain:Land, Sign):-  !,
  landmarks( Domain, Lands),
  ( compare_lands( Ma, Land, Lands, neg), Sign = neg, !
    ;
    Sign = pos
  ).

relative_qmag( Domain:M1, Domain:M2, Sign):-
  landmarks( Domain, Lands),
  compare_lands( M1, M2, Lands, Sign), !.


% qdir( Qdir, Sign):
%   Qdir is qualitative direction of change with sign Sign

qdir( dec, neg).
qdir( std, zero).
qdir( inc, pos).

% Laws of qualitative summation

%  qsum( Q1, Q2, Q3):
%    Q3 = Q1 + Q2, qualitative sum over domain [pos,zero,neg]

qsum( pos, pos, pos).
qsum( pos, zero, pos).
qsum( pos, neg, pos).
qsum( pos, neg, zero).
qsum( pos, neg, neg).
qsum( zero, pos, pos).
qsum( zero, zero, zero).
qsum( zero, neg, neg).
qsum( neg, pos, pos).
qsum( neg, pos, zero).
qsum( neg, pos, neg). 
qsum( neg, zero, neg).
qsum( neg, neg, neg).


%  qdirsum( D1, D2, D3):
%    qualitative sum over directions of change

qdirsum( D1, D2, D3):-
  qdir( D1, Q1), qdir( D2, Q2), qdir( D3, Q3),
  qsum( Q1, Q2, Q3).

% sum( QV1, QV2, QV3):
%   QV1 = QV2 + QV3, 
%   qualitative sum over qualitative values of form Domain:Qmag/Dir
%   When called, this predicate assumes that the
%   domains of all three arguments are instantiated

sum( D1:QM1/Dir1, D2:QM2/Dir2, D3:QM3/Dir3):-
  qdirsum( Dir1, Dir2, Dir3),     % Directions of change: Dir1 + Dir2 = Dir3
  qmag( D1:QM1), qmag( D2:QM2), qmag( D3:QM3),
      % QM1+QM2=QM3 must be consistent with all corresponding values:
  not((
    correspond( sum( D1:V1, D2:V2, D3:V3)),     % V1 + V2 = V3
    relative_qmag( D1:QM1, D1:V1, Sign1),
    relative_qmag( D2:QM2, D2:V2, Sign2),
    relative_qmag( D3:QM3, D3:V3, Sign3),
    not(qsum( Sign1, Sign2, Sign3))) ).

%  mplus( X, Y):
%    Y is a monotonically increasing function of X

mplus( D1:QM1/Dir, D2:QM2/Dir):-     % Equal directions of change
  qmag( D1:QM1), qmag( D2:QM2),
    % QM1, QM2 consistent with all corresponding values between D1, D2:
  not(( correspond( D1:V1, D2:V2),
        relative_qmag( D1:QM1, D1:V1, Sign1),
        relative_qmag( D2:QM2, D2:V2, Sign2),
        Sign1 \== Sign2 )).

% deriv( Var1, Var2):
%   time derivative of Var1 is qualitatively equal Var2

deriv( _:_/Dir1, Dom2:Qmag2/_):-
  qdir( Dir1, Sign1),
  qmag( Dom2:Qmag2),
  relative_qmag( Dom2:Qmag2, Dom2:zero, Sign2), % Sign2 = sign of Qmag2
  Sign1 = Sign2.

% transition( Domain:Qmag1/Dir1, Domain:Qmag2/Dir2):
%   Variable state transitions between "close" time points

transition( Dom:L1..L2/std, Dom:L1..L2/Dir2):-
  qdir( Dir2, _).

transition( Dom:L1..L2/inc, Dom:L1..L2/inc).

transition( Dom:L1..L2/inc, Dom:L1..L2/std).

transition( Dom:_..L2/inc, Dom:L2/inc):-
  L2 \== inf.

transition( Dom:_..L2/inc, Dom:L2/std):-
  L2 \== inf.

transition( Dom:L1..L2/dec, Dom:L1..L2/dec).

transition( Dom:L1..L2/dec, Dom:L1..L2/std).

transition( Dom:L1.._/dec, Dom:L1/dec):-
  L1 \== minf.

transition( Dom:L1.._/dec, Dom:L1/std):-
  L1 \== minf.

transition( Dom:L1/std, Dom:L1/std):-
  L1 \== _.._.    % L1 not an interval

transition( Dom:L1/std, Dom:L1..L2/inc):-
  qmag( Dom:L1..L2).

transition( Dom:L1/std, Dom:L0..L1/dec):-
  qmag( Dom:L0..L1).

transition( Dom:L1/inc, Dom:L1..L2/inc):-
  qmag( Dom:L1..L2).

transition( Dom:L1/dec, Dom:L0..L1/dec):-
  qmag( Dom:L0..L1).

% system_trans( State1, State2):
%   System state transition;
%   system state is a list of variable values

system_trans( [], []).

system_trans( [Val1 | Vals1], [Val2 | Vals2]):-
  transition( Val1, Val2),
  system_trans( Vals1, Vals2).

% legal_trans( State1, State2):
%   possible transition between states according to model

legal_trans( State1, State2):-
  system_trans( State1, State2),
  State1 \== State2,            % Qualitatively different next state
  legalstate( State2).          % Legal according to model

% simulate( SystemStates, MaxLength):
%  SystemStates is a sequence of states of simulated system
%  not longer than MaxLength

simulate( [State], MaxLength):-
  ( MaxLength = 1                % Max length reached
    ;
    not(legal_trans( State, _))   % No legal next state
  ) , !.

simulate( [State1,State2 | Rest], MaxLength):-
  MaxLength > 1, NewMaxL is MaxLength - 1,
  legal_trans( State1, State2),
  simulate( [State2 | Rest], NewMaxL).

% simulate( InitialState, QualBehaviour, MaxLength)

simulate( InitialState, [InitialState | Rest], MaxLength):-
  legalstate( InitialState),             % Satisfy system's model
  simulate( [InitialState | Rest], MaxLength).


% compare_lands( X1, X2, List, Sign):
%   if X1 before X2 in List then Sign = neg 
%   if X2 before X1 then Sign = pos else Sign = zero 

compare_lands( X1, X2, [First | Rest], Sign):-
  X1 = X2, !, Sign = zero
  ;
  X1 = First, !, Sign = neg
  ;
  X2 = First, !, Sign = pos
  ;
  compare_lands( X1, X2, Rest, Sign).


% Figure 20.9  A qualitative model of bath tub.


% A bath tub model 

landmarks( level, [ zero, top, inf]).
landmarks( flow, [ zero, full]).

correspond( level:zero, flow:zero ).

legalstate( [ Level, Flow]):-
  mplus( Flow, Level),
  deriv( Flow, Level),

initial([level:zero/inc, flow:full/std]).

go:-
  initial(S),
  findall(B, simulate(S,B,10), Behaviours),
  pprint(Behaviours), !.

pprint([]).

pprint([H|T]):-
  print('Simulate from initial state: '),
  pprintlist(H),
  pprint(T).

pprintlist([]):-
  nl.

pprintlist([H|T]):-
  print(H), nl, pprintlist(T). 