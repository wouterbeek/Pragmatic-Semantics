:- module(
  math_ext,
  [
    average/2, % +Numbers:list(number)
               % -Average:number
    binomial_coefficient/3, % +M:integer
                            % +N:integer
                            % -BinomialCoefficient:integer
    circumfence/2, % +Radius:float
                   % -Circumfence:float
    combinations/3, % +NumberOfObjects:integer
                    % +CombinationLength:integer
                    % -NumberOfCombinations:integer
    count_down/2, % ?From:or([integer,oneof([inf])])
                  % ?To:or([integer,oneof([inf])])
    cyclic_numlist/4, % +Min:integer
                      % +Max:integer
                      % +CycleLength:integer
                      % -NumList:list(integer)
    div/3,
    div_zero/3,
    euclidean_distance/3, % +Coordinate1:coordinate
                          % +Coordinate2:coordinate
                          % -EuclideanDistance:float
    even/1, % +Integer:integer
    factorial/2, % +N:integer
                 % -F:integer
    fibonacci/2, % ?Index:integer
                 % ?Fibonacci:integer
    log/3, % +Base:integer
           % +X:float
           % +Y:float
    minus/3, % ?X:number
             % ?Y:number
             % ?Z:number
    minus_list/3, % +N:number
                  % +Ms:list(number)
                  % -N_Minus_Ms:number
    mod/3,
    multiply_list/2, % +Numbers:list(number)
                     % -Multiplication:number
    number_parts/3, % ?Number:number
                         % ?IntegerComponent:integer
                         % ?FractionalComponent:integer
    number_length/2, % +Number:number
                     % -Length:integer
    number_length/3, % +Number:number
                     % +Radix:integer
                     % -Length:integer
    odd/1, % +Integer:integer
    permutations/2, % +NumberOfObjects:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumbersOfObjects:list(integer)
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumberOfObjects:integer
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    pred/2, % +X:integer
            % -Y:integer
    rbetween/3, % +Low:integer
                % +High:integer
                % ?Value:integer
    square/2 % +X:float
             % -Square:float
  ]
).

/** <module> Artihmetic extensions for SWI-Prolog

Extra arithmetic functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/08-2012/02, 2012/09-2012/10, 2012/12, 2013/07-2013/09
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(math(float_ext)).
:- use_module(math(int_ext)).
:- use_module(math(rational_ext)).



average([], 0.0):- !.
average(Numbers, Average):-
  sum_list(Numbers, Sum),
  length(Numbers, NumberOfNumbers),
  Average is Sum / NumberOfNumbers.

binomial_coefficient(M, N, BC):-
  factorial(M, F_M),
  factorial(N, F_N),
  MminN is M - N,
  factorial(MminN, F_MminN),
  BC is F_M / (F_N * F_MminN).

%! circumfence(+Radius:float, -Circumfence:float) is det.
% Returns the circumfence of a circle with the given radius.

circumfence(Radius, Circumfence):-
  Circumfence is Radius * pi * 2.

%! combinations(
%!   +NumberOfObjects:integer,
%!   +CombinationLength:integer,
%!   -NumberOfCombinations:integer
%! ) is det.
% Returns the number of combinations from the given objects and
% of the given size.
%
% *Definition*: A combination is a permutation in which the order
%               neglected. Therefore, $r!$ permutations correspond to
%               one combination (with r the combination length).

combinations(NumberOfObjects, CombinationLength, NumberOfCombinations):-
  permutations(NumberOfObjects, CombinationLength, NumberOfPermutations),
  factorial(CombinationLength, F),
  NumberOfCombinations is NumberOfPermutations / F.


%! count_down(
%!   +From:or([integer,oneof([inf])]),
%!   -To:or([integer,oneof([inf])])
%! ) is det.
%! count_down(
%!   -From:or([integer,oneof([inf])]),
%!   +To:or([integer,oneof([inf])])
%! ) is det.
% Decrements an integer, allowing the value `inf` as well.

count_down(inf, inf):- !.
count_down(N1, N2):-
  succ(N2, N1).


%! cyclic_numlist(
%!   +Min:integer,
%!   +Max:integer,
%!   +CycleLength:integer,
%!   -NumList:list(integer)
%! ) is det.
% Generates a number list for a cyclic list of numbers.
% This method works on a off-by-zero basis.
% We return the numbers in a sorted order.

cyclic_numlist(Min, Max, _CycleLength, NumList):-
  Min < Max, !,
  numlist(Min, Max, NumList).
cyclic_numlist(Min, Max, CycleLength, NumList):-
  Top is CycleLength - 1,
  numlist(Min, Top, HigherNumList),
  numlist(0, Max, LowerNumList),
  append(LowerNumList, HigherNumList, NumList).

% @tbd
div(X, Y, Z):-
  rational(X), rational(Y), !,
  rational_div(X, Y, Z).
div(X, Y, Z):-
  float_div(X, Y, Z).

div_zero(X, 0, 0):-
  integer(X), !.
div_zero(X, 0.0, 0.0):-
  float(X), !.
div_zero(X, Y, Z):-
  Z is X / Y.

%! euclidean_distance(
%!   +Coordinate1:coordinate,
%!   +Coordinate2:coordinate,
%!   -EuclideanDistance:float
%! ) is det.
% Returns the Euclidean distance between two coordinates.

euclidean_distance(
  coordinate(Dimension, Args1),
  coordinate(Dimension, Args2),
  EuclideanDistance
):-
  maplist(minus, Args1, Args2, X1s),
  maplist(square, X1s, X2s),
  sum_list(X2s, X2),
  EuclideanDistance is sqrt(X2).

%! even(+Number:number) is semidet.
% Succeeds if the integer is even.

even(N):-
  mod(N, 2, 0).

%! factorial(+N:integer, -F:integer) is det.
% Returns the factorial of the given number.
%
% The standard notation for the factorial of _|n|_ is _|n!|_.
%
% *Definition*: $n! = \prod_{i = 1}^n i$

factorial(N, F):-
  numlist(1, N, Numbers), !,
  multiply_list(Numbers, F).
% E.g., $0!$.
factorial(_N, 1).

fibonacci(0, 1):- !.
fibonacci(1, 1):- !.
fibonacci(N, F):-
  N1 is N - 1,
  N2 is N - 2,
  fibonacci(N1, F1),
  fibonacci(N2, F2),
  F is F1 + F2.

%! log(+Base:integer, +X:integer, -Y:double) is det.
% Logarithm with arbitrary base =|Y = log_{Base}(X)|=.
%
% @arg Base An integer.
% @arg X An integer.
% @arg Y A double.

log(Base, X, Y):-
  Numerator is log(X),
  Denominator is log(Base),
  Y is Numerator / Denominator.

minus(X, Y, Z):-
  nonvar(X), nonvar(Y), !,
  Z is X - Y.
minus(X, Y, Z):-
  nonvar(X), nonvar(Z), !,
  Y is X - Z.
minus(X, Y, Z):-
  nonvar(Y), nonvar(Z), !,
  X is Y + Z.

%! minus_list(+N:number, +Ms:list(number), -N_Minus_Ms:number) is det.
% Subtracts the given numbers for the given start number
% and returns the result.

minus_list(N, Ms, N_Minus_Ms):-
  sum_list(Ms, M),
  N_Minus_Ms is N - M.

mod(X, Y, Z):-
  rational(X), rational(Y), !,
  rational_mod(X, Y, Z).
mod(X, Y, Z):-
  float_mod(X, Y, Z).

%! multiply_list(+List:list(number), -Multiplication:number) is det.
% Multiplies the numbers in the given list.
%
% @arg List A list of numbers.
% @arg Multiplication A number.
% @see Extends the builin list manipulators sum_list/2, max_list/2
%      and min_list/2.

multiply_list([], 0):- !.
multiply_list([Number], Number):- !.
multiply_list([Number | Numbers], Multiplication):-
  multiply_list(Numbers, Multiplication1),
  Multiplication is Number * Multiplication1.

number_parts(N, N_I, N_F):-
  var(N), !,
  number_length(N_F, N_F_Length),
  N is N_I + N_F / 10 ** N_F_Length.
number_parts(N, N_I, N_F):-
  integer(N), !,
  int_parts(N, N_I, N_F).
number_parts(N, N_I, N_F):-
  rational(N), !,
  rational_parts(N, N_I, N_F).
number_parts(N, N_I, N_F):-
  float(N), !,
  float_parts(N, N_I, N_F).

%! number_length(+Number:number, -Length:integer) is det.
% @see number_length/3 with radix set to `10` (decimal).

number_length(M, L):-
  number_length(M, 10.0, L).

%! number_length(+Number:number, +Radix:integer, -Length:integer) is det.
% Returns the length of the given number 'before the dot'.
% The number is in decimal notation.
%
% @arg An integer representing a decimal number.
% @arg Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @arg Length An integer representing the number of digits in
%      the given number.

number_length(N1, Radix, L1):-
  N2 is N1 / Radix,
  N2 >= 1.0, !,
  number_length(N2, Radix, L2),
  L1 is L2 + 1.
number_length(_N, _Radix, 1):- !.

%! odd(?Number:number) is semidet.
% Succeeds if the integer is odd.

odd(N):-
  mod(N, 2, 1).

%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given number of distinct objects.
%
% @see permutations/3

permutations(NumbersOfObjects, NumberOfPermutations):-
  is_list(NumbersOfObjects), !,
  sum_list(NumbersOfObjects, NumberOfObjects),
  permutations(NumbersOfObjects, NumberOfObjects, NumberOfPermutations).
permutations(NumberOfObjects, NumberOfPermutations):-
  permutations([NumberOfObjects], NumberOfPermutations).

%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given numbers of distinct objects and that have (exactly)
% the given length.
%
% *Definition*: The number of permutations of _|m|_ groups of unique objects
%               (i.e., types) and with _|n_i|_ group members or occurences
%               (i.e., tokens), for $0 \leq i \leq m$ and that are (exactly)
%               of length _|r|_ is $\frac{n!}{\mult_{i = 1}^m(n_i!)(n - r)!}$.
%
% @arg NumbersOfObject A list of numbers, each indicating the number of
%        objects in a certain group.
% @arg PermutationLength The (exact) number of objects that occur
%        in a permutation.
% @arg NumberOfPermutations The number of permutations that can be created.

permutations(NumbersOfObjects, PermutationLength, NumberOfPermutations):-
  is_list(NumbersOfObjects), !,

  % The objects.
  sum_list(NumbersOfObjects, NumberOfObjects),
  factorial(NumberOfObjects, F1),

  % The length compensation.
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F3),

  % The groups.
  maplist(factorial, NumbersOfObjects, F2s),
  multiply_list([F3 | F2s], F23),

  NumberOfPermutations is F1 / F23.
permutations(NumberOfObjects, PermutationLength, NumberOfPermutations):-
  factorial(NumberOfObjects, F1),
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F2),
  NumberOfPermutations is F1 / F2.

%! pred(?Integer:integer, ?Predecessor:integer)
% A integer and its direct predecessor integer.
%
% This is used by meta-predicates that require uniform instantiation patterns.
%
% @arg Integer An integer.
% @arg Predecessor An integer.
% @see This extends the builin succ/2.

pred(Integer, Predecessor):-
  succ(Predecessor, Integer).

%! rbetween(?Min:integer, +Max:integer, ?Value:integer) is semidet.
% If `Min` and `Max` are given, `Value` is instantiated with `Max` and
% with predecessor integers upon backtracking, until `Value` is `Min`.
%
% If only `Max` is given there is no lowest value for `Value`.

rbetween(Min, Max, Value):-
  nonvar(Max), (nonvar(Min) -> Min =< Max ; true),
  rbetween(Min, Max, Max, Value).

rbetween(Min, Value, _Max, Value):-
  nonvar(Min), Min == Value, !.
rbetween(_Min, Value, _Max, Value).
rbetween(Min, Between, Max, Value):-
  NewBetween is Between - 1,
  rbetween(Min, NewBetween, Max, Value).

smaller_than_or_equal_to(_, inf):- !.
smaller_than_or_equal_to(X, Y):-
  X =< Y.

%! square(+X:float, -Square:float) is det.
% Returns the square of the given number.

square(X, Square):-
  Square is X ** 2.
