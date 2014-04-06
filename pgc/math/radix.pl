:- module(
  radix,
  [
    between_hex/3, % +LowHex:atom
                   % +HighHex:atom
                   % ?Number:integer
    decimal_to_digits/2, % +DecimalNumber:integer
                         % -DecimalDigits:list(between(0,9))
    decimal_to_digits/3, % +DecimalNumber:integer
                         % +Radix:oneof([2,8,10])
                         % -DecimalDigits:list(between(0,9))
    digits_to_decimal/2, % +DecimalDigits:list(between(0,9))
                         % -DecimalNumber:integer
    digits_to_decimal/3, % +DecimalDigits:list(between(0,15))
                         % +Radix:integer
                         % -DecimalNumber:integer
    number_to_decimal/3 % +RadixNumber:atomic
                        % +Radix:between(2,16)
                        % -DecimalNumber:integer
  ]
).

/** <module> RADIX

Predicate for transforming numbers with a different radix.

@author Wouter Beek
@tbd Study the radix topic further and reimplement these predicates
     in a more generic way.
@version 2013/07-2013/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).



between_hex(LowHex, HighHex, Number):-
  number_to_decimal(LowHex, 16, Low),
  number_to_decimal(HighHex, 16, High),
  between(Low, High, Number).

decimal_to_digits(DecimalNumber, DecimalDigits):-
  atom_chars(DecimalNumber, Chars),
  maplist(atom_number, Chars, DecimalDigits).

decimal_to_digits(D, R, Sol):-
  decimal_to_digits(D, R, [], Sol).

decimal_to_digits(D, R, A, A):-
  D < R, !.
decimal_to_digits(D, R, A, Sol):-
  H is D mod R,
  NewD is D // R,
  decimal_to_digits(NewD, R, [H|A], Sol).

digits_to_decimal(DecimalDigits, DecimalNumber):-
  digits_to_decimal(DecimalDigits, 10, DecimalNumber).

%! digits_to_decimal(
%!   +DecimalDigits:list(integer),
%!   +Radix:integer,
%!   -DecimalNumber:integer
%! ) is det.
% Process the decimal digits from left to right, using the radix to multiply
% the result at each step; returning the decimal number.
%
% @arg DecimalDigits A list of decimal digits.
%      Conversion from -- for instance -- hexadecimal digits has
%      already occured before this predicate is invoked.
% @arg Radix An integer indicating the radix of the decimal digits.
% @arg DecimalNumber An integer that is the given decimal digits
%      under the given radix.

digits_to_decimal(Ds, Radix, D):-
  aggregate_all(
    sum(ToDPart),
    (
      nth0(Position, Ds, FromD),
      ToDPart is FromD * Radix ** Position
    ),
    D
  ).

%! number_to_decimal(
%!   +RadixNumber:atomic,
%!   +Radix:between(2,16),
%!   -DecimalNumber:integer
%! ) is det.

number_to_decimal(RadixNumber, Radix, DecimalNumber):-
  atom_chars(RadixNumber, RadixDigits),
  maplist(number_to_decimal, RadixDigits, DecimalDigits),
  digits_to_decimal(DecimalDigits, Radix, DecimalNumber).

number_to_decimal(RadixDigit, DecimalNumber):-
  char_type(RadixDigit, xdigit(DecimalNumber)).

