:- module(
  dcg_cardinal,
  [
    between_digit//4, % +Low:between(0,9)
                      % +High:between(0,9)
                      % -Code:code
                      % -Number:between(0,9)
    binary_digit//2, % ?Code:code
                     % ?DecimalDigit:between(0,1)
    binary_number//1, % ?DecimalNumber:integer
    decimal_digit//2, % ?Code:code
                      % ?DecimalDigit:between(0,9)
    decimal_number//1, % ?DecimalNumber:integer
    exponent//0,
    exponent_sign//0,
    exponent_sign//1, % ?Code:code
    hexadecimal_digit//2, % ?Code:code
                          % ?DecimalDigit:between(0,15)
    hexadecimal_number//1, % -DecinalNumber:integer
    int_codes//1, % ?Codes:list(code)
    octal_digit//2, % ?Code:code
                    % ?DecimalDigit:between(0,7)
    octal_number//1, % -DecinalNumber:integer
    sign//1, % ?Sign:code
    sign//2, % -Tree:compound
             % ?Sign:oneof([-1,1])
    signed_number//1, % ?SignedNumber:float
    unsigned_number//1, % ?UnsignedNumber:float
    unsigned_number//3 % ?Number:float
                       % ?IntegerComponent:integer
                       % ?FractionalComponent:integer
  ]
).
:- reexport(
  library(dcg/basics),
  [
    digit//1,
    digits//1,
    float//1,
    integer//1,
    number//1,
    xdigit//1,
    xdigits//1,
    xinteger//1
  ]
).

/** <module> DCG_CARDINAL

DCGs for cardinal numbers.

@author Wouter Beek
@version 2013/06-2013/09
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_meta)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(math(math_ext)).

:- meta_predicate(digits_to_decimal_number(//,+,?,?,?)).
:- meta_predicate(digits_to_decimal_number(//,+,+,?,?,?)).



between_digit(Low1, High1, Code, DecimalDigit) -->
  {Low2 is Low1 + 48},
  {High2 is High1 + 48},
  between(Low2, High2, Code),
  {DecimalDigit is Code - 48}.

%! binary_digit(?Code:code, ?DecimalDigit:between(0,1))//

binary_digit(C, 0) --> zero(C).
binary_digit(C, 1) --> one(C).

%! binary_number(-DecimalNumber:integer)//

binary_number(N) -->
  digits_to_decimal_number(binary_digit, 2, N).


%! decimal_digit(?Code:code, ?DecimalDigit:between(0,9))//

decimal_digit(C, N) -->
  {var(C), var(N)},
  decimal_digit_nondet(C, N).
decimal_digit(C, N) -->
  decimal_digit_nondet(C, N), !.

decimal_digit_nondet(C, N) --> octal_digit(C, N).
decimal_digit_nondet(C, 8) --> eight(C).
decimal_digit_nondet(C, 9) --> nine(C).


%! decimal_number(-DecimalNumber:integer)//

decimal_number(N) -->
  digits_to_decimal_number(decimal_digit, 10, N).

%! digits_to_decimal_number(
%!   :DCGBody,
%!   +Radix:integer,
%!   ?DecimalNumber:integer
%! )//
% Processes digits of arbitrary radix and returns the decimal equivalent.
%
% @arg DCGBody processes a single digit if the given radix.
% @arg Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @arg An integer representing the processed number, converted to
%      the decimal number system.

digits_to_decimal_number(_Digit, Radix, M, H, T):-
  number(M), !,
  atomic_list_concat(['~', Radix, r], Format),
  format(codes(H, T), Format, [M]).
digits_to_decimal_number(Digit, Radix, M) -->
  % We start with processing the first digit.
  dcg_call(Digit, N),
  % We keep track of the decimal equivalent if the digits that we have
  % seen so far, in order to do the radix multiplication with.
  digits_to_decimal_number(Digit, Radix, N, M).

% Look for the next number...
digits_to_decimal_number(Digit, Radix, M1, M) -->
  % Process the next digit.
  dcg_call(Digit, N),
  % Perform radix multiplication.
  {M2 is M1 * Radix + N},
  digits_to_decimal_number(Digit, Radix, M2, M).
% End of code segment, the decimal number we have built so far is the result.
digits_to_decimal_number(_Digit, _Radix, M, M) --> [].

exponent -->
  exponent_sign,
  dcg_multi(decimal_digit, 1-_).

exponent_sign --> e.

exponent_sign(C) --> e(C).

%! hexadecimal_digit(?Code:code, ?DecimalNumber:between(0,15))//

hexadecimal_digit(C, N) --> decimal_digit(C, N).
hexadecimal_digit(C, 10) --> a(C).
hexadecimal_digit(C, 11) --> b(C).
hexadecimal_digit(C, 12) --> c(C).
hexadecimal_digit(C, 13) --> d(C).
hexadecimal_digit(C, 14) --> e(C).
hexadecimal_digit(C, 15) --> f(C).

%! hexadecimal_number(-DecimalDigit:integer)//

hexadecimal_number(N) -->
  digits_to_decimal_number(hexadecimal_digit, 16, N).

%! int_codes(?Codes:list(code))//
% A positive number of digits, possibly followed by a sign.

int_codes([C,D0|D]) -->
  sign(C), !,
  digit(D0),
  digits(D).
int_codes([D0|D]) -->
  digit(D0),
  digits(D).

%! octal_digit(?Code:code, ?DecimalDigit:between(0,7))//

octal_digit(C, N) --> binary_digit(C, N).
octal_digit(C, 2) --> two(C).
octal_digit(C, 3) --> three(C).
octal_digit(C, 4) --> four(C).
octal_digit(C, 5) --> five(C).
octal_digit(C, 6) --> six(C).
octal_digit(C, 7) --> seven(C).

%! octal_number(-DecimalDigit:integer)//

octal_number(N) -->
  digits_to_decimal_number(octal_digit, 8, N).

sign(-1) --> "-". %'
sign(1) --> "+". %'

sign(sign(-1), -1) --> "-".
sign(sign(1), 1) --> "+".

signed_number(N, H, T):-
  number(N), !,
  format(codes(H, T), '~w', [N]).
signed_number(N) -->
  unsigned_number(N).
signed_number(N) -->
  sign(Sg),
  unsigned_number(N1),
  {N is Sg * N1}.

unsigned_number(N, H, T):-
  number(N), !,
  format(codes(H, T), '~w', [N]).
unsigned_number(N) -->
  decimal_number(N).
unsigned_number(N) -->
  unsigned_number(N, _Integer, _Fraction).

%! unsigned_number(
%!   ?Number:float,
%!   ?IntegerComponent:integer,
%!   ?FractionalComponent:integer
%! )//

unsigned_number(N, N_I, N_F) -->
  ({N_I = 0} ; decimal_number(N_I)),
  dot,
  ({N_F = 0} ; decimal_number(N_F)),
  {number_parts(N, N_I, N_F)}.

