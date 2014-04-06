:- module(
  xsd_dateTime_generic,
  [
    dayCanonicalFragmentMap//1, % +Day:between(1,31)
    dayFrag//1, % -Day:between(1,31)
    endOfDayFrag//3, % -Hour:oneof([24])
                     % -Minute:oneof([0])
                     % -Second:oneof([0])
    hourCanonicalFragmentMap//1, % +Hour:between(0,23)
    hourFrag//1, % -Hour:between(1,23)
    minuteCanonicalFragmentMap//1, % +Minute:between(0,59)
    minuteFrag//1, % -Minute:between(1-59)
    monthCanonicalFragmentMap//1, % +Month:between(1,12)
    monthFrag//1, % -Month:between(1,12)
    secondCanonicalFragmentMap//1, % +Second:between(0.0,60.0)
    secondFrag//1, % -Second:float
    timezoneCanonicalFragmentMap//1, % +Timezone:between(-840,840)
    timezoneFrag//1, % -Minutes:between(-840,840)
    yearCanonicalFragmentMap//1, % +Year:integer
    yearFrag//1 % -Year:integer
  ]
).

/** <module> XSD dateTime generics

Generic DCG rules that are used in defining the grammar for XSD datatypes
conforming to the XSD 1.1 specification.

@author Wouter Beek
@version 2013/08-2013/11, 2014/03-2014/04
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(xsd(xsd_number_generic)).



%! dayCanonicalFragmentMap(+Day:between(1,31))//
% Maps an integer, presumably the day property of a
% date/timeSevenPropertyModel value, onto a dayFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Day An integer between 1 and 31 inclusive (may be limited further
%        depending on associated year and month).

dayCanonicalFragmentMap(D) -->
  unsTwoDigitCanonicalFragmentMap(D).


%! dayFrag(-Day:between(1,31))//
% Processes a day value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% dayFrag ::= ('0' [1-9]) | ([12] digit) | ('3' [01])
% ~~~

dayFrag(D) -->
  zero(C1),
  nonzero_decimal_digit(C2, _),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.
dayFrag(D) -->
  (one(C1) ; two(C1)),
  decimal_digit(C2, _),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.
dayFrag(D) -->
  three(C1),
  binary_digit(C2),
  {phrase(unsignedNoDecimalPtNumeral(D), [C1,C2])}.


%! endOfDayFrag(-Hour:oneof([24]), -Minute:oneof([0]), -Second:oneof([0]))//
% Combines the hourFrag//, minuteFrag//, secondFrag,
% and their separators to represent midnight of the day,
% which is the first moment of the next day.
%
% ~~~{.ebnf}
% endOfDayFrag ::= '24:00:00' ('.' '0'+)?
% ~~~

endOfDayFrag(24, 0, 0) -->
  `24:00:00`,
  `.`,
  dcg_multi(zero).


%! fourDigitCanonicalFragmentMap(+Integer:between(-9999,9999))//
% Maps an integer between =|-10000|= and =10000= onto an always-four-digit
% numeral.
%
% @arg Integer An integer whose absolute value is less than =10000=.

fourDigitCanonicalFragmentMap(I1) -->
  (
    {I1 < 0}
  ->
    `-`
  ;
    ``
  ),
  {
    I2 is copysign(I1, 1),
    xsd_number_generic:(N1 is I2 xsd_div 100)
  },
  unsTwoDigitCanonicalFragmentMap(N1),
  {xsd_number_generic:(N2 is I2 xsd_mod 100)},
  unsTwoDigitCanonicalFragmentMap(N2).


%! hourCanonicalFragmentMap(+Hour:between(0,23))//
% Maps an integer, presumably the hour property of a
% date/timeSevenPropertyModel value, onto a hourFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Hour An integer between 0 and 23 inclusive.

hourCanonicalFragmentMap(H) -->
  unsTwoDigitCanonicalFragmentMap(H).


%! hourFrag(-Hour:between(1,23))//
% Processes an hour value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% hourFrag ::= ([01] digit) | ('2' [0-3])
% ~~~

hourFrag(H) -->
  binary_digit(C1),
  decimal_digit(C2),
  {phrase(unsignedNoDecimalPtNumeral(H), [C1,C2])}.
hourFrag(H) -->
  two(C1),
  (binary_digit(C2) ; two(C2) ; three(C2)),
  {phrase(unsignedNoDecimalPtNumeral(H), [C1,C2])}.


%! minuteCanonicalFragmentMap(+Minute:between(0,59))//
% Maps an integer, presumably the minute property of a
% date/timeSevenPropertyModel value, onto a minuteFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Minute An integer between 0 and 59 inclusive.

minuteCanonicalFragmentMap(M) -->
  unsTwoDigitCanonicalFragmentMap(M).


%! minuteFrag(-Minute:between(1-59))//
% Processes a minute value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% minuteFrag ::= [0-5] digit
% ~~~

minuteFrag(M) -->
  ( binary_digit(C1)
  ; two(C1)
  ; three(C1)
  ; four(C1)
  ; five(C1)),
  decimal_digit(C2),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.


%! monthCanonicalFragmentMap(+Month:between(1,12))//
% Maps an integer, presumably the month property of a
% date/timeSevenPropertyModel value, onto a monthFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Month An integer between 1 and 12 inclusive.

monthCanonicalFragmentMap(M) -->
  unsTwoDigitCanonicalFragmentMap(M).


%! monthFrag(-Month:between(1,12))//
% Processes a month value, i.e. a numeral consisting of exactly two
% decimal digits.
%
% ~~~{.ebnf}
% monthFrag ::= ('0' [1-9]) | ('1' [0-2])
% ~~~

monthFrag(M) -->
  zero(C1),
  nonzero_decimal_digit(C2, _),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.
monthFrag(M) -->
  one(C1),
  (binary_digit(C2) ; two(C2)),
  {phrase(unsignedNoDecimalPtNumeral(M), [C1,C2])}.


nonzero_decimal_digit(C, D) -->
  between_digit(1, 9, C, D).


%! secondCanonicalFragmentMap(+Second:between(0.0,60.0))//
% Maps a decimal number, presumably the second property of a
% date/timeSevenPropertyModel value, onto a secondFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Second A nonnegative decimal number less than 70.

secondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsTwoDigitCanonicalFragmentMap(S).
secondCanonicalFragmentMap(S1) -->
  {xsd_number_generic:(N1 is S1 xsd_div 1)},
  unsTwoDigitCanonicalFragmentMap(N1),
  `.`,
  {xsd_number_generic:(N2 is S1 xsd_mod 1)},
  fractionDigitsCanonicalFragmentMap(N2).


%! secondFrag(-Second:float)//
% Processes a second value, i.e. a numeral consisting of exactly two
% decimal digits, or two decimal digits, a decimal point, and
% one or more trailing digits.
%
% ~~~{.ebnf}
% secondFrag ::= ([0-5] digit) ('.' digit+)?
% ~~~

secondFrag(S) -->
  (binary_digit(C1) ; two(C1) ; three(C1) ; four(C1) ; five(C1)),
  decimal_digit(C2),
  (
    dot(C3),
    dcg_multi1(decimal_digit, 1-_, CT),
    {phrase(unsignedDecimalPtNumeral(S), [C1,C2,C3|CT])}
  ;
    {phrase(unsignedNoDecimalPtNumeral(S), [C1,C2])}
  ).


%! timezoneCanonicalFragmentMap(+Timezone:between(-840,840))//
% Maps an integer, presumably the timezoneOffset property of a
% date/timeSevenPropertyModel value, onto a timezoneFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Timezone An integer between =|−840|= and =840= inclusive.

timezoneCanonicalFragmentMap(0) -->
  `Z`.
timezoneCanonicalFragmentMap(TZ1) -->
  `-`,
  {
    TZ2 is copysign(TZ1, 1),
    xsd_number_generic:(N1 is TZ2 xsd_div 60)
  },
  unsTwoDigitCanonicalFragmentMap(N1),
  `:`,
  {xsd_number_generic:(N2 is TZ2 xsd_mod 60)},
  unsTwoDigitCanonicalFragmentMap(N2).


%! timezoneFrag(-Minutes:between(-840,840))//
% Processes an offset between UTC and local time.
% Time zone offsets are a count of minutes (expressed as a count of hours and
% a count of minutes) that are added or subtracted from UTC time to get
% the "local" time.
%
% =Z= is an alternative representation of the time zone offset =|00:00|=,
% which is zero minutes from UTC.
%
% ~~~{.ebnf}
% timezoneFrag ::= 'Z'
%                | ('+' | '-')
%                  (('0' digit | '1' [0-3]) ':' minuteFrag | '14:00')
% ~~~

timezoneFrag(0) -->
  `Z`.
timezoneFrag(840) -->
  `14:00`.
timezoneFrag(TZ) -->
  sign(Sign),
  (
    zero(C1),
    decimal_digit(C2)
  ;
    one(C1),
    (
      binary_digit(C2)
    ;
      three(C2)
    )
  ),
  % @compat Here we deviate from the XSD 1.1 standard,
  %         which uses unsignedDecimalPtNumeral//1 instead.
  %         But note that the definition of timezoneFrag//1
  %         excludes the appearance of a decimal separator.
  {phrase(unsignedNoDecimalPtNumeral(N1), [C1,C2])},
  `:`,
  minuteFrag(N2),
  {TZ is copysign(N1 * 60 + N2, Sign)}.


%! unsTwoDigitCanonicalFragmentMap(+Integer:between(0,99))//
% Maps a nonnegative integer less than =100= onto an unsigned
% always-two-digit numeral.
%
% @arg Integer A nonnegative integer less than =100=.

unsTwoDigitCanonicalFragmentMap(I) -->
  {xsd_number_generic:(D1 is I xsd_div 10)},
  decimal_digit(_, D1),
  {xsd_number_generic:(D2 is I xsd_mod 10)},
  decimal_digit(_, D2).


%! yearCanonicalFragmentMap(+Year:integer)//
% Maps an integer, presumably the year property of a
% date/timeSevenPropertyModel value, onto a yearFrag//,
% part of a date/timeSevenPropertyModel's lexical representation.
%
% @arg Year An integer.

yearCanonicalFragmentMap(Y) -->
  {abs(Y) > 9999}, !,
  noDecimalPtCanonicalMap(Y).
yearCanonicalFragmentMap(Y) -->
  fourDigitCanonicalFragmentMap(Y).


%! yearFrag(-Year:integer)//
% Processes a year valie, i.e. a numeral consisting of at least four,
% decimal digits optionally preceded by a minus sign;
% leading '0' digits are prohibited except to bring the digit count
% up to four.
%
% ~~~{.ebnf}
% yearFrag ::= '-'? (([1-9] digit digit digit+)) | ('0' digit digit digit))
% ~~~
%
% @arg Year An integer.

yearFrag(Y) -->
  (minus_sign(S), {Cs = [S,Code|Codes]} ; {Cs = [Code|Codes]}),
  (
    nonzero_decimal_digit(Code, _),
    dcg_multi1(decimal_digit, 3-_, Codes)
  ;
    zero(Code),
    dcg_multi1(decimal_digit, 3, Codes)
  ),
  {
    phrase(noDecimalPtNumeral(S, I), Cs),
    Y is copysign(I, S)
  }.

