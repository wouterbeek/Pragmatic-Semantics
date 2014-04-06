:- module(
  iso8601_generic,
  [
    iso8601_float//4, % -Tree:compound
                      % +Name:atom
                      % +Length:integer
                      % ?Number:number
    iso8601_integer//4, % -Tree:compound
                        % +Name:atom
                        % +Length:integer
                        % ?Number:integer
    iso8601_time_designator/2, % +UTC_Time:compound
                               % -TimeDesignator:boolean
    iso8601_time_designator//1, % -Tree:compound
    iso8601_time_designator/4, % ?Hour:integter
                               % ?Minute:integer
                               % ?Second:float
                               % -TimeDesignator:boolean
    iso8601_week_designator//1 % -Tree:compound
  ]
).

/** <module> ISO8601_GENERIC

# Concepts

## Date and time representation

Expression indicating a time point, time interval or recurring time interval.

## Date and time format representation

Expression describing the format of a group of date and time representations.

## Basic format

Format of a date and time representation or date and time format
representation comprising the minimum number of time elements necessary for
the accuracy required.

The basic format should be avoided in plain text.

## Extended format

Extension of the basic format that includes additional separators.

## Complete representation

Representation that includes all the date and time components associated
with the expression; limited, if applicable, for time elements of
representations expressing a calendar year to four digits.

## Decimal representation

Expansion of a representation by addition of a decimal fraction to the lowest
order component of the expression.

## Representation with reduced accuracy

Abbreviation of a representation by omission of lower order components.

## Expanded representation

Expansion of a representation to allow identification of dates in calendar
years outside the range =|[0000]|= till =|[9999]|=.

Permitted only by mutual agreement of the partners in information interchange.

## Case

Lower case characters may be used when upper case characters are not
available.

## Underlining

The date and time format representations use characters that potentially
expand into more than one character in the date and time representation;
this is indicated by underlining.

If at the time of information interchange of the date and time format
representation the number of characters to be used in the date and time
representation is known, the variable expansion representation
(i.e. underlining) shall not be used.

In environments that do not support the representation of underlined
characters, the underline shall precede the character to be underlined.

## Characters representing date and time characters

In date and time format representations characters are used to represent
characters in the date and time representations as follows:
  * =|[Y]|= represents a digit used in the time element "year";
  * =|[M]|= represents a digit used in the time element "month";
  * =|[D]|= represents a digit used in the time element "day";
  * =|[w]|= represents a digit used in the time element "week";
  * =|[h]|= represents a digit used in the time element "hour";
  * =|[m]|= represents a digit used in the time element "minute";
  * =|[s]|= represents a digit used in the time element "second";
  * =|[n]|= represents a digit from a positive integer or zero;
  * =|[±]|= represents a plus sign =|[+]|= if in combination with the
    following element a positive value or zero needs to be represented
    (in this case, unless explicitly stated otherwise, the plus sign shall
    not be omitted), or a minus sign =|[−]|= if in combination with the
    following element a negative value needs to be represented.

In addition the following convention applies:
  * =|[_]|+ When any of the characters representing a digit is underlined,
    it represents zero or more digits in the corresponding date and time
    representation.

Other characters in the date and time format representations are copied in
the date and time representations.

## Characters used as designators

In representations the following characters are used as designators:
  * =|[P]|= is used as duration designator, preceding the component which
    represents the duration;
    (Based on the historical use of the term "period" for duration.)
  * =|[R]|= is used as recurring time interval designator;
  * =|[T]|= is used as time designator to indicate:
    * The start of the representation of local time to designate
      local time expressions as such,
    * The start of the representation of the time of day in date and time
      of day expressions,
    * The start of the representation of the number of hours, minutes or
      seconds in expressions of duration;
  * =|[W]|= is used as week designator, preceding a data element which
    represents the ordinal number of a calendar week within the calendar year;
  * =|[Z]|= is used as UTC designator.
  * =|[M]|= may (but need not) be used to indicate "month" or "minute".

Note that the meaning of =|M|= is context-dependent, based on its position in
the expression.

## Characters used as separators

In representations the following characters are used as separators:
  * =|[-]|= (hyphen): separates "year" from "month", "year" from “week”,
    "year" from "day", "month" from "day", and "week" and "day";
  * =|[:]|= (colon): separates "hour" from "minute",
    and "minute" from "second";
  * =|[/]|= (solidus): separates components in the representation of time
    intervals and recurring time intervals.

## Leading zeros

If a time element in a defined representation has a defined length,
then leading zeros shall be used as required.

# Notes by Markus Kuhn

## On the year-day format

Both day and year are useful units of structuring time, because the position
of the sun on the sky, which influences our lives, is described by them.
However the 12 months of a year are of some obscure mystic origin and have no
real purpose today except that people are used to having them (they do not
even describe the current position of the moon).

## On the leap second

The value =60= for =ss= might sometimes be needed during an inserted leap
second in an atomic time scale like Coordinated Universal Time (UTC).
A single leap second =|23:59:60|= is inserted into the UTC time scale every
few years as announced by the International Earth Rotation Service in Paris,
to keep UTC from wandering away more than =|0.9|= seconds from the less
constant astronomical time scale UT1, which is defined by the actual rotation
of the earth. In practice you are not very likely to see a clock showing
=|23:59:60|=. Most synchronized clocks resynchronize again to UTC some time
after a leap second has happened, or they temporarily slow down near the time
of a leap seconds, to avoid any disruption that an out-of-range timestamp
might otherwise cause.

## On 12h-based notations used in several English-speaking countries

The old English 12h notation has the following disadvantages:
* Longer to write down.
* Comparison is more difficult than plain string comparison.
* It is not clear how =|00:00|=, =|12:00|= and =|24:00|= are represented.
* It is easy to make the mistake that the next day starts at the overflow
  from =|12:59 a.m.|= to =|1:00 a.m.|=.

## On German national standards

The German standard DIN 5008, which specifies typographical rules for German
texts written on typewriters, was updated in 1996-05. The old German numeric
date notations =|DD.MM.YYYY|= and =|DD.MM.YY|= have been replaced by ISO 8601
conforming ones. Similarly for the old German time notations =|hh.mm|= and
=|hh.mm.ss|=.

The new notations are mentioned in the latest edition of the _Duden_.

The German alphanumeric date notation continues to be for example
"3. August 1994" or "3. Aug. 1994".

The corresponding Austrian standard has used ISO 8601 date and time notations
for some time.

## On EU standards

ISO 8601 has been adopted as European Standard EN 28601.

## On the UTC designator

The =Z= stands for the "zero meridian", which goes through Greenwich in
London, and it is also commonly used in radio communication where it is
pronounced "Zulu" (the word for Z in the international radio alphabet).

## On the difference between UTC and GMT

Universal Time (sometimes also called "Zulu Time") was called
Greenwich Mean Time (GMT) before 1972, however this term should no longer be
used. Since the introduction of an international atomic time scale, almost
all existing civil time zones are now related to UTC, which is slightly
different from the old and now unused GMT.

## Names for specific UTC corrections

There exists no international standard that specifies abbreviations for
civil time zones like CET, EST, etc. and sometimes the same abbreviation
is even used for two very different time zones. In addition, politicians
enjoy modifying the rules for civil time zones, especially for daylight
saving times, every few years, so the only really reliable way of
describing a local time zone is to specify numerically the difference of
local time to UTC.

| *|Abbreviated name|* | *|Full name|*                       | *|UTC correction|* |
| CET                  | Central European Time               | =|+0100|=          |
| EST                  | U.S./Canadian Eastern Standard Time | =|-0500|=          |

## ISO 8601 notation with C and POSIX

Function =|strftime()|= and utility =date=.

| =|%Y-%m-%d|=  | =|1999-12-31|= |
| =|%Y-%j|=     | =|1999-365|=   |
| =|%G-W%V-%u|= | =|1999-W52-5|= |
| =|%H:%M:%S|=  | =|23:59:59|=   |

--

@author Wouter Beek
@see http://www.cl.cam.ac.uk/~mgk25/iso-time.html
@tbd Appendix A, relations to earlier ISO standards, is not processed.
@version 2013/07-2013/08
*/

:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(math(math_ext)).
:- use_remote_module(math(radix)).



%! iso8601_float(
%!   -Tree:compound,
%!   +Name:atom,
%!   +Length:integer,
%!   ?Number:number
%! )//

iso8601_float(T0, Name, Length, N) -->
  {var(N)}, !,
  iso8601_integer(T1, integer, Length, N_I),
  (
    iso8601_fraction_separator(T2),
    iso8601_integer(T3, fraction, F_Length, N_F),
    % At least one digit must follow the decimal point if it appears.
    {F_Length > 0},
    {number_parts(N, N_I, N_F)}
  ;
    {N = N_I}
  ),
  {parse_tree(Name, [T1,T2,T3], T0)}.
iso8601_float(T0, Name, Length, N) -->
  {integer(N)}, !,
  iso8601_integer(T0, Name, Length, N).
iso8601_float(T0, Name, Length, N) -->
  {float(N)}, !,
  {number_parts(N, N_I, N_F)},
  iso8601_integer(T1, integer, Length, N_I),
  iso8601_fraction_separator(T2),
  iso8601_integer(T3, fraction, F_Length, N_F),
  % At least one digit must follow the decimal point if it appears.
  {F_Length > 0},
  {parse_tree(Name, [T1,T2,T3], T0)}.

% Comma is prefered.
iso8601_fraction_separator(',') --> comma.
iso8601_fraction_separator('.') --> dot.

%! iso8601_integer(
%!   -Tree:compound,
%!   +Name:atom,
%!   +Length:integer,
%!   ?Number:integer
%! )//

iso8601_integer(T0, Name, Length, I) -->
  {var(I)}, !,
  % Notice that we do not use the convert/1 option here. See below.
  dcg_multi2(decimal_digit, Length, _Cs, Is),
  % Notice that we cannot use the decimal number in the parse tree,
  % because then we would miss any padding zeros.
  {digits_to_decimal(Is, I)},
  {parse_tree(Name, Is, T0)}.
iso8601_integer(T0, Name, Length, I) -->
  {(nonvar(Length) -> Length_ = Length ; number_length(I, Length_))},
  {padded_number(I, Length_, Is)},
  dcg_multi2(decimal_digit, Length, _Cs, Is),
  {parse_tree(Name, Is, T0)}.

%! iso8601_time_designator(
%!   +UTC_Time:compound,
%!   -TimeDesignator:boolean
%! ) is det.
% Notice that we are strict about the insertion of the time designator
% =|[T]|=, in that we require it to be in place in all date-time
% representations with non-empty time.
%
% We do this because otherwise there will be abmbiguities.
% For example =198504122320Z= can be parsed as =|1985-04-12T23:20Z=|
% or as =|1985-04T12:23:20Z|=.

iso8601_time_designator(utc_time(time(H,M,S),_UTC_Correction), T):-
  iso8601_time_designator(H, M, S, T).

%! iso8601_time_designator(
%!   ?Hour:integter,
%!   ?Minute:integer,
%!   ?Second:float,
%!   -TimeDesignator:boolean
%! ) is det.

iso8601_time_designator(H, M, S, false):-
  var(H), var(M), var(S).
iso8601_time_designator(H, M, S, true):-
  (nonvar(H) ; nonvar(M) ; nonvar(S)).

iso8601_time_designator(time_designator('T')) -->
  "T".

iso8601_week_designator('W') -->
  "W".

%! padded_list(+List1:list, +Length:integer, -List2:list) is det.
% Padds the given list with zeros until it has the indicated length.

padded_list(L, N, L):-
  N =< 0, !.
padded_list(L1, N, [0|L2]):-
  NewN is N - 1,
  padded_list(L1, NewN, L2).

%! padded_number(
%!   +DecimalNumber:integer,
%!   +Length:integer,
%!   -DecimalDigits:list(between(0,9))
%! ) is det.
% Turns a decimal number into a list of decimal digits, padded with zeros.

padded_number(DecimalNumber, Length, DecimalDigits2):-
  decimal_to_digits(DecimalNumber, DecimalDigits1),
  length(DecimalDigits1, NumberOfDigits),
  NumberOfZeros is Length - NumberOfDigits,
  padded_list(DecimalDigits1, NumberOfZeros, DecimalDigits2).

