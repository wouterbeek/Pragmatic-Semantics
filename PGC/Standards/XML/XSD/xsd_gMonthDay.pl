:- module(
  xsd_gMonthDay,
  [
    xsd_gMonthDay_canonical_map//1, % +GregorianMonthDay:compound
    xsd_gMonthDay_lexical_map//1 % -GregorianMonthDay:compound
  ]
).

/** <module> XSD Gregorian month-day datatype

=*gMonthDay*= represents whole calendar days that recur at the same point
in each calendar year, or that occur in some arbitrary calendar year.
(Obviously, days beyond 28 cannot occur in all Februaries;
29 is nonetheless permitted.)

This datatype can be used, for example, to record birthdays;
an instance of the datatype could be used to say that someone's birthday
occurs on the 14th of September every year.

Because day/month combinations in one calendar only rarely correspond to
day/month combinations in other calendars, values of this type do not,
in general, have any straightforward or intuitive representation in terms of
most other calendars. This type should therefore be used with caution in
contexts where conversion to other calendars is desired.

### Value Space

gMonthDay uses the date/timeSevenPropertyModel, with year, hour, minute, and
second required to be absent. timezoneOffset remains optional.

#### Constraint: Day-of-month Values

The day value must be no more than 30 if month is one of 4, 6, 9, or 11,
and no more than 29 if month is 2.

#### XSD 1.0 compatibility

In version 1.0 of this specification, gMonthDay values did not retain
a time zone offset explicitly, but for time zone offsets not too far from
UTC their time zone offset could be recovered based on their value's first
moment on the timeline. The date/timeSevenPropertyModel retains all time zone
offsets.

An example that shows the difference from version 1.0:
A day is a calendar (or "local time") day offset from UTC by the appropriate
interval; this is now true for all day values, including those with time zone
offsets outside the range +12:00 through -11:59 inclusive:
=|12-12+13:00 < --12-12+11:00|= (just as =|12-12+12:00|= has always been less
than =|12-12+11:00|=, but in version 1.0 =|12-12+13:00 > --12-12+11:00|=,
since =|12-12+13:00|='s "recoverable time zone offset" was =|−11:00|=).

### Lexical Mapping

The lexical representations for gMonthDay are "projections" of those of
dateTime.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_gMonthDay_canonical_map(+GregorianMonthDay:compound)//
% Maps a gMonthDay value to a xsd_gMonthDay_lexical_map//.
%
% @arg GregorianMonthDay A complete gMonthDay value.

xsd_gMonthDay_canonical_map(dateTime(_Y,M,D,_H,_MM,_S,TZ)) -->
  `--`,
  monthCanonicalFragmentMap(M),
  `-`,
  dayCanonicalFragmentMap(D),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_gMonthDay_lexical_map(-GregorianMonthDay:compound)//
% Maps a xsd_gMonthDay_lexical_map// to a gMonthDay value.
%
% ~~~{.ebnf}
% gMonthDayRep ::= '--' monthFrag '-' dayFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% --(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_gMonthDay_lexical_map(GMD) -->
  `--`,
  monthFrag(M),
  `-`,
  dayFrag(D),
  {dayInMonth(M, D)},
  (`` ; timezoneFrag(TZ)), !,
  {newDateTime(_Y, M, D, _H, _MM, _S, TZ, GMD)}.

