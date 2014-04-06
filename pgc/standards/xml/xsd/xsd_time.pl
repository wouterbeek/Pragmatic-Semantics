:- module(
  xsd_time,
  [
    xsd_time_canonical_map//1, % +Time:compound
    xsd_time_lexical_map//1 % -Time:compound
  ]
).

/** <module> XSD time datatype

Time represents instants of time that recur at the same point in each calendar
day, or that occur in some arbitrary calendar day.

### Value Space

Time uses the date/timeSevenPropertyModel, with year, month, and day required
to be absent. timezoneOffset remains optional.

#### Timezone offset

A calendar (or "local time") day with a larger positive time zone offset
begins earlier than the same calendar day with a smaller (or negative)
time zone offset. Since the time zone offsets allowed spread over 28 hours,
it is possible for the period denoted by a given calendar day with one
time zone offset to be completely disjoint from the period denoted by
the same calendar day with a different offset — the earlier day ends
before the later one starts.

The moments in time represented by a single calendar day are spread over
a 52-hour interval, from the beginning of the day in the =|+14:00|= time zone
offset to the end of that day in the =|−14:00|= time zone offset.

#### Order

Time values (points in time in an "arbitrary" day) are ordered taking into
account their timezoneOffset.

The relative order of two time values, one of which has a timezoneOffset
of absent is determined by imputing time zone offsets of both =|+14:00|= and
=|−14:00|= to the value without an offset. Many such combinations will be
incomparable because the two imputed time zone offsets yield different orders.
However, for a given non-timezoned value, there will always be timezoned
values at one or both ends of the 52-hour interval that are comparable
(because the interval of incomparability is only 28 hours wide).

#### XSD 1.0 compatibility

Some pairs of time literals which in the 1.0 version of this specification
denoted the same value now (in this version) denote distinct values instead,
because values now include time zone offset information. Some such pairs,
such as =|05:00:00-03:00|= and =|10:00:00+02:00|=, now denote equal though
distinct values (because they identify the same points on the time line);
others, such as =|23:00:00-03:00|= and =|02:00:00Z|=, now denote unequal
values (=|23:00:00−03:00 > 02:00:00Z|= because =|23:00:00−03:00|= on any
given day is equal to =|02:00:00Z|= on the next day).

### Lexical Mappings

The lexical representations for time are "projections" of those of
dateTime.

The lexical mapping maps =|00:00:00|= and =|24:00:00|= to the same value,
namely midnight (hour, minute, and second are zero).

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_remote_module(xsd(xsd_dateTime_generic)).
:- use_remote_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_time_canonical_map(+Time:compound)// is det.
% Maps a time value to a xsd_time_lexical_map//.
%
% The compound term has the following form:
% ~~~{.pl}
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~
% The values for year, month, and day are neglected for XSD time.
%
% @arg Time A compound term that represents a time value.

xsd_time_canonical_map(dateTime(_Y,_M,_D,H,M,S,TZ)) -->
  hourCanonicalFragmentMap(H),
  `:`,
  minuteCanonicalFragmentMap(M),
  `:`,
  secondCanonicalFragmentMap(S),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_time_lexical_map(-DateTime:compound)// is det.
%
% ### Grammar definitions
%
% ~~~{.ebnf}
% xsd_time_lexical_map ::=
%     ((hourFrag ':' minuteFrag ':' secondFrag) | endOfDayFrag)
%     timezoneFrag?
% ~~~
%
% ~~~{.re}
% (([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?|(24:00:00(\.0+)?))
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~
%
% The EBNF and RE do not enforce additional constraints on xsd_time_lexical_map//,
% that are actually part of its definition.

xsd_time_lexical_map(DT) -->
  (
    hourFrag(H),
    `:`,
    minuteFrag(M),
    `:`,
    secondFrag(S)
  ;
    endOfDayFrag(H, M, S)
  ),
  (timezoneFrag(TZ) ; ``),
  {newDateTime(_Y, _M, _D, H, M, S, TZ, DT)}.

