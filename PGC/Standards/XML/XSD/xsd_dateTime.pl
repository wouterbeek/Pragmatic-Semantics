:- module(
  xsd_dateTime,
  [
    xsd_dateTime_canonical_map//1, % +DateTime:compound
    xsd_dateTime_lexical_map//1, % -DateTime:compound
    xsd_dateTime_compare/3 % -Order:oneof([incomparable,<,=,>])
                           % +DataTime1:compound
                           % +DataTime2:compound
  ]
).

/** <module> XSD dateTime datatype

*=dateTime=* represents instants of time, optionally marked with
a particular time zone offset.

### Value space

dateTime uses the date/timeSevenPropertyModel, with no properties except
timezoneOffset permitted to be absent.
The timezoneOffset property remains optional.

#### Constraint: Day-of-month Values

The day value must be no more than 30 if month is one of 4, 6, 9, or 11;
no more than 28 if month is 2 and year is not divisible by 4,
or is divisible by 100 but not by 400;
and no more than 29 if month is 2 and year is divisible by 400,
or by 4 but not by 100.

#### Order

dateTime values are ordered by their timeOnTimeline/2 value.

Since the order of a dateTime value having a timezoneOffset relative to
another value whose timezoneOffset is absent is determined by imputing
time zone offsets of both =|+14:00|= and =|−14:00|= to the value
with no time zone offset, many such combinations will be incomparable
because the two imputed time zone offsets yield different orders.

Although dateTime and other types related to dates and times have only
a partial order, it is possible for datatypes derived from dateTime
to have total orders, if they are restricted (e.g. using the pattern facet)
to the subset of values with, or the subset of values without,
time zone offsets. Similar restrictions on other date- and time-related
types will similarly produce totally ordered subtypes.

Note, however, that such restrictions do not affect the value shown,
for a given Simple Type Definition, in the =ordered= facet.

#### XSD 1.0 compatibility

Order and equality are essentially the same for dateTime in this version
of this specification as they were in version 1.0.
However, since values now distinguish time zone offsets,
equal values with different timezoneOffsets are not identical,
and values with extreme timezoneOffsets may no longer be equal
to any value with a smaller timezoneOffset.

##### Identity & equality

Values representing the same instant but having different time zone offsets
are equal but not identical.

### Lexical space

#### Redundant values

=Z=, =|+00:00|=, and =|-00:00|= are redundant.

Trailing fractional zero digits for secondFrag// are redundant.

Otherwise these mappings are one-to-one.

#### =endOfDayFrag=

There is no lexical mapping for endOfDayFrag//.
It is handled specially by the relevant lexical mappings
(e.g., =xsd_dateTime_lexical_map=).

### Concepts

*|Universal Coordinated Time (UTC)|* is an adaptation of TAI which closely
approximates UT1 by adding leap-seconds to selected UTC days.

### 7-property model for date and time

Two isomorphic ways to model moments in time:
  * Year, month, day, hour, minute and second.
  * Time (measured generally in seconds or days) from some starting moment.

The time zone offset is defined as the number of minutes of offset from UTC.
Values for the six primary properties are always stored in their local values
(the values shown in the lexical representations), rather than converted to
UTC.

7-property model:
  * =year=
  An integer.
  * =month=
  An integer between 1 and 12 inclusive.
  * =day=
  An integer between 1 and 31 inclusive, possibly restricted further
  depending on month and year.
  * =hour=
  An integer between 0 and 23 inclusive.
  * =minute=
  An integer between 0 and 59 inclusive.
  * =second=
  A decimal number greater than or equal to 0 and less than 60.
  * =timezoneOffset=
  An optional integer between −840 and 840 inclusive (i.e., 14 hours).

Values less than =1582= in the =year= property represent years in the
"proleptic Gregorian calendar".
A value of zero in the =year= property represents the year 1 BCE;
a value of =−1= represents the year 2 BCE, =−2= is 3 BCE, etc.

While calculating, property values from the dateTime
~~~{.xsd}
1972-12-31T00:00:00
~~~
are used to fill in for those that are absent, except that if day
is absent but month is not, the largest permitted day for that month is used.

### XSD 1.0 compatibility

In XSD 1.0 the year property was not permitted to have the value zero.
The year before the year 1 in the proleptic Gregorian calendar,
traditionally referred to as 1 BC or as 1 BCE, was represented by a year
value of −1, 2 BCE by −2, and so forth. Of course, many, perhaps most,
references to 1 BCE (or 1 BC) actually refer not to a year in the proleptic
Gregorian calendar but to a year in the Julian or "old style" calendar;
the two correspond approximately but not exactly to each other.

In this version of this specification, two changes are made in order to
agree with existing usage. First, year is permitted to have the value zero.
Second, the interpretation of year values is changed accordingly:
a year value of zero represents 1 BCE, −1 represents 2 BCE, etc.
This representation simplifies interval arithmetic and leap-year
calculation for dates before the common era (which may be why astronomers
and others interested in such calculations with the proleptic Gregorian
calendar have adopted it), and is consistent with the current edition of
ISO 8601.

Note that 1 BCE, 5 BCE, and so on (years 0000, −0004, etc. in the lexical
representation defined here) are leap years in the proleptic Gregorian
calendar used for the date/time datatypes defined here.
XSD 1.0 was unclear about the treatment of leap years before the common era.
If existing schemas or data specify dates of 29 February for any years
before the common era, then some values giving a date of 29 February which
were valid under a plausible interpretation of XSD 1.0 will be invalid
under this specification, and some which were invalid will be valid.
With that possible exception, schemas and data valid under the old
interpretation remain valid under the new.

### Leap seconds

A *|leap-second|* is an additional second added to the last day of December,
June, October, or March, when such an adjustment is deemed necessary by the
International Earth Rotation and Reference Systems Service (IERS) in order to
keep UTC within 0.9 seconds of observed astronomical time.
When leap seconds are introduced, the last minute in the day has more than
sixty seconds. In theory leap seconds can also be removed from a day,
but this has not yet occurred.
Leap seconds are not supported by the types defined here.

Because the simple types defined here do not support leap seconds,
they cannot be used to represent the final second, in UTC,

### Order

Values from any one date/time datatype using the seven-component model
(all except =duration=) are ordered the same as their timeOnTimeline/2 values,
except that if one value's timezoneOffset is absent and the other's is not,
and using maximum and minimum timezoneOffset values for the one whose
timezoneOffset is actually absent changes the resulting (strict) inequality,
the original two values are incomparable.

--

@author Wouter Beek
@see ISO 8601
@see ITU-R TF.460-6
@see USNO Historical List includes the times when the difference between
     TAI and UTC has changed tai_utc.txt.
@tbd Document what TAI and UT1 are.
@version 2013/08-2013/11, 2014/03-2014/04
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_dateTime_canonical_map(+DateTime:compound)// is det.
% Maps a dateTime value to a xsd_dateTime_lexical_map//.
%
% @arg DateTime A compound term of the following form:
% ~~~
% dateTime(Year,Month,Day,Hour,Minute,Second,TimeZone)
% ~~~

xsd_dateTime_canonical_map(dateTime(Y,M,D,H,MM,S,TZ)) -->
  yearCanonicalFragmentMap(Y),
  `-`,
  monthCanonicalFragmentMap(M),
  `-`,
  dayCanonicalFragmentMap(D),
  `T`,
  hourCanonicalFragmentMap(H),
  `:`,
  minuteCanonicalFragmentMap(MM),
  `:`,
  secondCanonicalFragmentMap(S),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_dateTime_lexical_map(-DateTime:compound)// is det.
% Subsequent =|-|=, =T=, and =|:|=, separate the various numerals.
%
% ### Grammar definitions
%
% ~~~{.ebnf}
% xsd_dateTime_lexical_map ::=
%     yearFrag '-' monthFrag '-' dayFrag 'T'
%     ((hourFrag ':' minuteFrag ':' secondFrag) | endOfDayFrag)
%     timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})
% -(0[1-9]|1[0-2])
% -(0[1-9]|[12][0-9]|3[01])
% T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?|(24:00:00(\.0+)?))
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~
%
% Note that the day-of-month representation constraint is not yet encoded in
% these definitions.
%
% ### Example
%
% Noon on 10 October 2002, Central Daylight Savings Time as well as
% Eastern Standard Time in the U.S.:
% ~~~
% 2002-10-10T12:00:00−05:00
% 2002-10-10T17:00:00Z
% ~~~
%
% ### XSD 1.0 compatibility
%
% XSD 1.1 distinguished between the terms 'timezone' and 'timezone offset'.
% XSD 1.0 did not make this distinction, but used the term 'timezone' for
% the time zone offset.
%
% @arg DateTime A compound term.
% @see W3C Working with Time Zones (WG Note)
%      http://www.w3.org/TR/2011/NOTE-timezone-20110705/

xsd_dateTime_lexical_map(DT) -->
  yearFrag(Y), hyphen, monthFrag(M), hyphen, dayFrag(D),
  `T`,
  (
    hourFrag(H),
    `:`,
    minuteFrag(MM),
    `:`,
    secondFrag(S)
  ;
    endOfDayFrag(H, MM, D)
  ),
  (timezoneFrag(TZ) ; ``),
  {newDateTime(Y, M, D, H, MM, S, TZ, DT)}.



% RELATIONS %

%! xsd_dateTime_compare(
%!   -Order:oneof([<,=,>]),
%!   +DateTime1:compound,
%!   +DateTime2:compound
%! ) is semidet.
% Fails only if the given values are incomparable.
%
% The ordering relation on the dateTime value space.
%
% DateTime values are ordered according to their timeOnTimeline/2 values,
% except that if one value's =timezoneOffset= is absent
% and the other's is not,
% and using maximum and minimum =timezoneOffset= values
% for the one whose =timezoneOffset= is actually absent (i.e. -840 and 840)
% changes the resulting (strict) inequality,
% the original two values are incomparable.

xsd_dateTime_compare(
  Order,
  dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
  dateTime(Y2,M2,D2,H2,MM2,S2,TZ2)
):-
  var(TZ1),
  nonvar(TZ2), !,
  xsd_dateTime_compare(
    InverseOrder,
    dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
    dateTime(Y2,M2,D2,H2,MM2,S2,TZ2)
  ),
  invert_order(InverseOrder, Order).
xsd_dateTime_compare(
  Order,
  dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
  dateTime(Y2,M2,D2,H2,MM2,S2,TZ2)
):-
  nonvar(TZ1),
  var(TZ2), !,

  % Calculate the order if we assume the minimum timezone.
  minimum_timezone(MinTZ),
  xsd_dateTime_compare(
    Order,
    dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
    dateTime(Y2,M2,D2,H2,MM2,S2,MinTZ)
  ),

  % Calculate the order if we assume the maximum timezone.
  maximum_timezone(MaxTZ),
  xsd_dateTime_compare(
    Order,
    dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
    dateTime(Y2,M2,D2,H2,MM2,S2,MaxTZ)
  ).
xsd_dateTime_compare(
  Order,
  dateTime(Y1,M1,D1,H1,MM1,S1,TZ1),
  dateTime(Y2,M2,D2,H2,MM2,S2,TZ2)
):-
  (
    nonvar(TZ1),
    nonvar(TZ2)
  ;
    var(TZ1),
    var(TZ2)
  ), !,
  timeOnTimeline(dateTime(Y1,M1,D1,H1,MM1,S1,TZ1), ToTL1),
  timeOnTimeline(dateTime(Y2,M2,D2,H2,MM2,S2,TZ2), ToTL2),
  compare(Order, ToTL1, ToTL2).

minimum_timezone(-840).
maximum_timezone(840).

invert_order(=, =).
invert_order(<, >).
invert_order(>, <).

