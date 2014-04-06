:- module(
  iso8601_time_interval,
  [
    iso8601_duration//2, % -Tree:compound
                         % ?ISO8601_Duration:compound
    iso8601_recurring_time_interval//6, % -Tree:compound,
                                        % ?Variant:between(1,4)
                                        % ?Format:oneof([basic,extended])
                                        % ?Recurrences:integer
                                        % ?DateTime1:compound
                                        % ?DateTime2:compound
    iso8601_time_interval//5 % -Tree:compound,
                             % ?Variant:between(1,4)
                             % ?Format:oneof([basic,extended])
                             % ?DateTime1:compound
                             % ?DateTime2:compound
  ]
).

/** <module> ISO8601_TIME_INTERVAL

# Custom datatypes

Date-time combinations:
~~~
date_time(
  ?Date:compound,
  ?UTC_Time:compound
)
~~~

Duration:
~~~
iso8601_duration(
  ?Year:between(0,9999),
  ?Month:between(1,12),
  ?Week:between(1,53),
  ?Day:between(1,366),
  ?Hour:between(0,24),
  ?Minute:between(0,59),
  ?Second:between(0.0,60.0)
)
~~~

--

# Time interval

A time interval is expressed in one of the following ways:
  1. by a start and an end;
  2. by a duration and context information;
  3. by a start and a duration;
  4. by a duration and an end.

In the second casethe time interval is not fully determined by the information
provided by the expression.
It is assumed that, where needed, additional information to completely
determine the time interval is available from the context.

Separators and designators
  * A solidus =|[/]|= is used to separate the two components.
    Variants 1, 3, 4.
    In certain application areas a double hyphen is used as a separator
    instead of a solidus.
  * =|[P]|= directly precedes the remainder of the expression of duration.
    Variants 2, 3, 4.

*Duration* can be expressed by a combination of components with
_|accurate duration|_ (hour, minute and second) and components with
_|nominal duration|_ (year, month, week and day).

A duration can also be designated by an expression containing components with
both accurate and nominal duration.

See ISO 31-1 for accurate durations using the time unit day.

Duration is used as a component in representations of time intervals and
recurring time intervals, representation of duration as such is not
facilitated.

## Format with designators

In expressions of time interval or recurring time interval, duration can be
represented by a combination of components with designators:
  * The number of years is followed by the designator =|[Y]|=.
  * The number of months is followed by the designator =|[M]|=.
  * The number of weeks is followed by the designator =|[W]|=.
  * The number of days is followed by the designator =|[D]|=.
  * The part including time components is preceded by the designator =|[T]|=.
  * The number of hours is followed by =|[H]|=
  * The number of minutes is followed by the designator =|[M]|=.
  * The number of seconds is followed by the designator =|[S]|=.

For the day component on can express a arbitrary multiple of the duration
of a calendar day, i.e. =|[n_nD]|=.

In both basic and extended format the complete representation of
the expression for duration is =|[PnnW]|= or =|[PnnYnnMnnDTnnHnnMnnS]|=.

In these representations the maximum number of digits in a component needs
to be agreed by the partners in information interchange.

For reduced accuracy or decimal representations of this representation,
the following rules apply:
  * If necessary for a particular application, the lowest order components
    may be omitted to represent duration with reduced accuracy.
  * If necessary for a particular application, the lowest order components
    may have a decimal fraction.
    The decimal fraction shall be divided from the integer part by
    the decimal sign (see ISO 31-0).
    The decimal fraction shall at least have one digit, the maximum number
    of digits in the decimal component needs to be agreed by the
    partners in information interchange.
    If the magnitude of the number is less than unity, the decimal sign
    shall be preceded by a zero (see ISO 31-0).
  * If the number of years, months, days, hours, minutes or seconds
    in any of these expressions equals zero, the number and
    the corresponding designator may be absent;
    however, at least one number and its designator shall be present.
  * The designator =|[T]|= is absent if all of the time components are absent.

## Alternative format

By mutual agreement of the partners in information interchange,
duration may be expressed in conformity with the following format for time
points: (1) calendar date, (2) ordinal date,
(3) possibly including the time designator =|[T]|=,
and (4) combinations of dates (cardinal dates and ordinal dates) and
times (restricted to local time's complete representations, representations
with reduced accuracy, and representations with decimal fraction).

The values expressed must not exceed the "carry over points" of 12 months,
30 days, 24 hours, 60 minutes and 60 seconds.
Since weeks have no defined carry over point (52 or 53), weeks should not
be used in these applications.

In these expressions a possible value of the year time element is =|[0000]|=,
of the calendar month and calendar day of the month time elements =|[00]|= and
of the calendar day of the year time element =|[000]|=.

The complete representation of the expression for duration in
the alternative format is as follows:
  * Basic format: =|PYYYYMMDDThhmmss|= or =|PYYYYDDDThhmmss|=
  * Extended format: =|PYYYY-MM-DDThh:mm:ss|= or =|PYYYY-DDDThh:mm:ss|=

## Complete representation

### Intervals indentified by start and end

When the application identifies the need for a complete representation of
a time interval, identified by its start and its end, it shall use an
expression in accordance with the format designators (see above),
combining any two complete date and time-of-day representations,
provided that the resulting expression is either consistently
in basic format or consistently in extended format.

The following examples represent a time interval beginning at 20 minutes
and 50 seconds past 23 hours on 12 April 1985 local time and
ending at 30 minutes past 10 hours on 25 June 1985 local time.

Basic format:
~~~
YYYYMMDDThhmmss/YYYYMMDDThhmmss
~~~
Example: =|19850412T232050/19850625T103000|=

Extended format:
~~~
YYYY-MM-DDThh:mm:ss/YYYY-MM-DDThh:mm:ss
~~~
Example: =|1985-04-12T23:20:50/1985-06-25T10:30:00|=

### Intervals indentified by duration and context information

#### Format with designators

When an application identifies the need for a complete representation of
a time interval through its duration and context information, with duration
in the format with designators, it shall use an expression in accordance
with the separators and designators for intervals (see above)
using a complete duration representation as defined by the format with
designators (see above).

Basic and extended format:
~~~
PnnYnnMnnDTnnHnnMnnS
PnnW
~~~

Example 1: =|P2Y10M15DT10H30M20S|=
Example 2: =|P6W|=

Example 1 represents a time interval with a duration of 2 years, 10 months,
15 days, 10 hours, 30 minutes and 20 seconds.
Example 2 represents a time interval with a duration of six weeks.

#### Alternative format

If, by agreement, a complete representation of a time interval through
its duration and context information, with duration in the alternative format,
is used, the expression shall be in accordance with the alternative
representation described above.

Basic format:
~~~
PYYYYMMDDThhmmss
~~~
Example: =|P00021015T103020|=

Extended format:
~~~
PYYYY-MM-DDThh:mm:ss
~~~
Example: =|P0002-10-15T10:30:20|=

The examples represent a time interval with a duration of 2 years,
10 months, 15 days, 10 hours, 30 minutes and 20 seconds.

### Intervals identified by start and duration

Combines any complete date and time of day representation with any complete
representation of duration.

Basic format:
~~~
YYYYMMDDThhmmss/PnnYnnMnnDTnnHnnMnnS
YYYYMMDDThhmmss/PYYYYMMDDThhmmss
~~~
Examples:
  * =|19850412T232050/P1Y2M15DT12H30M0S|=
  * =|19850412T232050/P00010215T123000|=

Extended format:
~~~
YYYY-MM-DDThh:mm:ss/PnnYnnMnnDTnnHnnMnnS
YYYY-MM-DDThh:mm:ss/PYYYY-MM-DDThh:mm:ss
~~~
Examples:
  * =|1985-04-12T23:20:50/P1Y2M15DT12H30M0S|=
  * =|1985-04-12T23:20:50/P0001-02-15T12:30:00|=

The examples represent a time interval of 1 year, 2 months, 15 days,
12 hours and 30 minutes, beginning on 12 April 1985 at 20 minutes and
50 seconds past 23 hours local time.

## Intervals identified by duration and end

Complete a representation of the duration with any complete representation
of date and time of day.

Basic format:
~~~
PnnYnnMnnDTnnHnnMnnS/YYYYMMDDThhmmss
PYYYYMMDDThhmmss/YYYYMMDDThhmmss
~~~
Examples:
  * =|P1Y2M15DT12H30M0S/19850412T232050|=
  * =|P00010215T123000/19850412T232050|=

Extended format:
~~~
PnnYnnMnnDTnnHnnMnnS/YYYY-MM-DDThh:mm:ss
PYYYY-MM-DDThh:mm:ss/YYYY-MM-DDThh:mm:ss
~~~
Examples:
  * =|P1Y2M15DT12H30M0S/1985-04-12T23:20:50|=
  * =|P0001-02-15T12:30:00/1985-04-12T23:20:50|=

The examples represent a time interval of 1 year, 2 months, 15 days and
12 hours and 30 minutes, ending on 12 April 1985 at 20 minutes and
50 seconds past 23 hours local time.

## Other complete representations

A complete representation of ordinal dates or week dates can be used instead
of calendar dates,

Instead of local time UTC time and local time plus UTC difference can be used.

## Non-complete representations

### For intervals represented by using a start and an end time point

Higher order time elements may be omitted from the expression following
the solidus (i.e. the representation for "end of time interval");
in such a case it shall be assumed that the corresponding time elements from
the "start of time interval" expression apply
(e.g. if =|[YYYYMM]|= are omitted, the end of the time interval is in
the same calendar year and calendar month as the start of the time interval).

Representations for time zones and UTC included with the component preceding
the solidus shall be assumed to apply to the component following the solidus,
unless a corresponding alternative is included.

--

# Recurring time interval

A recurring time interval shall be expressed in one of the following ways:
  * By a number of recurrences (optional), a start and an end.
    This represents a recurring time interval of which the first
    time interval is identified by the last two components of
    the expression and the number of recurrences by the first component.
    If the number of recurrences is absent, the number of occurrences is
    unbounded.
  * By a number of recurrences (optional), a duration and context.
    This represents a recurring time interval with the indicated duration
    for each time interval and with the indicated number of recurrences.
    If the number of recurrences is absent, the number of occurrences is
    unbounded.
  * By a number of recurrences (optional), a start and a duration.
    This represents a recurring time interval of which the first time
    interval is identified by the last two components of the expression
    and the number of recurrences by the first component. If the number
    of recurrences is absent, the number of occurrences is unbounded.
  * By a number of recurrences (optional), a duration and an end.
    This represents a recurring time interval of which the last time interval
    is identified by the last two components of the expression and
    the number of recurrences by the first component. If the number of
    recurrences is absent, the number of occurrences is unbounded.

All representations start with the designator =|[R]|=, followed,
without spaces, by the number of recurrences, if present, followed,
without spaces, by a solidus =|[/]|=, followed,
without spaces, by the expression of a time interval (see above).

## Complete representations

Basic format:
~~~
Rn/YYYYMMDDThhmmss/YYYYMMDDThhmmss
Rn/PnnYnnMnnDTnnHnnMnnS
Rn/YYYYMMDDThhmmss/PnnYnnMnnDTnnHnnMnnS
RnPnnYnnMnnDTnnHnnMnnS/YYYYMMDDThhmmss
~~~
Examples:
  * =|R12/19850412T232050/19850625T103000|=
  * =|R12/P2Y10M15DT10H30M20S|=
  * =|R12/19850412T232050/P1Y2M15DT12H30M0S|=
  * =|R12/P1Y2M15DT12H30M0S/19850412T232050|=

Extended format:
~~~
Rn/YYYY-MM-DDThh:mm:ss/YYYY-MM-DDThh:mm:ss
Rn/YYYY-MM-DDThh:mm:ss/PnYnMnDTnHnMnS
Rn/PnnYnnMnnDTnnHnnMnnS/YYYY-MM-DDThh:mm:ss
~~~
Examples:
  * =|R12/l985-04-12T23:20:50/1985-06-25T10:30:00|=
  * =|R12/1985-04-12T23:20:50/P1Y2M15DT12H30M0S|=
  * =|R12/P1Y2M15DT12H30M0S/1985-04-12T23:20:50|=

## Non-complete representations

Using a non-complete time interval representation (see above).

--

@author Wouter Beek
@tbd The alternative representations for time intervals are not supported.
@tbd The non-complete representations are not supported.
@version 2013/08
*/

:- use_module(datetime(iso8601_date_time)).
:- use_module(datetime(iso8601_generic)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(parse_tree)).



iso8601_duration(T0, iso8601_duration(Y,M,W,D,H,MM,S)) -->
  {var(M), var(W)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(D)} ; iso8601_number_of_days(T3, D)),
  {iso8601_time_designator(H, M, S, T)},
  ({T = true}, iso8601_time_designator(T4) ; {T = false}),
  ({var(H)} ; iso8601_number_of_hours(T5, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T6, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T7, S)),
  {parse_tree(iso8601_duration, [T1,T2,T3,T4,T5,T6,T7], T0)}.
iso8601_duration(T0, iso8601_duration(Y,M,W,D,H,MM,S)) -->
  {var(W)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(M)} ; iso8601_number_of_months(T3, M)),
  ({var(D)} ; iso8601_number_of_days(T4, D)),
  {iso8601_time_designator(H, M, S, T)},
  ({T = true}, iso8601_time_designator(T5) ; {T = false}),
  ({var(H)} ; iso8601_number_of_hours(T6, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T7, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T8, S)),
  {parse_tree(iso8601_duration, [T1,T2,T3,T4,T5,T6,T7,T8], T0)}.
iso8601_duration(T0, iso8601_duration(Y,M,W,D,H,MM,S)) -->
  {var(M)},
  iso8601_duration_designator(T1),
  ({var(Y)} ; iso8601_number_of_years(T2, Y)),
  ({var(W)} ; iso8601_number_of_weeks(T3, W)),
  ({var(D)} ; iso8601_number_of_days(T4, D)),
  {iso8601_time_designator(H, M, S, T)},
  ({T = true}, iso8601_time_designator(T5) ; {T = false}),
  ({var(H)} ; iso8601_number_of_hours(T6, H)),
  ({var(MM)} ; iso8601_number_of_minutes(T7, MM)),
  ({var(S)} ; iso8601_number_of_seconds(T8, S)),
  {parse_tree(iso8601_duration, [T1,T2,T3,T4,T5,T6,T7,T8], T0)}.

iso8601_recurring_time_interval(
  T0,
  Variant,
  Format,
  N,
  DateTime1,
  DateTime2
) -->
  iso8601_recurence_designator(T1),
  ({var(N)} ; decimal_number(N), {T2 = recurrences(N)}),
  forward_slash,
  iso8601_time_interval(T3, Variant, Format, DateTime1, DateTime2),
  {parse_tree(recurring_time_interval, [T1,T2,'/',T3], T0)}.

% Start of interval and end of interval.
iso8601_time_interval(T0, 1, Format, DateTime1, DateTime2) -->
  iso8601_date_time(T1, Format, DateTime1),
  iso8601_interval_separator(T2),
  iso8601_date_time(T3, Format, DateTime2),
  {parse_tree(time_interval, [T1,T2,T3], T0)}.
% Duration only.
iso8601_time_interval(
  time_interval(T0),
  2,
  _Format,
  date_time(date(Y,M,W,D),utc_time(time(H,MM,S),_UTC_Correction)),
  _DateTime2
) -->
  iso8601_duration(T0, duration(Y,M,W,D,H,MM,S)).
% Start of interval and duration.
iso8601_time_interval(T0, 3, Format, DateTime1, DateTime2) -->
  iso8601_date_time(T1, Format, DateTime1),
  iso8601_interval_separator(T2),
  iso8601_duration(T3, DateTime2),
  {parse_tree(time_interval, [T1,T2,T3], T0)}.
% Duration and end of interval.
iso8601_time_interval(T0, 4, Format, DateTime1, DateTime2) -->
  iso8601_duration(T1, DateTime1),
  iso8601_interval_separator(T2),
  iso8601_date_time(T3, Format, DateTime2),
  {parse_tree(time_interval, [T1,T2,T3], T0)}.



% SUPPORT PREDICATES %

iso8601_day_designator('D') -->
  "D".

iso8601_duration_designator(duration_designator('P')) -->
  "P".

iso8601_hour_designator('H') -->
  "H".

iso8601_interval_separator(interval_separator('/')) -->
  "/".

iso8601_minute_designator('M') -->
  "M".

iso8601_month_designator('M') -->
  "M".

iso8601_number_of_days(days(D,X), D) -->
  decimal_number(D),
  iso8601_day_designator(X).

iso8601_number_of_hours(hours(H,X), H) -->
  decimal_number(H),
  iso8601_hour_designator(X).

iso8601_number_of_minutes(minutes(M,X), M) -->
  decimal_number(M),
  iso8601_minute_designator(X).

iso8601_number_of_months(months(M,X), M) -->
  decimal_number(M),
  iso8601_month_designator(X).

iso8601_number_of_seconds(seconds(T1,X), S) -->
  iso8601_float(T1, seconds, _Length, S),
  iso8601_second_designator(X).

% Since weeks have no defined carry over point (52 or 53),
% weeks should not be used in these applications.
iso8601_number_of_weeks(weeks(W,X), W) -->
  decimal_number(W),
  iso8601_week_designator(X).

iso8601_number_of_years(years(Y,X), Y) -->
  decimal_number(Y),
  iso8601_year_designator(X).

iso8601_recurence_designator(recurrence_designator('R')) -->
  "R".

iso8601_second_designator('S') -->
  "S".

iso8601_year_designator('Y') -->
  "Y".

