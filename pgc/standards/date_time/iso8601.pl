:- module(
  iso8601,
  [
    calendar_day_name/2, % ?DayOrdinal:between(1,7)
                         % ?DayName:atom
    calendar_month/4, % ?Year:between(0,9999),
                      % ?CalenderMonthNumber:between(1,12),
                      % ?NumberOfDaysInMonth:between(28,31),
                      % ?DatesOfDays:pair(between(1,336),between(31,366))
    centennial_year/1, % ?Year:between(0,9999)
    iso8601_day_in_month/1, % ?Day:between(1,31)
    iso8601_day_in_week/1, % ?Day:between(1,7)
    day_in_week/4, % +Year:between(0,9999)
                   % +Month:between(1,12)
                   % +Day:between(1,31)
                   % -DayInWeek:between(1,7)
    iso8601_day_in_year/1, % ?Day:between(1,366)
    iso8601_hour_in_day/1, % ?Hour:between(0,24)
    iso8601_leap_year/1, % ?Year:between(0,9999)
    iso8601_minute_in_hour/1, % ?Minute:between(0,59)
    iso8601_month_in_year/1, % ?Month:between(1,12)
    number_of_days_in_year/2, % +Year:between(0,9999)
                              % -NumberOfDaysInYear:integer
    number_of_weeks_in_year/2, % +Year:between(0,9999)
                               % -NumberOfWeeksInYear:between(52,53)
    iso8601_second_in_minute/1, % ?Second:between(0,59)
    iso8601_week_in_year/1, % ?Week:between(1,53)
    week_in_year/4, % +Year:between(0,9999)
                    % +Month:between(1,12)
                    % +Day:between(1,31)
                    % -WeekInYear:pair(between(0,9999),between(1,53))
    iso8601_year/1 % ?Year:between(0,9999)
  ]
).

/** <module> ISO_8601

Maintained by ISO Technical Committee TC 154.

# Basic concepts

Concepts taken from [iec60050-111]:
  * Time axis
  * Instance
  * Time interval
  * Time scale
  * Date / Time point / Time
  * Duration
  * Standard time

## Nominal duration

Duration expressed amongst others in years, months, weeks or days.

The duration of a calendar year, a calendar month, a calendar week or a
calendar day depends on its position in the calendar. Therefore, the exact
duration of a nominal duration can only be evaluated if the duration of the
calendar years, calendar months, calendar weeks or calendar days used are
known.

## Date ("Calendar date" in IEC 60050-111).

Time point representing a calendar day on a time scale consisting of an
origin and a succession of calendar days.

## Calendar date

Date representing a particular calendar day by its calendar year, its
calendar month and its ordinal number within its calendar month.

## Ordinal date

Date representing a particular calendar day by its calendar year and its
ordinal number within its calendar year.

## Week date

Date representing a particular calendar day by the calendar year to which
its calendar week belongs, the ordinal number of its calendar week within
that calendar year and its ordinal number within its calendar week.

## Coordinated Universal Time (UTC) [IEC 60050-713]

Time scale which forms the basis of a coordinated radio dissemination of
standard frequencies and time signals; it corresponds exactly in rate with
international atomic time, but differs from it by an integral number of
seconds.

UTC is established by the International Bureau of Weights and Measures
(BIPM, i.e. Bureau International des Poids et Mesures) and the International
Earth Rotation Service (IERS). UTC provides the basis of standard time,
the use of which is legal in most countries. The 15th Conférence Géneral des
Poids et Mesures (CGPM) (1975) judged in its Resolution 5 that this usage can
be strongly recommended.

UTC is adjusted to UT1 by the insertion or deletion of seconds, known as
*|leap seconds|*.

Greenwich Mean Time (GMT) is internationally replaced by UTC. UTC is often
(incorrectly) referred to as GMT.

UTC is generally used by aviation and maritime navigation that also uses local
apparent time and local mean time for celestial navigation (see ISO 19018).

Additional information can be found as follows:
  * The URL for the ITU http://www.itu.int/itudoc/itu-r/rec/tf/index.html
  * The URL for the International Bureau of Weights and Measures
    http://www.bipm.fr
  * The URL for the International Earth Rotation Service
    http://hpiers.obspm.fr

## UTC of day

Quantitative expression marking an instant within a calendar day in accordance
with UTC.

## Standard time of day ("Clock time" in IEC 60050-111)

Quantitative expression marking an instant within a calendar day by the
duration elapsed after midnight in the local standard time

## Local time

Locally applicable time of day such as standard time of day, or a non-UTC
based time of day.

## Recurring time interval

Series of consecutive time intervals of the same duration or nominal duration.

If the duration of the time intervals is measured in calendar entities, the
duration of each time interval depends on the calendar dates of its start and
its end.

# Time units, nominal durations and time intervals

## Second

Base unit of measurement of time in the International System of Units (SI)
as defined by the International Committee of Weights and Measures
(CIPM, i.e. Comité International des Poids et Mesures).

This is the base unit for expressing duration.

[ISO 31-1]

## Leap second

Intentional time step of one second to adjust UTC to ensure appropriate
agreement with UT1, a time scale based on the rotation of the Earth.

An inserted second is called *|positive leap second|* and an omitted second
is called *|negative leap second|*. A positive leap second is inserted between
[23:59:59Z] and [24:00:00Z] and can be represented as [23:59:60Z]. Negative
leap seconds are achieved by the omission of [23:59:59Z]. Insertion or
omission takes place as determined by IERS, normally on 30 June or
31 December, but if necessary on 31 March or 30 September.

[Rec. ITU-R TF.460-5]

## Minute

Unit of time, equal to 60 seconds.

[ISO 31-1]

## Hour

Unit of time, equal to 60 minutes.

[ISO 31-1]

## Calendar day

Time interval starting at midnight and ending at the next midnight, the latter
being also the starting instant of the next calendar day.

A calendar day is often also referred to as day.

The duration of a calendar day is 24 hours; except if modified by:
  * the insertion or deletion of leap seconds, by decision of the
    International Earth Rotation Service (IERS), or
  * the insertion or deletion of other time intervals, as may be prescribed
    by local authorities to alter the time scale of local time.

## Day (1)

Duration of a calendar day.

The term "day" applies also to the duration of any time interval which
starts at a certain time of day at a certain calendar day and ends at the same
time of day at the next calendar day.

## Day (2)

Unit of time, equal to 24 hours.

[ISO 31-1]

## Calendar week

Time interval of seven calendar days starting with a Monday.

A calendar week is often also referred to as week.

A calendar week may be identified by its ordinal number within its calendar
year.

## Week

Duration of a calendar week.

The term "week" applies also to the duration of any time interval which starts
at a certain time of day at a certain calendar day and ends at the same time
of day at the same calendar day of the next calendar week.

## Calendar week number

Ordinal number which identifies a calendar week within its calendar year
according to the rule that the first calendar week of a year is that one which
includes the first Thursday of that year and that the last calendar
week of a calendar year is the week immediately preceding the first calendar
week of the next calendar year.

## Calendar month

Time interval resulting from the division of a calendar year in 12 time
intervals, each with a specific name and containing a specific number of
calendar days.

A calendar month is often referred to as month.

## Month

Duration of 28, 29, 30 or 31 calendar days depending on the start and/or the
end of the corresponding time interval within the specific calendar month.

The term "month" applies also to the duration of any time interval which
starts at a certain time of day at a certain calendar day of the calendar
month and ends at the same time of day at the same calendar day of the next
calendar month, if it exists. In other cases the ending calendar day has to
be agreed on.

In certain applications a month is considered as a duration of 30 calendar
days.

## Calendar year

Cyclic time interval in a calendar which is required for one revolution of the
Earth around the Sun and approximated to an integral number of calendar days.

A calendar year is often also referred to as year.

Unless otherwise specified the term designates in this International Standard
a calendar year in the Gregorian calendar.

## Year

Duration of 365 or 366 calendar days depending on the start and/or the end of
the corresponding time interval within the specific calendar year.

The term "year" applies also to the duration of any time interval which starts
at a certain time of day at a certain calendar date of the calendar year and
ends at the same time of day at the same calendar date of the next calendar
year, if it exists. In other cases the ending calendar date has to be agreed
on.

## Gregorian calendar

Calendar in general use, introduced in 1582 to define a calendar year that
more closely approximated the tropical year than the Julian calendar.

## Common year

Calendar year in the Gregorian calendar that has 365 calendar days.

## Leap year

Calendar year in the Gregorian calendar that has 366 calendar days.

## Centennial year

Calendar year in the Gregorian calendar whose year number is divisible without
remainder by hundred.

--

# From previous ISOs

## From ISO 2014

Introduced the all-numeric date notation in most-to-least-significant
order =|[YYYY]-[MM]-[DD]|=.

## From ISO 2015

The week numbering system

## From ISO 2711

The identification of days by ordinal dates.

# Principles

  * The lexicographical order of the representation corresponds to
    chronological order, by ordering the values from most to least
    significant. (Except for negative years.)
  * Values have a fixed length that must be padded by zeros.
  * Two representation formats: a basic format with a minimal number of
    separators, and an extended format with separators to enhance human
    readability.
  * The separator used between date values is the hyphen; the colon is used
    as the separator between time values.
  * Reduced precision/accuracy by dropping any of the values in the order from
    least to most significant.
  * Supports the addition of a decimal fraction to the smallest time value.

# Gregorian calendar

This International Standard uses the Gregorian calendar for the identification
of calendar days.
It  distinguishes common years of 365 consecutive calendar days and leap years
of 366 consecutive calendar days.
It has a reference point that assigns 20 May 1875 to the calendar day that the
*|Convention du Mètre|* was signed in Paris.

## Leap year

A year whose year number is divisible by four an integral number of times.
However, a centennial year is not a leap year unless its year number is
divisible by four hundred an integral number of times.

## Common year

A year that is not a leap year.

## Proleptic Gregorian calendar

The use of this calendar for dates preceding the introduction of the Gregorian
calendar (also called the *|proleptic Gregorian calendar|*) should only be by
agreement of the partners in information interchange.

The introduction of the Gregorian calendar included the cancellation of the
accumulated inaccuracies of the Julian calendar. However, no dates shall be
inserted or deleted when determining dates in the proleptic Gregorian
calendar.

The Gregorian calendar was introduced on 15 October 1582. In the calendar set
by this standard the calendar day preceding that calendar day is referred to
as 14 October 1582. In the Julian calendar that calendar day is referred to
as 4 October 1582.

## First week of year

The rule for determining the first calendar week (based on the first Thursday
of the year, is equivalent with the rule "the first calendar week is the
calendar week which includes 4 January".

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(aggregate)).



%! calendar_day_name(
%!   ?OrdinalDayNumberInWeek:between(1,7),
%!   ?CalendarDayName:atom
%! ) is nondet.
% Names for calendar day ordinals.

calendar_day_name(1, 'Monday').
calendar_day_name(2, 'Tuesday').
calendar_day_name(3, 'Wednesday').
calendar_day_name(4, 'Thursday').
calendar_day_name(5, 'Friday').
calendar_day_name(6, 'Saturday').
calendar_day_name(7, 'Sunday').

%! calendar_month(
%!   ?Year:between(0,9999),
%!   ?CalenderMonthNumber:between(1,12),
%!   ?NumberOfDaysInMonth:between(28,31),
%!   ?DatesOfDays:pair(between(1,336),between(31,366))
%! ) is nondet.
% Properties of calender months according to the Gregorian calender.
%
% A *|calender month|* is defined as a time interval resulting from the
% division of a calendar year in 12 time intervals, each with a specific
% name and containing a specific number of calendar days.

calendar_month(_Y, 1, 31, 1-31).
calendar_month(Y, 2, NumberOfNamesInMonth, DatesOfDays):-
  (
    iso8601_leap_year(Y)
  ->
    NumberOfNamesInMonth = 29,
    DatesOfDays = 32-59
  ;
    NumberOfNamesInMonth = 28,
    DatesOfDays = 32-60
  ).
calendar_month(Y, 3, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 60-90 ; DatesOfDays = 61-91).
calendar_month(Y, 4, 30, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 91-120 ; DatesOfDays = 92-121).
calendar_month(Y, 5, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 121-151 ; DatesOfDays = 122-152).
calendar_month(Y, 6, 30, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 152-181 ; DatesOfDays = 153-182).
calendar_month(Y, 7, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 182-212 ; DatesOfDays = 183-213).
calendar_month(Y, 8, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 213-243 ; DatesOfDays = 214-244).
calendar_month(Y, 9, 30, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 244-273 ; DatesOfDays = 245-274).
calendar_month(Y, 10, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 274-304 ; DatesOfDays = 275-304).
calendar_month(Y, 11, 30, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 305-334 ; DatesOfDays = 306-335).
calendar_month(Y, 12, 31, DatesOfDays):-
  (iso8601_leap_year(Y) -> DatesOfDays = 335-365 ; DatesOfDays = 336-366).

%! calendar_month_name(?DayOrdinal:between(1,12), ?DayName:atom) is nondet.
% Names for calendar month ordinals.

calendar_month_name(1,  'January'  ).
calendar_month_name(2,  'February' ).
calendar_month_name(3,  'March'    ).
calendar_month_name(4,  'April'    ).
calendar_month_name(5,  'May'      ).
calendar_month_name(6,  'June'     ).
calendar_month_name(7,  'July'     ).
calendar_month_name(8,  'August'   ).
calendar_month_name(9,  'September').
calendar_month_name(10, 'October'  ).
calendar_month_name(11, 'November' ).
calendar_month_name(12, 'December' ).

%! centennial_year(?Year:between(0,9999)) is nondet.
% Centennial years according to the Gregorian calender.
%
% A *|centennial year|* is a year whose year number is divisible by 100 an
% integral number of times.

centennial_year(Y):-
  iso8601_year(Y),
  Y rem 100 =:= 0.

%! day_diff(
%!   +Y1:between(0,9999),
%!   +M1:between(1,12),
%!   +D1:between(28,31),
%!   +Y2:between(0,9999),
%!   +M2:between(1,12),
%!   +D2:between(28,31),
%!   -Diff:integer
%! ) is det.
% Returns the number of days between the two given dates.

% If the first date is earlier than the second date, then swap those dates.
day_diff(Y1, M1, D1, Y2, M2, D2, Diff2):-
  (
    Y1 < Y2
  ;
    Y1 = Y2,
    M1 < M2
  ;
    Y1 = Y2,
    M1 = M2,
    D1 < D2
  ), !,
  day_diff(Y2, M2, D2, Y1, M1, D1, Diff1),
  Diff2 is -1 * Diff1.
% When the year and month are the same, then the difference in days is
% straightforward.
day_diff(Y, M, D1, Y, M, D2, Diff):- !,
  Diff is D1 - D2.
% First take care of the days.
day_diff(Y1, M1, D1, Y2, M2, D2, Diff2):-
  NewMonth1 is M1 - 1,
  day_diff(Y1, NewMonth1, Y2, M2, D1, Diff1),
  calendar_month(Y2, M2, NumberOfDays2, _),
  Diff2 is Diff1 + NumberOfDays2 - D2.

% Done!
day_diff(Y, M, Y, M, Diff, Diff):- !.
% When we run out of months, we move the year back.
day_diff(Y1, 0, Y2, M2, X, Diff):- !,
  NewY1 is Y1 - 1,
  day_diff(NewY1, 12, Y2, M2, X, Diff).
% We know that the first date is still later than the second date.
% We move the first date back for one month, adding the number of days
% to the cumulative difference.
day_diff(Y1, M1, Y2, M2, X, Diff):-
  calendar_month(Y1, M1, NumberOfDays1, _),
  NewX is X + NumberOfDays1,
  NewM1 is M1 - 1,
  day_diff(Y1, NewM1, Y2, M2, NewX, Diff).

iso8601_day_in_month(D):-
  between(1, 31, D).

iso8601_day_in_week(D):-
  between(1, 7, D).

%! day_in_week(
%!   +Year:between(0,9999),
%!   +Month:between(1,12),
%!   +Day:between(28,31),
%!   -OrdinalDayInWeek:between(1,7)
%! ) is det.
% Returns the ordinal number of day in week (an integer between 1 and 7)
% of the given date.

day_in_week(Y, M, D, OrdinalDayInWeek2):-
  legal_date(Y, M, D),
  % 2000-01-01 is a Sunday according to the ISO standard, but Monday is the
  % first day of the week.
  % We therefore calculate the difference relative to 2000-01-03.
  day_diff(Y, M, D, 2000, 1, 3, Diff),
  OrdinalDayInWeek1 is Diff rem 7,
  (OrdinalDayInWeek1 >= 0 -> Correction = 1 ; Correction = 8),
  OrdinalDayInWeek2 is OrdinalDayInWeek1 rem 7 + Correction.

%! first_thursday_of_the_year(
%!   +Year:between(0,9999),
%!   -FirstThursday:integer
%! ) is det.
% The first Thursday of the year is used in the definition of the first week
% of the year.

first_thursday_of_the_year(Y, FirstThursday):-
  day_in_week(Y, 1, 1, DayOfWeek),
  FirstThursday is (7 - ((DayOfWeek + 2) mod 7)) mod 7.

%! iso8601_day_in_year(?Day:between(1,366)) is nondet.

iso8601_day_in_year(D):-
  between(1, 366, D).

%! iso8601_hour_in_day(?Hour:between(0,24)) is nondet.

iso8601_hour_in_day(H):-
  0.0 =< H, H =< 24.0.

%! last_day_of_last_week_of_year(+Year:between(0,9999), -Day:integer) is det.
% The last day of the last week of the year is not always the last day of the
% year.

last_day_of_last_week_of_year(Y, D):-
  calendar_month(Y, 12, LastDay, _),
  day_in_week(Y, 12, LastDay, DayInWeek),
  D is LastDay - (DayInWeek mod 7).

%! iso8601_leap_year(?Year:between(0,9999)) is nondet.
% Leap years according to the Gregorian calendar.
%
% A *|leap year|* is a year whose year number is divisible by 4 an integral
% number of times. However, a centennial year is not a leap year unless its
% year number is divisible by 400 an integral number of times.

iso8601_leap_year(Y):-
  iso8601_year(Y),
  Y rem 4 =:= 0,
  (\+ centennial_year(Y) ; Y rem 400 =:= 0).

%! legal_date(
%!   +Year:between(0,9999),
%!   +Month:between(1,12),
%!   +Day:between(1,31)
%! ) is semidet.
% A legal date has integers for year, month, and day within specific ranges.

legal_date(Y, M, D):-
  iso8601_year(Y),
  calendar_month(Y, M, NumberOfDays, _),
  between(1, NumberOfDays, D).

%! iso8601_minute_in_hour(?Minute:between(0,59)) is nondet.

iso8601_minute_in_hour(M):-
  0.0 =< M, M < 60.0.

iso8601_month_in_year(M):-
  between(1, 12 , M).

%! number_of_days_in_year(
%!   +Year:between(0,9999),
%!   -NumberOfDaysInYear:integer
%! ) is det.

number_of_days_in_year(Y, NumberOfDaysInYear):-
  aggregate_all(
    sum(N),
    number_of_days_in_month_(Y, N),
    NumberOfDaysInYear
  ).

number_of_days_in_month_(Y, N):-
  between(1, 12, M),
  calendar_month(Y, M, N, _).

%! number_of_weeks_in_year(
%!   +Year:between(0,9999),
%!   -NumberOfWeeksInYear:between(52,53)
%! ) is det.
% Returns the number of weeks for the given year.

number_of_weeks_in_year(Y, NumberOfWeeksInYear):-
  first_thursday_of_the_year(Y, FirstThursdayOfY),
  SuccY is Y + 1,
  first_thursday_of_the_year(SuccY, FirstThursdayOfSuccY),
  day_diff(SuccY, 1, FirstThursdayOfSuccY, Y, 1, FirstThursdayOfY, Diff),
  NumberOfWeeksInYear is Diff / 7.

%! iso8601_second_in_minute(?Second:between(0,59)) is nondet.

iso8601_second_in_minute(S):-
  0.0 =< S, S < 60.0.

iso8601_week_in_year(W):-
  between(1, 53, W).

%! week_in_year(
%!   +Year:between(0,9999),
%!   +Month:between(1,12),
%!   +Day:between(28,31),
%!   -WeekInYear:pair(between(0,9999),between(1,53))
%! ) is det.
% Returns the ordinal number of the week in year (between 1 and 53)
% of the given date.
%
% The calendar week number identifies the calendar week within the calendar
% year. Each calendar week has seven calendar days.
%
% The reference point of the time scale assigns Saturday to 1 January 2000.
%
% *|Calendar week|* is defined as a time interval of seven calendar days
% starting with a Monday.
%
% A *|calendar week number|* is defined as an ordinal number which identifies
% a calendar week within its calendar year according to the rule that the
% first calendar week of a year is that one which includes the first Thursday
% of that year and that the last calendar week of a calendar year is the
% week immediately preceding the first calendar week of the next calendar
% year.

% The date denotes a day in the first month of the year: the day may belong
% to the last week of the previous year.
week_in_year(Y, 1, D, WY-W):- !,
  first_thursday_of_the_year(Y, FirstThursdayOfY),
  (
    D =< FirstThursdayOfY - 4
  ->
    WY is Y - 1,
    calendar_month(WY, 12, LastDayOfLastMonthOfPredY, _),
    week_in_year_(WY, 12, LastDayOfLastMonthOfPredY, W)
  ;
    WY = Y,
    week_in_year_(Y, 1, D, W)
  ).
% The date denotes a day in the last month of the year: the day may belong
% to the first week of the next year.
week_in_year(Y, 12, D, WY-W):- !,
  SuccY is Y + 1,
  first_thursday_of_the_year(SuccY, FirstThursdayOfSuccY),
  (
    D >= FirstThursdayOfSuccY - 3
  ->
    WY = SuccY,
    W = 1
  ;
    WY = Y,
    week_in_year_(Y, 12, D, W)
  ).
% The date denotes a day in month 2 though 11: the day must belong to some
% week of the given year.
week_in_year(Y, M, D, Y-W):-
  week_in_year_(Y, M, D, W).

% The common part of determining the week number for a given date.
% Once we have taken care of those cases in which the year of the week of the
% given date may be different from the year in that date, we can simply
% calculate the number of days since the first date of the year,
% and take the ceiling of that number divided by the week size.
week_in_year_(Y, M, D, W):-
  day_diff(Y, M, D, Y, 1, 1, Diff),
  W is ceil(Diff / 7).

%! iso8601_year(?Year:between(0,9999)) is nondet.

iso8601_year(Y):-
  between(0, 9999, Y).

