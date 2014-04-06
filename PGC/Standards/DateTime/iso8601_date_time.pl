:- module(
  iso8601_date_time,
  [
    iso8601_calendar_date_time//3, % -Tree:compound
                                   % ?Format:oneof([basic,extended])
                                   % ?DateTime:compound
    iso8601_date_time//3, % -Tree:compound
                          % ?Format:oneof([basic,extended])
                          % ?DateTime:compound
    iso8601_ordinal_date_time//3, % -Tree:compound
                                  % ?Format:oneof([basic,extended])
                                  % ?DateTime:compound
    iso8601_week_date_time//3 % -Tree:compound
                              % ?Format:oneof([basic,extended])
                              % ?DateTime:compound
  ]
).

/** <module> ISO8601_DATE_TIME

# Custom datetypes

=|date/4|= from module [iso8601_date].

=|utc_time/2|= from module [iso8601_time_point].

Datetime:
~~~
date_time(
  ?Date:compound,
  ?UTC_Time:compound
)
~~~

--

# Time designator

Notice that we are strict about the insertion of the time designator =|[T]|=,
in that we require it to be in place in all date-time representations with
non-empty time.

We do this because otherwise there will be abmbiguities.
For example =198504122320Z= can be parsed as =|1985-04-12T23:20Z=|
or as =|1985-04T12:23:20Z|=.

--

# Date and time of day representations

There are 3 variants:
  1. For *|calendar dates|*: year – month – day of the month –
     time designator – hour – minute – second – zone designator.
  2. For *|ordinal dates|*: year – day of the year – time designator – hour –
     minute – second – zone designator
  3. For *|week dates|*: year – week designator – week – day of the week –
     time designator – hour – minute – second – zone designator.

The zone indicator is either =|[Z]|=, in case of UTC of day, or a difference
components, in case of local time.

Time designator =|[T]|= is used to indicate the start of the representation of
the time of day component. It may be omitted in applications where there is no
risk of confusing a date and time of day representation with others defined in
this International Standard, and where there is mutual agreement between the
partners in information interchange.

## Examples of complete representations

Basic format:
~~~
YYYYMMDDThhmmss
YYYYMMDDThhmmssZ
YYYYMMDDThhmmss±hhmm
YYYYMMDDThhmmss±hh
~~~
Examples:
  * =|19850412T101530|=
  * =|19850412T101530Z|=
  * =|19850412T101530+0400|=
  * =|19850412T101530+04|=

Extended format:
~~~
YYYY-MM-DDThh:mm:ss
YYYY-MM-DDThh:mm:ssZ
YYYY-MM-DDThh:mm:ss±hh:mm
YYYY-MM-DDThh:mm:ss±hh
~~~
Examples:
  * =|1985-04-12T10:15:30|=
  * =|1985-04-12T10:15:30Z|=
  * =|1985-04-12T10:15:30+04:00|=
  * =|1985-04-12T10:15:30+04|=

In complete representations, calendar dates, ordinal dates, and week dates
may be use.

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(datetime(iso8601_date)).
:- use_module(datetime(iso8601_generic)).
:- use_module(datetime(iso8601_time_point)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(parse_tree)).



iso8601_calendar_date_time(T0, Format, date_time(Date,UTC_Time)) -->
  iso8601_calendar_date(T1, Format, Date),
  {iso8601_time_designator(UTC_Time, T)},
  iso8601_local_time(T2, Format, T, UTC_Time),
  {parse_tree(date_time, [T1,T2], T0)}.

%! iso8601_date_time(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?DateTime:compound
%! )//

iso8601_date_time(T0, Format, DateTime) -->
  iso8601_calendar_date_time(T0, Format, DateTime).
iso8601_date_time(T0, Format, DateTime) -->
  iso8601_ordinal_date_time(T0, Format, DateTime).
iso8601_date_time(T0, Format, DateTime) -->
  iso8601_week_date_time(T0, Format, DateTime).

iso8601_ordinal_date_time(T0, Format, date_time(Date,UTC_Time)) -->
  iso8601_ordinal_date(T1, Format, Date),
  {iso8601_time_designator(UTC_Time, T)},
  iso8601_local_time(T2, Format, T, UTC_Time),
  {parse_tree(date_time, [T1,T2], T0)}.

iso8601_week_date_time(T0, Format, date_time(Date,UTC_Time)) -->
  iso8601_week_date(T1, Format, Date),
  {iso8601_time_designator(UTC_Time, T)},
  iso8601_local_time(T2, Format, T, UTC_Time),
  {parse_tree(date_time, [T1,T2], T0)}.

