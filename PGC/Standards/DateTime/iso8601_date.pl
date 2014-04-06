:- module(
  iso8601_date,
  [
    iso8601_calendar_date//3, % ?Tree:compound
                              % ?Format:oneof([basic,extended])
                              % ?Date:compound
    iso8601_ordinal_date//3, % -Tree:compound
                             % ?Format:oneof([basic,extended])
                             % ?Date:compound
    iso8601_week_date//3 % -Tree:compound
                         % ?Format:oneof([basic,extended])
                         % ?Date:compound
  ]
).

/** <module> ISO8601_DATE

# Custom datetypes

Date:
~~~
date(
  ?Year:between(0,9999),
  ?Month:between(1,12),
  ?Week:between(1,53),
  ?Day:between(1,366)
)
~~~

--

# Date representations

## Calendar date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|0000|= to
=|9999|=.
Values in the range =|0000|= through =|1582|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar month|* is represented by two digits.

*|Calendar day of the month|* is represented by two digits.

### Complete representations

Basic format:
~~~
YYYYMMDD
~~~
Example: =|19850412|=

Extended format:
~~~
YYYY-MM-DD
~~~
Example: =|1985-04-12|=

### Reduced representations

Reduced representations cannot be given in the extended format.

When only the day is omitted, a separator shall be inserted between the year
and the month, but separators shall not be used in the other representations
with reduced accuracy.

Month:
~~~
YYYY-MM
~~~
Example: =|1985-04|=

Year:
~~~
YYYY
~~~
Example: =|1985|=

Century:
~~~
YY
~~~
Example: =|19|=

### Expanded representations

Day (basic):
~~~
±_YYYYYYMMDD
~~~

Day (extended):
~~~
±_YYYYYY-MM-DD
~~~

Month:
~~~
±_YYYYYY-MM
~~~

Year:
~~~
±_YYYYYY
~~~

Century:
~~~
±_YYY
~~~

## Ordinal date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|0000|= to
=|9999|=.

Values in the range =|0000|= through =|1582|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar day of the year|* is represented by three decimal digits.
The calendar days are represented by =|001|= through =|365|= (leap year)
or =|366|= (common year), depending on the year.

### Complete representations

Basic format:
~~~
YYYYDDD
~~~
Example: =|1985102|=

Extended format:
~~~
YYYY-DDD
~~~
Example: =|1985-102|=

### Expanded representations

Basic format:
~~~
±YYYYYDDD
~~~
Example: =|+001985102|=

Extended format:
~~~
±YYYYY-DDD
~~~
Example: =|+001985-102|=

## Week date

*|Calendar year|* is, unless specified otherwise, represented by four digits,
according to the Gregorian calendar by values in the range =|[0000]|= to
=|[9999]|=.

Values in the range =|[0000]|= through =|[1582]|= shall only be
used by mutual agreement of the partners in information interchange.

*|Calendar week|* is represented by two decimal digits and ranges from
=|00|= to =|52|= or =|53|=, depending on the year.

*|Calendar day of the week|* is represented by one decimal digit,
see iso8601:calendar_day_name/2.

### Complete representations

Basic format:
~~~
YYYYWwwD
~~~
Example: =|1985W155|=

Extended format:
~~~
YYYY-Www-D
~~~
Example: =|1985-W15-5|=

### Representations with reduced accuracy

Basic format:
~~~
YYYYWww
~~~
Example: =|1985W15|=

Extended format:
~~~
YYYY-Www
~~~
Example: =|1985-W15|=

### Expanded representations

Basic format:
~~~
±YYYYYWwwD
~~~
Example: =|+001985W155|=

Extended format:
~~~
±YYYYY-Www-D
~~~
Example: =|+001985-W15-5|=

Basic format:
~~~
±YYYYYWww
~~~
Example: =|+001985W15|=

Extended format:
~~~
±YYYYY-Www
~~~
Example: =|+001985-W15|=

--

@author Wouter Beek
@version 2013/07
*/

:- use_remote_module(datetime(iso8601_generic)).
:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(parse_tree)).



%! iso8601_calendar_date(
%!   ?Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! )//

% We put several representation forms together in this DCG rule
% for compactness.
iso8601_calendar_date(T0, Format, date(Y,M,W,D)) -->
  {var(W)},
  % Note that a reduced representation may consist only of a century.
  % For generation purposes, we want to put the century before the year
  % (in generation we prefer shorter strings).
  ({Format = basic}, iso8601_century(T1, Y) ; iso8601_year(T1, Y)),
  (
    % Reduced representations may end here.
    {(var(M), var(D))}
  ;
    % Sometimes a separator occurs between year and month.
    (
      % The extended format of the complete representation
      % has a hyphen between year and month.
      hyphen, {Format = extended}
    ;
      % The basic format of the reduced representation
      % has a hyphen between year and month.
      hyphen, {Format = basic, var(D)}
    ;
      % The basic format of the complete representation
      % has no hyphen between year and month.
      {Format = basic}
    ),
    iso8601_month_in_year(T2, M),
    (
      % Reduced representations may end here.
      {var(D)}
    ;
      % Sometimes a separator occurs between month and day.
      (
        % The extended format of the complete representation
        % has a hyphen between month and day.
        hyphen, {Format = extended}
      ;
        {Format = basic}
      ),
      iso8601_day_in_month(T3, D)
    )
  ),
  {parse_tree(calendar_date, [T1,T2,T3], T0)}.

%! iso8601_ordinal_date(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! )//
% No reduced representations are defined for ordinal dates.

iso8601_ordinal_date(T0, Format, date(Y,M,W,D)) -->
  {var(M), var(W)},
  iso8601_year(T1, Y),
  % Sometimes a separator occurs between year and day.
  (
    % The extended format of the complete representation
    % has a hyphen between year and day.
    hyphen, {Format = extended}
  ;
    {Format = basic}
  ),
  iso8601_day_in_year(T2, D),
  {parse_tree(ordinal_date, [T1,T2], T0)}.

%! iso8601_week_date(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! )//

iso8601_week_date(T0, Format, date(Y,M,W,D)) -->
  {var(M)},
  iso8601_year(T1, Y),
  (hyphen, {Format = extended} ; {Format = basic}),
  iso8601_week_designator(T2),
  iso8601_week_in_year(T3, W),
  (
    {var(D)}
  ;
    (hyphen, {Format = extended} ; {Format = basic}),
    iso8601_day_in_week(T4, D),
    {parse_tree(week_date, [T1,T2,T3,T4], T0)}
  ).



% SPECIFIC SUPPORT PREDICATES %

iso8601_century(T0, C) -->
  {var(C)}, !,
  iso8601_integer(T0, century, 2, C_),
  {C is C_ * 100},
  {iso8601_year(C)}.
iso8601_century(T0, C) -->
  {C_ is C / 100},
  iso8601_integer(T0, century, 2, C_),
  {iso8601_year(C)}.

iso8601_day_in_month(T0, D) -->
  iso8601_integer(T0, day_in_month, 2, D),
  {iso8601_day_in_month(D)}.

iso8601_day_in_week(T0, D) -->
  iso8601_integer(T0, day_in_week, 1, D),
  {iso8601_day_in_week(D)}.

iso8601_day_in_year(T0, D) -->
  iso8601_integer(T0, day_in_year, 3, D),
  {iso8601_day_in_year(D)}.

iso8601_month_in_year(T0, M) -->
  iso8601_integer(T0, month, 2, M),
  {iso8601_month_in_year(M)}.

iso8601_week_in_year(T0, W) -->
  iso8601_integer(T0, week, 2, W),
  {iso8601_week_in_year(W)}.

iso8601_year(T0, Y) -->
  iso8601_integer(T0, year, 4, Y),
  {iso8601_year(Y)}.

