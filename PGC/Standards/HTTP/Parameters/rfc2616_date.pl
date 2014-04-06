:- module(
  rfc2616_date,
  [
    'delta-seconds'//2, % -ParseTree:compound
                        % ?Seconds:nonneg
    'HTTP-date'//2 % -ParseTree:compound
                   % ?Date:compound
  ]
).

/** <module> RFC 2616 date-time

Date-time values for RFC 2616 (HTTP 1.1).

# Compounds

## Date

~~~{.pl}
date(
  Year:between(0,9999),
  Month:between(1,12),
  Day:between(0,99),
  Time:compound
)
~~~

## Time

~~~{.pl}
time(
  ?Hour:between(0,99),
  ?Minute:between(0,99),
  ?Second:between(0,99)
)
~~~

# RFC 2616

## Syntax

~~~{.abnf}
HTTP-date    = rfc1123-date | rfc850-date | asctime-date
rfc1123-date = wkday "," SP date1 SP time SP "GMT"
rfc850-date  = weekday "," SP date2 SP time SP "GMT"
asctime-date = wkday SP date3 SP time SP 4DIGIT
date1        = 2DIGIT SP month SP 4DIGIT
               ; day month year (e.g., 02 Jun 1982)
date2        = 2DIGIT "-" month "-" 2DIGIT
               ; day-month-year (e.g., 02-Jun-82)
date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
               ; month day (e.g., Jun  2)
time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
               ; 00:00:00 - 23:59:59
wkday        = "Mon" | "Tue" | "Wed"
             | "Thu" | "Fri" | "Sat" | "Sun"
weekday      = "Monday" | "Tuesday" | "Wednesday"
             | "Thursday" | "Friday" | "Saturday" | "Sunday"
month        = "Jan" | "Feb" | "Mar" | "Apr"
             | "May" | "Jun" | "Jul" | "Aug"
             | "Sep" | "Oct" | "Nov" | "Dec"
~~~

## Formats

HTTP applications have historically allowed three different formats
 for the representation of date/time stamps:
~~~{.txt}
[1]   Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
[2]   Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
[3]   Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
~~~

The first format is preferred as an Internet standard and represents
 a fixed-length subset of that defined by RFC 1123
 (an update to RFC 822).
The second format is in common use,
 but is based on the obsolete RFC 850 date format
 and lacks a four-digit year.

HTTP-date is case sensitive and MUST NOT include additional `LWS`
 beyond that specifically included as `SP` in the grammar.

@see RFC 822
@see RFC 850
@see RFC 1123

## Conformance

### Parsing

HTTP/1.1 clients and servers that parse the date value MUST accept
 all three formats (for compatibility with HTTP/1.0).

Note: Recipients of date values are encouraged to be robust in
 accepting date values that may have been sent by non-HTTP
 applications, as is sometimes the case when retrieving or posting
 messages via proxies/gateways to SMTP or NNTP.

### Generation

HTTP/1.1 clients and servers MUST only generate the RFC 1123 format
 for representing HTTP-date values in header fields.

## Pragmatics

All HTTP date/time stamps MUST be represented in Greenwich Mean Time (GMT),
 without exception.
For the purposes of HTTP, GMT is exactly equal to
 UTC (Coordinated Universal Time).
This is indicated in the first two formats by the inclusion of `GMT`
 as the three-letter abbreviation for time zone,
 and MUST be assumed when reading the `asctime` format.

Note: HTTP requirements for the date/time stamp format apply only
 to their usage within the protocol stream. Clients and servers are not
 required to use these formats for user presentation, request logging, etc.

--

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).
:- use_module(math(radix)).



%! 'asctime-date'(-ParseTree:compound, ?Date:compound)//
% Date-time for ANSI C.
%
% ~~~{.abnf}
% asctime-date = wkday SP date3 SP time SP 4DIGIT
% ~~~
%
% @see ANSI C
% @see RFC 2616

% @tbd Conversion between `D1` and `D2`.
'asctime-date'('asctime-date'(T1,T2,T3,Year), date(Year,Month,Day,Time)) -->
  wkday(T1, Day),
  'SP',
  date3(T2, Month, Day),
  'SP',
  time(T3, Time),
  'SP',
  dcg_multi2('DIGIT', 4-4, _, [Y1,Y2,Y3,Y4]),
  {digits_to_decimal([Y1,Y2,Y3,Y4], Year)}.



%! date1(
%!   -ParseTree:compound,
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(0,99)
%! )//
% Date for RFC 1123.
%
% ~~~{.abnf}
% date1 = 2DIGIT SP month SP 4DIGIT   ; day month year (e.g., 02 Jun 1982)
% ~~~
%
% @see RFC 1123
% @see RFC 2616

date1(date1(Day,T1,Year), Year, Month, Day) -->
  % Day
  dcg_multi2('DIGIT', 2-2, _, [D1,D2]),
  {digits_to_decimal([D1,D2], Day)},
  'SP',
  
  % Month
  month(T1, Month),
  'SP',
  
  % Year
  dcg_multi2('DIGIT', 4-4, _, [Y1,Y2,Y3,Y4]),
  {digits_to_decimal([Y1,Y2,Y3,Y4], Year)}.



%! date2(
%!   -ParseTree:compound,
%!   ?Year:between(0,99),
%!   ?Month:between(1,12),
%!   ?Day:between(0,99)
%! )//
% Date for RFC 850.
%
% ~~~{.abnf}
% date2 = 2DIGIT "-" month "-" 2DIGIT   ; day-month-year (e.g., 02-Jun-82)
% ~~~
%
% @see RFC 850
% @see RFC 2616

date2(date2(Year,T1,Day), Year, Month, Day) -->
  % Day
  dcg_multi2('DIGIT', 2-2, _, [Y1,Y2]),
  {digits_to_decimal([Y1,Y2], Year)},
  "-",
  
  % Month
  month(T1, Month),
  "-",
  
  % Year
  dcg_multi2('DIGIT', 2-2, _, [D1,D2]),
  {digits_to_decimal([D1,D2], Day)}.



%! date3(-ParseTree:compound, ?Month:between(1,12), ?Day:between(0,99))//
% Date for ANSI C.
%
% ~~~{.abnf}
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; month day (e.g., Jun  2)
% ~~~
%
% @see ANSI C
% @see RFC 2616

date3(date3(T1,Day), Month, Day) -->
  % Month
  month(T1, Month),
  'SP',
  
  % Day
  (
    dcg_multi2('DIGIT', 2-2, _, [D1,D2]),
    {digits_to_decimal([D1,D2], Day)}
  ;
    'SP',
    'DIGIT'(_, Day)
  ).



%! 'delta-seconds'(-ParseTree:compound, ?Seconds:nonneg)//
% Some HTTP header fields allow a time value to be specified as an
%  integer number of seconds, represented in decimal,
%  after the time that the message was received.
%
% ~~~{.abnf}
% delta-seconds = 1*DIGIT
% ~~~
%
% @see RFC 2616

'delta-seconds'('delta-seconds'(Seconds), Seconds) -->
  dcg_multi2('DIGIT', 1-_, _, Ss),
  {digits_to_decimal(Ss, Seconds)}.



%! 'HTTP-date'(-ParseTree:compound, ?Date:compound)//
% Date-time for HTTP.
%
% ~~~{.abnf}
% HTTP-date  = rfc1123-date | rfc850-date | asctime-date
% ~~~
%
% @arg ParseTree
% @arg Date
%
% @see ANSI C
% @see RFC 850
% @see RFC 1123
% @see RFC 2616

'HTTP-date'('HTTP-date'(T1), Date) -->
  'rfc1123-date'(T1, Date).
'HTTP-date'('HTTP-date'(T1), Date) -->
  'rfc850-date'(T1, Date).
'HTTP-date'('HTTP-date'(T1), Date) -->
  'asctime-date'(T1, Date).



%! month(-ParseTree:compound, ?Month:between(1,12))//
% Month names used in HTTP dates.
%
% ~~~{.abnf}
% month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ~~~
%
% @see RFC 2616

month(month('Jan'), 1) -->
  "Jan".
month(month('Feb'), 2) -->
  "Feb".
month(month('Mar'), 3) -->
  "Mar".
month(month('Apr'), 4) -->
  "Apr".
month(month('May'), 5) -->
  "May".
month(month('Jun'), 6) -->
  "Jun".
month(month('Jul'), 7) -->
  "Jul".
month(month('Aug'), 8) -->
  "Aug".
month(month('Sep'), 9) -->
  "Sep".
month(month('Oct'), 10) -->
  "Oct".
month(month('Nov'), 11) -->
  "Nov".
month(month('Dec'), 12) -->
  "Dec".



%! 'rfc1123-date'(-ParseTree:compound, ?Date:compound)//
% Date-time for RFC 1123.
%
% ~~~{.abnf}
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ~~~
%
% @see RFC 1123
% @see RFC 2616

'rfc1123-date'('rfc1123-date'(T1,T2,T3,'GMT'), date(Year,Month,Day,Time)) -->
  wkday(T1, Day),
  ",",
  'SP',
  date1(T2, Year, Month, Day),
  'SP',
  time(T3, Time),
  'SP',
  "GMT".



%! 'rfc850-date'(-ParseTree:compound, ?Date:compound)//
% Date-time for RFC 850.
%
% ~~~{.abnf}
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ~~~
%
% @see RFC 850
% @see RFC 2616

'rfc850-date'('rfc850-date'(T1,T2,T3,'GMT'), date(Year,Month,Day,Time)) -->
  weekday(T1, Day),
  ",",
  'SP',
  date2(T2, Year, Month, Day),
  'SP',
  time(T3, Time),
  'SP',
  "GMT".



%! time(-ParseTree:compound, ?Time:compound)//
% Time for HTTP (for ANSI C, RFC 850, and RFC 1123).
%
% ~~~{.abnf}
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT   ; 00:00:00 - 23:59:59
% ~~~
%
% @see RFC 2616

time(time(Hour,Minute,Second), time(Hour,Minute,Second)) -->
  % Hour
  dcg_multi2('DIGIT', 2-2, _, [H1,H2]),
  {digits_to_decimal([H1,H2], Hour)},
  ":",
  % Minute
  dcg_multi2('DIGIT', 2-2, _, [M1,M2]),
  {digits_to_decimal([M1,M2], Minute)},
  ":",
  % Second
  dcg_multi2('DIGIT', 2-2, _, [S1,S2]),
  {digits_to_decimal([S1,S2], Second)}.



%! weekday(-ParseTree:compound, ?Day:between(1,7))//
% Full weekday names.
%
% ~~~{.abnf}
% weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday"
%         | "Saturday" | "Sunday"
% ~~~
%
% @RFC 2616

weekday(weekday('Monday'), 1) -->
  "Monday".
weekday(weekday('Tuesday'), 2) -->
  "Tuesday".
weekday(weekday('Wednesday'), 3) -->
  "Wednesday".
weekday(weekday('Thursday'), 4) -->
  "Thursday".
weekday(weekday('Friday'), 5) -->
  "Friday".
weekday(weekday('Saturday'), 6) -->
  "Saturday".
weekday(weekday('Sunday'), 7) -->
  "Sunday".



%! wkday(-ParseTree:compound, ?Day:between(1,7))//
% Abbreviated weekday names.
%
% ~~~{.abnf}
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ~~~
%
% @see RFC 2616

wkday(wkday('Mon'), 1) -->
  "Mon".
wkday(wkday('Tue'), 2) -->
  "Tue".
wkday(wkday('Wed'), 3) -->
  "Wed".
wkday(wkday('Thu'), 4) -->
  "Thu".
wkday(wkday('Fri'), 5) -->
  "Fri".
wkday(wkday('Sat'), 6) -->
  "Sat".
wkday(wkday('Sun'), 7) -->
  "Sun".

