:- module(
  rfc1123,
  [
    rfc1123_datetime//1, % -Tree:compound
    rfc1123_datetime//8, % -Tree:compound
                         % ?Weekday:atom
                         % ?Day:integer
                         % ?Month:atom
                         % ?Year:integer
                         % ?Hour:integer
                         % ?Minute:integer
                         % ?Seconds:integer
    rfc1123_datetime_to_gv/1 % +Datetime:atom
  ]
).

/** <module> RFC 1123

The standard for datetime that is used by HTTP 1.1.

@author Wouter Beek
@compat RFC 1123
@see http://www.ietf.org/rfc/rfc1123.txt
@version 2013/07, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(gv(gv_file)).
:- use_module(library(plunit)).
:- use_module(math(radix)).



%! rfc1123_date(-Tree:compound, ?Day:integer, ?Month:atom, ?Year:integer)//
% The RFC 1123 data format.
%
% Example:
% ~~~{.bnf}
% 02 Jun 1982
% ~~~
%
% @arg Tree A parse tree.
% @arg Day An integer between 1 and 99 (going beyond intended use).
% @arg Month An atom. See rfc1123_month//2.
% @arg Year An integer between 0 and 9999.

rfc1123_date(date(day(DD),T1,year(YYYY)), DD, Month, YYYY) -->
  {maplist(nonvar, [DD,Month,YYYY])}, !,
  {
    decimal_to_digits(DD, [D1,D2]),
    decimal_to_digits(YYYY, [Y1,Y2,Y3,Y4])
  },
  rfc1123_date(D1, D2, T1, Month, Y1, Y2, Y3, Y4).
rfc1123_date(date(day(DD),T1,year(YYYY)), DD, Month, YYYY) -->
  rfc1123_date(D1, D2, T1, Month, D3, D4, D5, D6),
  {
    digits_to_decimal([D1,D2], DD),
    digits_to_decimal([D3,D4,D5,D6], YYYY)
  }.

rfc1123_date(D1, D2, T1, Month, D3, D4, D5, D6) -->
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  space,
  rfc1123_month(T1, Month),
  space,
  decimal_digit(_, D3),
  decimal_digit(_, D4),
  decimal_digit(_, D5),
  decimal_digit(_, D6).

%! rfc1123_month(-Tree:compound, ?Month:atom)//

rfc1123_month(month(januari), januari) --> "Jan".
rfc1123_month(month(febuari), febuari) --> "Feb".
rfc1123_month(month(march), march) --> "Mar".
rfc1123_month(month(april), april) --> "Apr".
rfc1123_month(month(may), may) --> "May".
rfc1123_month(month(june), june) --> "Jun".
rfc1123_month(month(july), july) --> "Jul".
rfc1123_month(month(august), august) --> "Aug".
rfc1123_month(month(september), september) --> "Sep".
rfc1123_month(month(october), october) --> "Oct".
rfc1123_month(month(november), november) --> "Nov".
rfc1123_month(month(december), december) --> "Dec".

%! rfc1123_datetime(-Tree:compound)//

rfc1123_datetime(T) -->
  rfc1123_datetime(T, _Weekday, _Day, _Month, _Year, _Hour, _Minute, _Second).

%! rfc1123_datetime(
%!   -Tree:compound,
%!   ?Weekday:atom,
%!   ?Day:integer,
%!   ?Month:atom,
%!   ?Year:integer,
%!   ?Hour:integer,
%!   ?Minute:integer,
%!   ?Second:integer
%! )//

rfc1123_datetime(
  rfc1123_datetime(T1,T2,T3,time_zone('GMT')),
  Weekday,
  Day,
  Month,
  Year,
  Hour,
  Minute,
  Second
) -->
  rfc1123_weekday(T1, Weekday),
  comma,
  space,
  rfc1123_date(T2, Day, Month, Year),
  space,
  rfc1123_time(T3, Hour, Minute, Second),
  space,
  "GMT".

rfc1123_datetime_to_gv(Datetime):-
  atom_codes(Datetime, Codes),
  once(phrase(rfc1123_datetime(Tree), Codes)),
  absolute_file_name(project(temp), File, [access(write),file_type(jpeg)]),
  tree_to_gv_file(
    [method(dot),name(Datetime),to_file_type(jpeg)],
    Tree,
    File
  ).

%! rfc1123_time(
%!   -Tree:compound,
%!   ?Hour:integer,
%!   ?Minute:integer,
%!   ?Second:integer
%! )//
% Times between `00:00:00` and `23:59:59`.
%
% @arg Tree A parse tree.
% @arg Time A list of three integers in the `00-23`, `00-59`,
%      and `00-59` range.

rfc1123_time(
  time(hour(Hour),minte(Minute),second(Second)),
  Hour,
  Minute,
  Second
) -->
  {maplist(nonvar, [Hour,Minute,Second])}, !,
  {
    decimal_to_digits(Hour, [D1,D2]),
    decimal_to_digits(Minute, [D3,D4]),
    decimal_to_digits(Second, [D5,D6])
  },
  rfc1123_time(D1, D2, D3, D4, D5, D6).
rfc1123_time(
  time(hour(Hour),minute(Minute),second(Second)),
  Hour,
  Minute,
  Second
) -->
  rfc1123_time(D1, D2, D3, D4, D5, D6),
  {
    digits_to_decimal([D1,D2], Hour),
    digits_to_decimal([D3,D4], Minute),
    digits_to_decimal([D5,D6], Second)
  }.

rfc1123_time(D1, D2, D3, D4, D5, D6) -->
  decimal_digit(_, D1),
  decimal_digit(_, D2),
  colon,
  decimal_digit(_, D3),
  decimal_digit(_, D4),
  colon,
  decimal_digit(_, D5),
  decimal_digit(_, D6).

%! rfc1123_weekday(-Tree:compound, ?Weekday:atom)//

rfc1123_weekday(weekday(monday), monday) --> "Mon".
rfc1123_weekday(weekday(tuesday), tuesday) --> "Tue".
rfc1123_weekday(weekday(wednesday), wednesday) --> "Wed".
rfc1123_weekday(weekday(thursday), thursday) --> "Thu".
rfc1123_weekday(weekday(friday), friday) --> "Fri".
rfc1123_weekday(weekday(saturday), saturday) --> "Sat".
rfc1123_weekday(weekday(sunday), sunday) --> "Sun".



:- begin_tests(rfc1123).

:- use_module(library(apply)).

rfc1123_atom('Sun, 06 Nov 1994 08:49:37 GMT').

test(rfc123_parse, [forall(rfc1123_atom(Datetime))]):-
  atom_codes(Datetime, Codes),
  once(
    phrase(
      rfc1123_datetime(Tree, Weekday, Day, Month, Year, Hour, Minute, Second),
      Codes
    )
  ),
  maplist(formatnl, [Tree, Weekday, Day, Month, Year, Hour, Minute, Second]).

test(rfc1123_parse_gv, [forall(rfc1123_atom(Datetime))]):-
  rfc1123_datetime_to_gv(Datetime).

:- end_tests(rfc1123).
