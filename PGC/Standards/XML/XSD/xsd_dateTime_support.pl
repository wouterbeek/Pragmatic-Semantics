:- module(
  xsd_dateTime_support,
  [
    dayInMonth/2, % +Month:between(1,12)
                  % +DayInMonth:between(1,31)
    dayInMonth/3, % +Year:integer
                  % +Month:between(1,12)
                  % +DayInMonth:between(1,31)
    daysInMonth/3, % ?Year:integer
                   % ?Month:between(1,12)
                   % ?DaysInMonth:between(28,31)
    newDateTime/8, % ?Year:integer
                   % ?Month:between(1,12)
                   % ?Day:between(1,31)
                   % ?Hour:between(0,24)
                   % ?Minute:between(0,59)
                   % ?Second:between(0.0,60.0)
                   % ?Timezone:between(-840,840)
                   % -DateTime:compound
    normalizeDay/6, % +Year:integer
                    % +Month:integer
                    % +Day:integer
                    % -NormalizedYear:integer
                    % -NormalizedMonth:integer
                    % -NormalizedDay:integer
    normalizeMinute/10, % +Year:integer
                        % +Month:integer
                        % +Day:integer
                        % +Hour:integer
                        % +Minute:integer
                        % -NormalizedYear:itneger
                        % -NormalizedMonth:integer
                        % -NormalizedDay:integer
                        % -NormalizedHour:integer
                        % -NormalizedMinute:integer
    normalizeMonth/4, % +Year:integer
                      % +Month:integer
                      % -NormalizedYear:integer
                      % -NormalizedMonth:integer
    normalizeSecond/12, % +Year:integer
                        % +Month:integer
                        % +Day:integer
                        % +Hour:integer
                        % +Minute:integer
                        % +Second:float
                        % -NormalizedYear:integer
                        % -NormalizedMonth:integer
                        % -NormalizedDay:integer
                        % -NormalizedHour:integer
                        % -NormalizedMinute:integer
                        % -NormalizedSecond:float
    timeOnTimeline/2 % +DateTime:compound
                     % -Seconds:float
  ]
).

/** <module> XSD dateTime support

Support predicates that are used to implementation the XSD datatypes
that represent date, time, and duration in a standards-compliant way.

@author Wouter Beek
@version 2013/08-2013/11, 2014/03-2014/04
*/

:- use_module(generics(meta_ext)).
:- use_module(xsd(xsd_number_generic)).



%! dayInMonth(+Month:between(1,12), +DayInMonth:between(28,31)) is semidet.
% Succeeds if the given day can occur in the given month.
%
% The day value must be no more than 30 if month is one of 4, 6, 9, or 11,
% and no more than 29 if month is 2.

dayInMonth(2, D):- !,
  between(1, 29, D).
dayInMonth(M, D):-
  memberchk(M, [4,6,9,11]), !,
  between(1, 30, D).
dayInMonth(_M, D):-
  between(1, 31, D).

dayInMonth(Y, M, D):-
  var(Y), !,
  dayInMonth(M, D).
dayInMonth(Y, M, D):-
  daysInMonth(Y, M, MaxD),
  between(1, MaxD, D).


%! daysInMonth(
%!   ?Year:integer,
%!   +Month:between(1,12),
%!   -DaysInMonth:between(28,31)
%! ) is det.
%! daysInMonth(
%!   ?Year:integer,
%!   +Month:between(1,12),
%!   +DaysInMonth:between(28,31)
%! ) is semidet.
% Returns the number of the last day of the month for any combination
% of year and month.
%
% @arg Year An optional integer.
% @arg Month An integer between 1 and 12.
% @arg DaysInMonth An integer between 28 and 31 inclusive.

% When m is 2 and y is not evenly divisible by 4,
% or is evenly divisible by 100 but not by 400, or is absent.
daysInMonth(Y, 2, 28):-
  var(Y), !.
daysInMonth(Y, 2, 28):-
  Y rem 4 =\= 0, !.
daysInMonth(Y, 2, 28):-
  Y rem 100 =:= 0,
  Y rem 400 =\= 0, !.
% When m is 2 and y is evenly divisible by 400,
% or is evenly divisible by 4 but not by 100,
daysInMonth(Y, 2, 29):-
  Y rem 400 =:= 0, !.
daysInMonth(Y, 2, 29):-
  Y rem 4 =:= 0,
  Y rem 100 =\= 0, !.
% When m is 4, 6, 9, or 11.
daysInMonth(_Y, M, 30):-
  memberchk(M, [4,6,9,11]), !.
% Otherwise, i.e. m is 1, 3, 5, 7, 8, 10, or 12.
daysInMonth(_Y, M, 31):-
  memberchk(M, [1,3,5,7,8,10,12]), !.


%! newDateTime(
%!   ?Year:integer,
%!   ?Month:between(1,12),
%!   ?Day:between(1,31),
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59),
%!   ?Second:between(0.0,60.0),
%!   ?Timezone:between(-840,840),
%!   -DateTime:compound
%! ) is det.
% Returns an instance of the date/timeSevenPropertyModel with property values
% as specified in the arguments. If an argument is omitted, the
% corresponding property is set to absent.
%
% @arg Year An optional integer.
% @arg Month An optional integer between 1 and 12 inclusive.
% @arg Day An optional integer between 1 and 31 inclusive.
% @arg Hour An optional integer between 0 and 24 inclusive.
% @arg Minute An optional integer between 0 and 59 inclusive.
% @arg Second An optional decimal number greater than or equal to
%        0 and less than 60.
% @arg Timezone An optional integer between −840 and 840 inclusive.
%
% ~~~
% dt be an instance of the date/timeSevenPropertyModel
% yr be Yr when Yr is not absent, otherwise 1
% mo be Mo when Mo is not absent, otherwise 1
% da be Da when Da is not absent, otherwise 1
% hr be Hr when Hr is not absent, otherwise 0
% mi be Mi when Mi is not absent, otherwise 0
% se be Se when Se is not absent, otherwise 0
%
% normalizeSecond(yr, mo, da, hr, mi, se)
% Set the year property of dt to absent when Yr is absent, otherwise yr.
% Set the month property of dt to absent when Mo is absent, otherwise mo.
% Set the day property of dt to absent when Da is absent, otherwise da.
% Set the hour property of dt to absent when Hr is absent, otherwise hr.
% Set the minute property of dt to absent when Mi is absent, otherwise mi.
% Set the second property of dt to absent when Se is absent, otherwise se.
% Set the timezoneOffset property of dt to Tz
% Return dt.
% ~~~

newDateTime(Y1, M1, D1, H1, MM1, S1, TZ, DT):-
  % Set the values that are used for performing the nprmalization.
  default(Y1,  1,   Y2 ),
  default(M1,  1,   M2 ),
  default(D1,  1,   D2 ),
  default(H1,  0,   H2 ),
  default(MM1, 0,   MM2),
  default(S1,  0.0, S2 ),
  normalizeSecond(Y2, M2, D2, H2, MM2, S2, Y3, M3, D3, H3, MM3, S3),

  % Variables stay variable.
  % Non-variables get the normalized value.
  var_or_value(Y1,  Y3,  Y4 ),
  var_or_value(M1,  M3,  M4 ),
  var_or_value(D1,  D3,  D4 ),
  var_or_value(H1,  H3,  H4 ),
  var_or_value(MM1, MM3, MM4),
  var_or_value(S1,  S3,  S4 ),
  DT = dateTime(Y4,M4,D4,H4,MM4,S4,TZ).


%! normalizeDay(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   -NormalizedYear:integer,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer
%! ) is det.
% If month is out of range, or day is out of range for the appropriate month,
% then adjust values accordingly, otherwise make no change.
%
% ~~~
% normalizeMonth(yr, mo)
% Repeat until da is positive and not greater than daysInMonth(yr, mo):
%   If da exceeds daysInMonth(yr, mo) then:
%     Subtract that limit from da.
%     Add 1 to mo.
%     normalizeMonth(yr, mo)
%   If da is not positive then:
%     Subtract 1 from mo.
%     normalizeMonth(yr, mo)
%     Add the new upper limit from the table to da.
% ~~~

normalizeDay(Y1, M1, D1, Y2, M2, D2):-
  normalizeMonth(Y1, M1, Y3, M3),
  normalizeDay_(Y3, M3, D1, Y2, M2, D2).

normalizeDay_(Y1, M1, D1, Y2, M2, D2):-
  daysInMonth(Y1, M1, D1Max),
  (
    D1 > D1Max
  ->
    D3 is D1 - D1Max,
    MX is M1 + 1,
    normalizeMonth(Y1, MX, Y3, M3),
    normalizeDay_(Y3, M3, D3, Y2, M2, D2)
  ;
    D1 < 0
  ->
    MX is M1 - 1,
    normalizeMonth(Y1, MX, Y3, M3),
    daysInMonth(Y3, M3, D3Max),
    D3 is D1 + D3Max,
    normalizeDay_(Y3, M3, D3, Y2, M2, D2)
  ;
    Y2 = Y1,
    M2 = M1,
    D2 = D1
  ).


%! normalizeMinute(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   +Hour:integer,
%!   +Minute:integer,
%!   -NormalizedYear:itneger,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer,
%!   -NormalizedHour:integer,
%!   -NormalizedMinute:integer
%! ) is det.
% Normalizes minute, hour, month, and year values to values that obey
% the appropriate constraints.
%
% ~~~
% Add mi div 60 to hr.
% Set mi to mi mod 60.
% Add hr div 24 to da.
% Set hr to hr mod 24.
% normalizeDay(yr, mo, da).
% ~~~

normalizeMinute(Y1, M1, D1, H1, MM1, Y2, M2, D2, H2, MM2):-
  xsd_number_generic:(HX  is H1 + MM1 xsd_div 60),
  xsd_number_generic:(MM2 is      MM1 xsd_mod 60),
  xsd_number_generic:(DX  is D1 + HX  xsd_div 24),
  xsd_number_generic:(H2  is      HX  xsd_mod 24),
  normalizeDay(Y1, M1, DX, Y2, M2, D2).


%! normalizeMonth(
%!   +Year:integer,
%!   +Month:integer,
%!   -NormalizedYear:integer,
%!   -NormalizedMonth:integer
%! ) is det.
% If month =M1= is out of range, adjust month and year =Y1= accordingly;
% otherwise, make no change.
%
% ~~~
% Add (mo − 1) div 12  to yr.
% Set mo to (mo − 1) mod 12 + 1.
% ~~~
%
% @arg Year An integer.
% @arg Month An integer.
% @arg NormalizedYear An integer.
% @arg NormalizedMonth An integer.

normalizeMonth(Y1, M1, Y2, M2):-
  % Add (mo − 1) div 12 to yr.
  xsd_number_generic:(Y2 is Y1 + (M1 - 1) xsd_div 12),
  % Set mo to (mo − 1) mod 12 + 1.
  xsd_number_generic:(M2 is (M1 - 1) xsd_mod 12 + 1).


%! normalizeSecond(
%!   +Year:integer,
%!   +Month:integer,
%!   +Day:integer,
%!   +Hour:integer,
%!   +Minute:integer,
%!   +Second:float,
%!   -NormalizedYear:itneger,
%!   -NormalizedMonth:integer,
%!   -NormalizedDay:integer,
%!   -NormalizedHour:integer,
%!   -NormalizedMinute:integer,
%!   -NormalizedSecond:float
%! ) is det.
% Normalizes second, minute, hour, month, and year values to values that
% obey the appropriate constraints (ignoring leap seconds).
%
% ~~~
% Add se div 60 to mi.
% Set se to se mod 60 .
% normalizeMinute(yr, mo, da, hr, mi).
% ~~~

normalizeSecond(Y1, M1, D1, H1, MM1, S1, Y2, M2, D2, H2, MM2, S2):-
  % Notice that div/2 would not work since seconds is a float.
  MM0 is MM1 + floor(S1 / 60),
  % Notice that mod/2 would not work since seconds is a float.
  S2 is S1 - 60 * floor(S1 / 60),
  normalizeMinute(Y1, M1, D1, H1, MM0, Y2, M2, D2, H2, MM2).


%! timeOnTimeline(+DateTime:compound, -Seconds:float) is det.
% Maps a date/timeSevenPropertyModel value to the decimal number representing
% its position on the "time line".
%
% @arg DateTime A date/timeSevenPropertyModel value.
% @arg Seconds A decimal number.

timeOnTimeline(dateTime(Y1,M1,D1,H1,MM1,S1,UTC), ToTl):-
  % yr be 1971 when dt's year is absent, and dt's year − 1 otherwise.
  (  var(Y1)
  -> Y2 = 1971
  ;  Y2 is Y1 - 1),

  % mo be 12 or dt's month, similarly.
  default(M1, 12, M2),

  % da be daysInMonth(yr+1, mo) − 1  or (dt's day) − 1, similarly.
  Y3 is Y2 + 1,
  (  var(D1)
  -> daysInMonth(Y3, M2, D2_),
     D2 is D2_ - 1
  ;  D2 is D1 - 1),

  % hr be 0 or dt's hour, similarly.
  default(H1, 0, H2),

  % mi be 0 or dt's minute, similarly.
  default(MM1, 0, MM2),

  % se be 0 or dt's second, similarly.
  default(S1, 0.0, S2),

  % Subtract timezoneOffset from mi when timezoneOffset is not absent.
  (  var(UTC)
  -> MM3 = MM2
  ;  MM3 is MM2 - UTC),

  % Add 86400 × Summ < mo ·daysInMonth·(yr + 1, m) to ToTl.
  aggregate_all(
    sum(D3_),
    (
      between(1,M2,M3),
      daysInMonth(Y3, M3, D3_)
    ),
    D3
  ),
  xsd_number_generic:(ToTl is
      % Year.
      31536000 * Y2
      % Leap-year days.
      % Add 86400 × (yr div 400 − yr div 100 + yr div 4) to ToTl.
      + 86400 * (Y2 xsd_div 400 - Y2 xsd_div 100 + Y2 xsd_div 4)
      % Month.
      + 86400 * D3
      % Day
      % Add 86400 × da to ToTl.
      + 86400 * D2
      % Hour.
      + 3600 * H2
      % Minute.
      + 60 * MM3
      % Second.
      + S2).


%! var_or_value(+Argument, +Value, -VariableOrValue) is det.

var_or_value(Arg, _Val, _Var):-
  var(Arg), !.
var_or_value(_Arg, Val, Val).

