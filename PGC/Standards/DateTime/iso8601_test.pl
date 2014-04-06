:- module(iso8601_test, []).

/** <module> ISO8601_TEST

Unit tests for the ISO 8061 DCG.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(plunit)).

:- begin_tests(iso8601).

:- use_remote_module(datetime(iso8601_date)).
:- use_remote_module(datetime(iso8601_date_time)).
:- use_remote_module(datetime(iso8601_time_interval)).
:- use_remote_module(datetime(iso8601_time_point)).

:- discontiguous(test/2).
:- discontiguous(iso8601_time_interval_example/5).



% API %

%! iso8604_compound(Year, MonthInYear, DayInMonth, WeekInYear, DayInWeek)

iso8601_compound(1995, 1, 1, 1994-52, 7).
iso8601_compound(1996, 12, 31, 1997-1, 2).

test(
  day_in_week,
  [
    forall(iso8601_compound(Y, M, D, _WeekInYear1, DayInWeek1)),
    true(DayInWeek1 == DayInWeek2)
  ]
):-
  day_in_week(Y, M, D, DayInWeek2).

test(
  week_in_year,
  [
    forall(iso8601_compound(Y, M, D, WeekInYear1, _DayInWeek1)),
    true(WeekInYear1 == WeekInYear2)
  ]
):-
  week_in_year(Y, M, D, WeekInYear2).



% DATE & TIME

iso8601_calendar_date_time_example(A, F, date_time(Date,UTC_Time)):-
  iso8601_calendar_date_example(A1, F, Date),
  iso8601_local_time_example(A2, F, true, UTC_Time),
  atomic_concat(A1, A2, A).

test(
  iso8601_calendar_date_time_generate,
  [
    forall(iso8601_calendar_date_time_example(A1, F, DateTime)),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_calendar_date_time(_T0, F, DateTime), Cs)),
  atom_codes(A2, Cs),
  formatnl(A2).

test(
  iso8601_calendar_date_time_parse,
  [
    forall(iso8601_calendar_date_time_example(A, F, DT1)),
    true(DT1 = DT2)
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_calendar_date_time(_T0, F, DT2), Cs)).



% CALENDAR DATE %

%! iso8601_calendar_date_example(
%!   ?Date:atom,
%!   ?F:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_calendar_date_example('19850412'  , basic,    date(1985,4,_,12)).
iso8601_calendar_date_example('1985-04-12', extended, date(1985,4,_,12)).
% Reduced examples:
iso8601_calendar_date_example('1985-04'   , basic,    date(1985,4,_,_ )).
iso8601_calendar_date_example('1985'      , basic,    date(1985,_,_,_ )).
iso8601_calendar_date_example('19'        , basic,    date(1900,_,_,_ )).

test(
  iso8601_calendar_date_generate,
  [
    forall(iso8601_calendar_date_example(A1, F, Date)),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_calendar_date(_T0, F, Date), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_calendar_date_parse,
  [
    forall(iso8601_calendar_date_example(A, F, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_calendar_date(_T0, F, Date2), Cs)).



% LOCAL TIME %

%! iso8601_local_time_example(
%!   ?Time:atom,
%!   ?F:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?UTC_Time:compound
%! ) is nondet.

iso8601_local_time_example(A, F, T, utc_time(Time,true)):-
  % UTC correction cannot occur with the UTC indicator.
  iso8601_local_time_example_(A1, F, Time, true),
  (
    T = false, A = A1
  ;
    T = true, atomic_concat('T', A1, A)
  ).
iso8601_local_time_example(
  A,
  F,
  T,
  utc_time(Time,UTC_Correction)
):-
  iso8601_local_time_example_(A1, F, Time, false),
  iso8601_utc_correction_example(A2, F, UTC_Correction),
  (
    T = false, atomic_concat(A1, A2, A)
  ;
    T = true, atomic_list_concat(['T',A1,A2], A)
  ).

%! iso8601_local_time_example_(
%!   ?Time:atom,
%!   ?F:oneof([basic,extended]),
%!   ?Time:compound,
%!   ?UTC:boolean
%! ) is nondet.

iso8601_local_time_example_('232050',     basic,    time(23,  20,  50  ), false).
iso8601_local_time_example_('23:20:50',   extended, time(23,  20,  50  ), false).
iso8601_local_time_example_('2320',       basic,    time(23,  20,  _   ), false).
iso8601_local_time_example_('23:20',      extended, time(23,  20,  _   ), false).
iso8601_local_time_example_('23',         basic,    time(23,  _,   _   ), false).
iso8601_local_time_example_('232050,5',   basic,    time(23,  20,  50.5), false).
iso8601_local_time_example_('23:20:50,5', extended, time(23,  20,  50.5), false).
iso8601_local_time_example_('2320,8',     basic,    time(23,  20.8,_   ), false).
iso8601_local_time_example_('23:20,8',    extended, time(23,  20.8,_   ), false).
iso8601_local_time_example_('23,3',       basic,    time(23.3,_,   _   ), false).
iso8601_local_time_example_('000000',     basic,    time(0,   0,   0   ), false).
iso8601_local_time_example_('00:00:00',   extended, time(0,   0,   0   ), false).
iso8601_local_time_example_('240000',     basic,    time(24,  0,   0   ), false).
iso8601_local_time_example_('24:00:00',   extended, time(24,  0,   0   ), false).
iso8601_local_time_example_('232030Z',    basic,    time(23,  20,  30  ), true ).
iso8601_local_time_example_('2320Z',      basic,    time(23,  20,  _   ), true ).
iso8601_local_time_example_('23Z',        basic,    time(23,  _,   _   ), true ).
iso8601_local_time_example_('23:20:30Z',  extended, time(23,  20,  30  ), true ).
iso8601_local_time_example_('23:20Z',     extended, time(23,  20,  _   ), true ).
iso8601_local_time_example_('152746',     basic,    time(15,  27,  46  ), false).
iso8601_local_time_example_('15:27:46',   extended, time(15,  27,  46  ), false).

%! iso8601_utc_correction_example(
%!   ?Correction:atom,
%!   ?F:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?UTC_Correction:compound
%! ) is nondet.

iso8601_utc_correction_example('',       _,        false           ).
iso8601_utc_correction_example('+0100',  basic,    utc(true,  1, 0)).
iso8601_utc_correction_example('+01:00', extended, utc(true,  1, 0)).
iso8601_utc_correction_example('+01',    basic,    utc(true,  1, _)).
iso8601_utc_correction_example('+01',    extended, utc(true,  1, _)).
iso8601_utc_correction_example('-0500',  basic,    utc(false, 5, 0)).
iso8601_utc_correction_example('-05:00', extended, utc(false, 5, 0)).
iso8601_utc_correction_example('-05',    basic,    utc(false, 5, _)).
iso8601_utc_correction_example('-05',    extended, utc(false, 5, _)).

test(
  iso8601_local_time_generate,
  [
    forall(iso8601_local_time_example(A1, F, T, UTC_Time)),
    true(A1 = A2)
  ]
):-
  once(phrase(iso8601_local_time(_T0, F, T, UTC_Time), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_local_time_parse,
  [
    forall(iso8601_local_time_example(A, F, T1, UTC_Time1)),
    true(maplist(=, [T1,UTC_Time1], [T2,UTC_Time2]))
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_local_time(_T0, F, T2, UTC_Time2), Cs)).



% ORDINAL DATE %

%! iso8601_ordinal_date_example(
%!   ?Date:atom,
%!   ?F:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_ordinal_date_example('1985102',  basic,    date(1985,_,_,102)).
iso8601_ordinal_date_example('1985-102', extended, date(1985,_,_,102)).

test(
  iso8601_ordinal_date_generate,
  [
    forall(iso8601_ordinal_date_example(A1, F, Date)),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_ordinal_date(_T0, F, Date), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_ordinal_date_parse,
  [
    forall(iso8601_ordinal_date_example(A, F, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_ordinal_date(_T0, F, Date2), Cs)).



% TIME INTERVALS %

%! iso8601_time_interval_example(
%!   ?TimeInterval:atom,
%!   ?Variant:between(1,4),
%!   ?F:oneof([basic,extended]),
%!   ?DT1:compound,
%!   ?DT2:compound
%! ) is nondet.

iso8601_time_interval_example(
  '19850412T232050/19850625T103000',
  1,
  basic,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
).
iso8601_time_interval_example(
  '1985-04-12T23:20:50/1985-06-25T10:30:00',
  1,
  extended,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
).
iso8601_time_interval_example(
  'P2Y10M15DT10H30M20S',
  2,
  basic,
  date_time(date(2,10,_,15),utc_time(time(10,30,20),false)),
  _
).
iso8601_time_interval_example(
  'P6W',
  2,
  basic,
  date_time(date(_,_,6,_),utc_time(time(_,_,_),false)),
  _
).
% Alternative representation
%iso8601_time_interval_example(
%  'P00021015T103020',
%  2,
%  basic,
%  date_time(date(2,10,_,15),utc_time(time(10,30,20),false)),
%  _
%).
% Alternative representation
%iso8601_time_interval_example(
%  'P0002-10-15T10:30:20',
%  2,
%  extended,
%  date_time(date(2,10,_,15),utc_time(time(10,30,20),false)),
%  _
%).
% Alternative representation
%iso8601_time_interval_example(
%  'P0002155T103020',
%  2,
%  basic,
%  date_time(date(2,_,_,155),utc_time(time(10,30,20),false)),
%  _
%).
% Alternative representation
%iso8601_time_interval_example(
%  'P0002-155T10:30:20',
%  2,
%  extended,
%  date_time(date(2,_,_,155),utc_time(time(10,30,20),false)),
%  _
%).
iso8601_time_interval_example(
  '19850412T232050/P1Y2M15DT12H30M0S',
  3,
  basic,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
).
% Alternative format.
%iso8601_time_interval_example(
%  '19850412T232050/P00010215T123000',
%  3,
%  basic,
%  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
%  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
%).
iso8601_time_interval_example(
  '1985-04-12T23:20:50/P1Y2M15DT12H30M0S',
  3,
  extended,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
).
% Alternative format.
%iso8601_time_interval_example(
%  '1985-04-12T23:20:50/P0001-02-15T12:30:00',
%  3,
%  extended,
%  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
%  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
%).
iso8601_time_interval_example(
  'P1Y2M15DT12H30M0S/19850412T232050',
  4,
  basic,
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
).
% Alternative representation
%iso8601_time_interval_example(
%  'P00010215T123000/19850412T232050',
%  4,
%  basic,
%  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
%  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
%).
iso8601_time_interval_example(
  'P1Y2M15DT12H30M0S/1985-04-12T23:20:50',
  4,
  extended,
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
).
% Alternative representation
%iso8601_time_interval_example(
%  'P0001-02-15T12:30:00/1985-04-12T23:20:50',
%  4,
%  extended,
%  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
%  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
%).

test(
  iso8601_time_interval_generate,
  [
    forall(iso8601_time_interval_example(A1, Variant, F, DT1, DT2)),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_time_interval(_T0, Variant, F, DT1, DT2), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_time_interval_parse,
  [
    forall(iso8601_time_interval_example(A, Variant, F, DT1, DT2)),
    true(maplist(=, [DT1, DT2], [DT3, DT4]))
  ]
):-
  atom_codes(A, Cs),
  once(
    phrase(iso8601_time_interval(_T0, Variant, F, DT3, DT4), Cs)
  ).

iso8601_recurring_time_interval_example(
  'R12/19850412T232050/19850625T103000',
  1,
  basic,
  12,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
).
iso8601_recurring_time_interval_example(
  'R12/P2Y10M15DT10H30M20S',
  2,
  _Format,
  12,
  date_time(date(2,10,_,15),utc_time(time(10,30,20),false)),
  _
).
iso8601_recurring_time_interval_example(
  'R12/19850412T232050/P1Y2M15DT12H30M0S',
  3,
  basic,
  12,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
).
iso8601_recurring_time_interval_example(
  'R12/P1Y2M15DT12H30M0S/19850412T232050',
  4,
  basic,
  12,
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
).
% The seconds are not generated correctly (=0= i.o. =00=).
%iso8601_recurring_time_interval_example(
%  'R12/l985-04-12T23:20:50/1985-06-25T10:30:00',
%  1,
%  extended,
%  12,
%  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
%  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
%).
iso8601_recurring_time_interval_example(
  'R12/1985-04-12T23:20:50/P1Y2M15DT12H30M0S',
  3,
  extended,
  12,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false))
).
iso8601_recurring_time_interval_example(
  'R12/P1Y2M15DT12H30M0S/1985-04-12T23:20:50',
  4,
  extended,
  12,
  date_time(date(1,   2,_,15),utc_time(time(12,30,0 ),false)),
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false))
).

test(
  iso8601_recurring_time_interval_generate,
  [
    forall(
      iso8601_recurring_time_interval_example(A1, V, F, N, DT1, DT2)
    ),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_recurring_time_interval(_T0, V, F, N, DT1, DT2), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_recurring_time_interval_parse,
  [
    forall(
      iso8601_recurring_time_interval_example(A, V1, F, N1, DT1, DT2)
    ),
    true(maplist(=, [V1,N1,DT1,DT2], [V2,N2,DT3,DT4]))
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_recurring_time_interval(_T0, V2, F, N2, DT3, DT4), Cs)).



% WEEK DATE %

%! iso8601_week_date_example(
%!   ?Date:atom,
%!   ?F:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_week_date_example('1985W155',   basic,    date(1985,_,15,5)).
iso8601_week_date_example('1985-W15-5', extended, date(1985,_,15,5)).
% Reduced accuracy:
iso8601_week_date_example('1985W15',    basic,    date(1985,_,15,_)).
iso8601_week_date_example('1985-W15',   extended, date(1985,_,15,_)).

test(
  iso8601_week_date_generate,
  [
    forall(iso8601_week_date_example(A1, F, Date)),
    true(A1 == A2)
  ]
):-
  once(phrase(iso8601_week_date(_Tree, F, Date), Cs)),
  atom_codes(A2, Cs).

test(
  iso8601_week_date_parse,
  [
    forall(iso8601_week_date_example(A, F, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(A, Cs),
  once(phrase(iso8601_week_date(_Tree, F, Date2), Cs)).

:- end_tests(iso8601).

