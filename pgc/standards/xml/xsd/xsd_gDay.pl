:- module(
  xsd_gDay,
  [
    xsd_gDay_canonical_map//1, % +GregorianDay:compound
    xsd_gDay_lexical_map//1 % -GregorianDay:compound
  ]
).

/** <module> XSD Gregorian day datatype

*=gDay=* represents whole days within an arbitrary month -- days that recur at
the same point in each (Gregorian) month. This datatype is used to represent
a specific day of the month. To indicate, for example, that an employee gets
a paycheck on the 15th of each month. Days beyond 28 cannot occur
in all months; they are nonetheless permitted, up to 31.

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gDay uses the date/timeSevenPropertyModel, with year, month, hour, minute,
and second required to be absent. timezoneOffset remains optional and day
must be between 1 and 31 inclusive.

Since gDay values (days) are ordered by their first moments, it is possible
for apparent anomalies to appear in the order when timezoneOffset values
differ by at least 24 hours. (It is possible for timezoneOffset values to
differ by up to 28 hours.)

Examples that may appear anomalous:
  * =|---15 < ---16|= , but =|---15−13:00 > ---16+13:00|=
  * =|---15−11:00 = ---16+13:00|=
  * =|---15−13:00 <> ---16|= , because  =|---15−13:00 > ---16+14:00|=
    and =|---15−13:00 < 16−14:00|=

Time zone offsets do not cause wrap-around at the end of the month:
the last day of a given month with a time zone offset of =|−13:00|=
may start after the first day of the next month with offset =|+13:00|=,
as measured on the global timeline, but nonetheless
=|---01+13:00 < ---31−13:00|=.

### Lexical Mapping

The lexical representations for gDay are "projections" of those of dateTime.

--

@author Wouter Beek
@version 2013/08, 2014/03-2014/04
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(xsd(xsd_dateTime_generic)).
:- use_remote_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_gDay_canonical_map(+GregorianDay:compound)// is det.
% Maps a gDay value to a xsd_gDay_canonical_map//.
%
% @arg GregorianDay A complete gDay value.

xsd_gDay_canonical_map(dateTime(_Y,_M,D,_H,_MM,_S,TZ)) -->
  `---`,
  dayCanonicalFragmentMap(D),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_gDay_lexical_map(-GregorianDay:compound)// is det.
%
% ~~~{.ebnf}
% gDayLexicalRep ::= '---' dayFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% ---(0[1-9]|[12][0-9]|3[01])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_gDay_lexical_map(DT) -->
  `---`,
  dayFrag(D),
  (`` ; timezoneFrag(TZ)), !,
  {newDateTime(_Y, _M, D, _H, _MM, _S, TZ, DT)}.

