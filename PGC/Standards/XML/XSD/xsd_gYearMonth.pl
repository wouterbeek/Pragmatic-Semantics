:- module(
  xsd_gYearMonth,
  [
    xsd_gYearMonth_canonical_map//1, % +GregorianYearMonth:compound
    xsd_gYearMonth_lexical_map//1 % -GregorianYearMonth:compound
  ]
).

/** <module> XSD gYearMonth datatype

=*gYearMonth*= represents specific whole Gregorian months
in specific Gregorian years.

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gYearMonth uses the date/timeSevenPropertyModel, with day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gYearMonth are "projections" of those
of dateTime.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_gYearMonth_canonical_map(+GregorianYearMonth:compound)// is det.
% Maps a gYearMonth value to a xsd_gYearMonth_lexical_map//.
%
% @arg GregorianYearMonth A complete gYearMonth value.

xsd_gYearMonth_canonical_map(dateTime(Y,M,_,_,_,_,TZ)) -->
  yearCanonicalFragmentMap(Y),
  `-`,
  monthCanonicalFragmentMap(M),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_gYearMonth_lexical_map(-GregorianYearMonth:compound)// is det.
% Maps a xsd_gYearMonth_lexical_map// to a gYearMonth value.
%
% ~~~{.ebnf}
% xsd_gYearMonth_lexical_map ::= yearFrag '-' monthFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])
% (Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_gYearMonth_lexical_map(GYM) -->
  yearFrag(Y),
  `-`,
  monthFrag(M),
  (`` ; timezoneFrag(TZ)), !,
  {newDateTime(Y, M, _, _, _, _, TZ, GYM)}.

