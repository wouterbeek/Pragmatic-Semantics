:- module(
  xsd_gYear,
  [
    xsd_gYear_canonical_map//1, % +GregorianYear:compound
    xsd_gYear_lexical_map//1 % -GregorianYear:compound
  ]
).

/** <module> XSD Gregorian year datatype

*=gYear=* represents Gregorian calendar years.

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gYear uses the date/timeSevenPropertyModel, with month, day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gYear are "projections" of those
of dateTime.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_gYear_canonical_map(+GregorianYear:compound)// is det.
% Maps a gYear value to a xsd_gYear_lexical_map//.
%
% @arg GregorianYear A complete gYear value.

xsd_gYear_canonical_map(dateTime(Y,_,_,_,_,_,TZ)) --> !,
  yearCanonicalFragmentMap(Y),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_gYear_lexical_map(-GregorianYear:compound)//
% Maps a xsd_gYear_lexical_map// to a gYear value.
%
% ~~~{.ebnf}
% xsd_gYear_lexical_map ::= yearFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% -?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_gYear_lexical_map(GY) -->
  yearFrag(Y),
  (`` ; timezoneFrag(TZ)), !,
  {newDateTime(Y, _M, _D, _H, _MM, _S, TZ, GY)}.

