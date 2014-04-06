:- module(
  xsd_gMonth,
  [
    xsd_gMonth_canonical_map//1, % +GregorianMonth:compound
    xsd_gMonth_lexical_map//1 % -GregorianMonth:compound
  ]
).

/** <module> XSD Gregorian month datatype

*=gMonth=* represents whole (Gregorian) months within an arbitrary year—months
that recur at the same point in each year. It might be used, for example,
to say what month annual Thanksgiving celebrations fall in different
countries (=|--11|= in the United States, =|--10|= in Canada, and possibly
other months in other countries).

Because month/year combinations in one calendar only rarely correspond to
month/year combinations in other calendars, values of this type are not,
in general, convertible to simple values corresponding to month/year
combinations in other calendars. This type should therefore be used with
caution in contexts where conversion to other calendars is desired.

### Value Space

gMonth uses the date/timeSevenPropertyModel, with year, day, hour, minute,
and second required to be absent. timezoneOffset remains optional.

### Lexical Mapping

The lexical representations for gMonth are "projections" of those of dateTime.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).



% CANONICAL MAP %

%! xsd_gMonth_canonical_map(+GregorianMonth:compound)// is det.
% Maps a gMonth value to a xsd_gMonth_lexical_map//.
%
% @arg GregorianMonth A dateTime compound term with only month
%      and (optionally) timezone instantiated.

xsd_gMonth_canonical_map(dateTime(_,M,_,_,_,_,TZ)) -->
  `--`,
  monthCanonicalFragmentMap(M),
  ({var(TZ)} ; timezoneCanonicalFragmentMap(TZ)), !.



% LEXICAL MAP %

%! xsd_gMonth_lexical_map(-GregorianMonth:compound)//
% Maps a xsd_gMonth_lexical_map// to a gMonth value.
%
% ~~~{.ebnf}
% xsd_gMonth_lexical_map ::= '--' monthFrag timezoneFrag?
% ~~~
%
% ~~~{.re}
% --(0[1-9]|1[0-2])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
% ~~~

xsd_gMonth_lexical_map(DT) -->
  `--`,
  monthFrag(M),
  (`` ; timezoneFrag(TZ)), !,
  {newDateTime(_Y, M, _D, _H, _MM, _S, TZ, DT)}.

