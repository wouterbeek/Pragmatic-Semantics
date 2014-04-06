:- module(
  xsd_yearMonthDuration,
  [
    xsd_yearMonthDuration_canonical_map//1, % +YearMonthDuration:compound
    xsd_yearMonthDuration_lexical_map//1 % -YearMonthDuration:compound
  ]
).

/** <module> XSD year-month duration datatype

*=yearMonthDuration=* is a datatype that is derived from =duration=
by restricting its lexical representations to instances of
xsd_yearMonthDuration_lexical_map//.

### Value spae

The value space of =yearMonthDuration= is that of =duration= restricted to
those whose seconds property is =0=.

#### Order

This results in a duration datatype which is totally ordered.

#### Implementation

The always-zero seconds is formally retained in order that
=yearMonthDuration='s (abstract) value space truly be a subset of that
of duration An obvious implementation optimization is to ignore the zero
and implement =yearMonthDuration= values simply as integer values.

### Lexical mapping

The lexical space is reduced from that of =duration= by disallowing
duDayFrag// and duTimeFrag// fragments in the lexical representations.

#### RE

~~~
-?P((([0-9]+Y)([0-9]+M)?)|([0-9]+M))
~~~
or
~~~
-?P[0-9]+(Y([0-9]+M)?|M)
~~~
but the formal definition of yearMonthDuration uses a simpler RE in its
pattern facet: =|[^DT]*|=. This pattern matches only strings of characters
which contain no =D= and no =T=, thus restricting the lexical space of
duration to strings with no day, hour, minute, or seconds fields.

### Canonical mapping

The canonical mapping is that of duration restricted in its range to
the lexical space.

The yearMonthDuration value whose months and seconds are both zero has
no canonical representation in this datatype since its canonical
representation in duration (=PT0S=) is not in the lexical space of
yearMonthDuration.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(xsd(xsd_duration)).
:- use_module(xsd(xsd_duration_generic)).



% CANONICAL MAP %

%! xsd_yearMonthDuration_canonical_map(+YearMonthDuration:number)// is det.

xsd_yearMonthDuration_canonical_map(duration(M,0)) -->
  xsd_duration_canonical_map(duration(M,0)).



% LEXICAL MAP %

%! xsd_yearMonthDuration_lexical_map(-Duration:compound)// is det.
% ~~~{.ebnf}
% xsd_yearMonthDuration_lexical_map ::= '-'? 'P' duYearMonthFrag
% ~~~

xsd_yearMonthDuration_lexical_map(duration(M2,0)) -->
  (`-`, {Sign = -1} ; {Sign = 1}),
  `P`,
  duYearMonthFrag(M1),
  {M2 is copysign(M1, Sign)}.

