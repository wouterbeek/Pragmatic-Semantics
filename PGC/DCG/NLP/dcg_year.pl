:- module(
  dcg_year,
  [
    year//2, % ?Lang:atom
             % ?Year:oneof([integer,pair(integer)])
    year_interval//2, % ?Lang:atom
                      % ?YearInterval:pair(integer))
    year_point//2 % ?Lang:atom
                  % ?YearPoint:integer
  ]
).

/** <module> DCG_YEAR

DCGs for parsing/generating year information.

# Preposition intervals

Intervals indicated by prepositions are currently not handled at all.

Some examples include:
  * "before 1907"
  * "after the 17th century"
  * "in the midst of 1780"

The problem is that open intervals like [1] do not normally
have the same meaning as [2] but have a meaning that is
(i) context dependent and (ii) fuzzy. I will treat these separately.

~~~{.html}
[1] before 1808
~~~

~~~{.html}
[2] from the Big Bang until 1808
~~~

## Context dependency of preposition intervals

The meaning of preposition intervals is (at least in some cases)
context dependent.

For instance, the interval that occurs in the meaning of [3]
is allowed to be bigger than the interval that occurs in the meaning of [4],
where the latter is at least bounded by Einstein's birth year.

~~~{.html}
[3] The first dinosaurs walked the earth before 300 million years B.C.
~~~

~~~{.html}
[4] Einstein came up with the idea of general relativity before 1937
~~~

## Fuzzyness of preposition intervals

The meaning of preposition intervals is (at least in some cases) fuzzy.

For instance, the book mentioned in [5] is (based on the meaning of [5]
solely) more likely to be published in 1924 than in 1901, even though both
are physically possible (given the birth and death years of James Joyce).

~~~{.html}
[5] James Joyce's Ulyssus was published before 1925.
~~~

@author Wouter Beek
@tbd Intervals indicated by prepositions are not handled at all
     (the preposition is simply skipped). See the note above.
@version 2013/05-2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(nlp(dcg_century)).
:- use_module(nlp(dcg_dict)).



%! year(?Languag:atom, Year:oneof([integer,pair(integer)]))// is nondet.

% 1. Years sometime occur between round brackets.
year(Lang, Year) -->
  opening_bracket,
  year(Lang, Year),
  closing_bracket.
% 2. Two years, indicating a start year and an end year.
year(Lang, Interval) -->
  year_interval(Lang, Interval).
% 3. A single year.
year(Lang, Year) -->
  year_point(Lang, Year).
% 4. An interval indicated by a preposition.
%    The meaning of the preposition cannot be taken into account.
%year(Lang, Year) -->
%  pre(Lang), blank,
%  year(Lang, Year).

%! year_interval(?Lang:atom, ?Interval:pair(integer))//
% A year interval, i.e. an interval delimited by a first and a last year.
%
% Note that the meaning of uncertainty notation can be context-dependent.
% For example =|1917-19??|= means =|1917-1999|=,
% whereas =|19??-1917|= means =|1900-1917|=.

% A year interval designated by a start year and an end year.
year_interval(Lang, Year1-Year2) -->
  year_point(Lang, Year1),
  year_separator,
  year_point(Lang, Year2).
% A year interval designated by a semi-year expression indicating
% uncertainty and an end year.
% The lowest year in the year interval designated by the
% semi-year expression is taken as the begin year.
year_interval(Lang, Year11-Year2) -->
  year_uncertainty(Year11-_Year12),
  year_separator,
  year_point(Lang, Year2).
% A year interval designated by a start year and a semi-year expression
% indicating uncertainty.
% The highest year in the year interval designated by the
% semi-year expression is taken as the end year.
year_interval(Lang, Year1-Year22) -->
  year_point(Lang, Year1),
  year_separator,
  year_uncertainty(_Year21-Year22).
% Module DCG_CENTURY parses expressions denoting centuries
% and centuries denote a year interval with a duration of 100 years.
year_interval(Lang, Interval) -->
  century_interval(Lang, Interval).
% A year interval designated by a single semi-year expression indicating
% uncertainty.
% Example =199?= designates the interval 1991-1999.
year_interval(_Lang, Interval) -->
  year_uncertainty(Interval).
% Example =|tussen 1608 en 1618|= denotes the same year interval
% as =|1608-1618|=.
year_interval(Lang, Year1-Year2) -->
  year_interval_preposition(Lang), blank,
  year_point(Lang, Year1), blanks,
  conj(Lang), blank,
  year_point(Lang, Year2).
% Example =|tussen 1530/1545|= denotes the same year interval
% as =|1530-1545|=.
year_interval(Lang, Interval) -->
  year_interval_preposition(Lang), blanks,
  year_interval(Lang, Interval).
% This is a controversial one.
% If an interval is designated by an expression of the form =|Y1 OR Y2|=
% and Y1 and Y2 are not direct successors, then we take the interval from
% =Y1= until =Y2=.
%year_interval(Lang, Year1-Year2) -->
%  year_point(Lang, Year1),
%  blank, disj(Lang), blank,
%  year_point(Lang, Year2).

year_interval_preposition(en) --> "between".
year_interval_preposition(nl) --> "tussen".

year_point(_Lang, Year) -->
  integer(Year).
% Uncertainty w.r.t. the end of an interval.
% Open intervals are interpreted as a single year.
%year_point(Lang, Year) -->
%  integer(Year),
%  year_separator,
%  uncertainty(Lang).
% Uncertainty w.r.t. the end of an interval.
% Open intervals are interpreted as a single year.
%year_point(Lang, Year) -->
%  uncertainty(Lang),
%  blanks,
%  year_point(Lang, Year).
year_point(_Lang, Year) -->
  integer(Year),
  year_separator.

year_separator --> blank, year_separator, blank.
year_separator --> equals_sign.
year_separator --> forward_slash.
year_separator --> hyphen_minus.

% Uncertainty widening, e.g. 19?? means 1900-1999.
year_uncertainty(Year1-Year2) -->
  digits(Ds),
  {Ds \== []},
  dcg_multi(uncertainty_sign, 1-_, [], N),
  {
    number_codes(X, Ds),
    Multiplier is 10**N,
    Year1 is X * Multiplier,
    Y is X + 1,
    Z is Y * Multiplier,
    Year2 is Z - 1
  }.

uncertainty_sign --> "X".
uncertainty_sign --> "?".

convert_epoch(Year, 'AD', Year):- !.
convert_epoch(UnsignedYear, 'BC', SignedYear):-
  SignedYear is -1 * UnsignedYear.

epoch('BC') --> "v", blanks, ".", blanks, "Chr".
epoch('AD').
