:- module(
  xsd_duration,
  [
    xsd_duration_canonical_map//1, % +Duration:compound
    xsd_duration_lexical_map//1, % -Duration:compound
    xsd_duration_compare/3, % ?Order:oneof([<,=,>])
                            % +Duration1:compound
                            % +Duration2:compound
    dateTimePlusDuration/3 % +Duration:compound
                           % +DateTime1:compound
                           % -DateTime2:compound
  ]
).

/** <module> XSD duration datatype

*=duration=* is a datatype that represents durations of time.
The concept of duration being captured is drawn from those of ISO 8601,
specifically durations without fixed endpoints.
For example, "15 days" (whose most common lexical representation in duration
is =P15D=) is a duration value; "15 days beginning 12 July 1995" and
"15 days ending 12 July 1995" are not duration values.
Duration can provide addition and subtraction operations between duration
values and between duration/dateTime value pairs, and can be the result of
subtracting dateTime values. However, only addition to dateTime is required
for XML Schema processing and is defined in dateTimePlusDuration/2.

### Value Space

Duration values can be modelled as two-property tuples.
Each value consists of an integer number of months and a decimal number of
seconds. The seconds value must not be negative if the months value is
positive and must not be positive if the months is negative.

Durations are partially ordered.
Equality of duration is defined in terms of equality of dateTime;
order for duration is defined in terms of the order of dateTime.
Specifically, the equality or order of two duration values is determined
by adding each duration in the pair to each of the following four
dateTime values:
~~~
1696-09-01T00:00:00Z
1697-02-01T00:00:00Z
1903-03-01T00:00:00Z
1903-07-01T00:00:00Z
~~~
If all four resulting dateTime value pairs are ordered the same way
(less than, equal, or greater than), then the original pair of duration
values is ordered the same way; otherwise the original pair is incomparable.

These four values are chosen so as to maximize the possible differences
in results that could occur, such as the difference when adding =P1M= and
=P30D= (since =|P1M <> P30D|=). Example:
~~~
1697-02-01T00:00:00Z + P1M < 1697-02-01T00:00:00Z + P30D
1903-03-01T00:00:00Z + P1M > 1903-03-01T00:00:00Z + P30D
~~~
If two duration values are ordered the same way when added to each of these
four dateTime values, they will retain the same order when added to any
other dateTime values.
Therefore, two duration values are incomparable if and only if they can ever
result in different orders when added to any dateTime value.

Under the definition just given, two duration values are equal
if and only if they are identical.

Two totally ordered datatypes =yearMonthDuration= and =dayTimeDuration=
are derived from duration.

### Lexical mapping

The lexical representations of duration are more or less based on the pattern:
~~~
PnYnMnDTnHnMnS
~~~
More precisely, the lexical space is the set of character strings satisfying
xsd_duration_lexical_map//

~~~{.ebnf}
duYearFrag ::= unsignedNoDecimalPtNumeral 'Y'
duMonthFrag ::= unsignedNoDecimalPtNumeral 'M'
duDayFrag ::= unsignedNoDecimalPtNumeral 'D'
duHourFrag ::= unsignedNoDecimalPtNumeral 'H'
duMinuteFrag ::= unsignedNoDecimalPtNumeral 'M'
duSecondFrag ::= (unsignedNoDecimalPtNumeral | unsignedDecimalPtNumeral) 'S'
duYearMonthFrag ::= (duYearFrag duMonthFrag?) | duMonthFrag
duTimeFrag ::=
    'T'
    ( (duHourFrag duMinuteFrag? duSecondFrag?)
    | (duMinuteFrag duSecondFrag?)
    | duSecondFrag)
duDayTimeFrag ::= (duDayFrag duTimeFrag?) | duTimeFrag
~~~

--

@author Wouter Beek
@version 2013/07-2013/08, 2014/03-2014/04
*/

:- use_module(xsd(xsd_dateTime)).
:- use_module(xsd(xsd_dateTime_generic)).
:- use_module(xsd(xsd_dateTime_support)).
:- use_module(xsd(xsd_decimal)).
:- use_module(xsd(xsd_duration_generic)).
:- use_module(xsd(xsd_number_generic)).



% CANONICAL MAP %

%! xsd_duration_canonical_map(+Duration:compound)//
% Maps a duration's property values to xsd_duration_lexical_map// fragments and
% combines the fragments into a complete xsd_duration_lexical_map//.
%
% Compound terms that represent durations have the following form:
% ~~~
% duration(Months:nonneg, Seconds:float)
% ~~~
%
% @arg Duration A complete duration value.

xsd_duration_canonical_map(duration(M1,S1)) -->
  % Emit sign.
  (
    {(M1 < 0 ; S1 < 0.0)}
  ->
    `-`
  ;
    ``
  ),

  % Emit duration value symbol.
  `P`,

  % Emit duration absolute value.
  (
    {M1 =\= 0, S1 =\= 0.0}
  ->
    {M2 is abs(M1)},
    duYearMonthCanonicalFragmentMap(M2),
    {S2 is abs(S1)},
    duDayTimeCanonicalFragmentMap(S2)
  ;
    {M1 =\= 0, S1 =:= 0.0}
  ->
    {M2 is abs(M1)},
    duYearMonthCanonicalFragmentMap(M2)
  ;
    {M1 =:= 0}
  ->
    {S2 is abs(S1)},
    duDayTimeCanonicalFragmentMap(S2)
   ).


%! duDayCanonicalFragmentMap(+NumberOfDays:nonneg)//
% Maps a nonnegative integer, presumably the day normalized value from the
% seconds of a duration value, to a duDayFrag//, a fragment of a duration
% lexical representation.
%
% @arg NumberOfDays A nonnegative integer.

duDayCanonicalFragmentMap(NumberOfDays) -->
  {NumberOfDays =:= 0}, !.
duDayCanonicalFragmentMap(NumberOfDays) -->
  unsignedNoDecimalPtCanonicalMap(NumberOfDays),
  `D`.


%! duDayTimeCanonicalFragmentMap(+Seconds:float)
% Maps a nonnegative decimal number, presumably the absolute value of
% the seconds of a duration value, to a duDayTimeFrag//,
% a fragment of a duration lexical representation.
%
% @arg Seconds A nonnegative decimal number.

duDayTimeCanonicalFragmentMap(NumberOfSeconds) -->
  {NumberOfSeconds =:= 0}, !,
  `T0S`.
duDayTimeCanonicalFragmentMap(NumberOfSeconds1) -->
  {
    % Days.
    xsd_number_generic:(NumberOfDays     is NumberOfSeconds1 xsd_div 86400),
    % Hours.
    % h is (ss mod 86400) div 3600.
    xsd_number_generic:(X                is NumberOfSeconds1 xsd_mod 86400),
    xsd_number_generic:(NumberOfHours    is X                xsd_div 3600 ),
    % Minutes.
    % m is (ss mod 3600) div 60.
    xsd_number_generic:(Y                is NumberOfSeconds1 xsd_mod 3600 ),
    xsd_number_generic:(NumberOfMinutes  is Y                xsd_div 60   ),
    % Seconds.
    % s is ss mod 60.
    xsd_number_generic:(NumberOfSeconds2 is NumberOfSeconds1 xsd_mod 60   )
  },
  duDayCanonicalFragmentMap(NumberOfDays),
  duTimeCanonicalFragmentMap(
    NumberOfHours,
    NumberOfMinutes,
    NumberOfSeconds2
  ).


%! duHourCanonicalFragmentMap(+NumberOfHours:nonneg)//
% Maps a nonnegative integer, presumably the hour normalized value from the
% seconds of a duration value, to a duHourFrag//, a fragment of a duration
% lexical representation.
%
% NumberOfHours A nonnegative integer.

duHourCanonicalFragmentMap(H) -->
  {H =:= 0}, !.
duHourCanonicalFragmentMap(H) -->
  unsignedNoDecimalPtCanonicalMap(H),
  `H`.


%! duMinuteCanonicalFragmentMap(+NumberOfMinutes:nonneg)//
% Maps a nonnegative integer, presumably the minute normalized value from
% the ·seconds· of a duration value, to a duMinuteFrag//, a fragment of
% a duration lexical representation.
%
% @arg NumberOfMinutes A nonnegative integer.

duMinuteCanonicalFragmentMap(M) -->
  {M =:= 0}, !.
duMinuteCanonicalFragmentMap(M) -->
  unsignedNoDecimalPtCanonicalMap(M),
  `M`.


%! duSecondCanonicalFragmentMap(+NumberOfSeconds:float)//
% Maps a nonnegative decimal number, presumably the second normalized value
% from the seconds of a duration value, to a duSecondFrag//, a fragment of
% a duration lexical representation.
%
% @arg NumberOfSeconds A nonnegative decimal number.

duSecondCanonicalFragmentMap(S) -->
  {S =:= 0.0}, !.
duSecondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsignedNoDecimalPtCanonicalMap(S),
  `S`.
duSecondCanonicalFragmentMap(S) -->
  unsignedDecimalPtCanonicalMap(S),
  `S`.


%! duTimeCanonicalFragmentMap(
%!   +NumberOfHours:nonneg,
%!   +NumberOfMinutes:nonneg,
%!   +NumberOfSeconds:float
%! )//
% Maps three nonnegative numbers, presumably the hour, minute, and second
% normalized values from a duration's seconds, to a duTimeFrag//, a fragment
% of a duration lexical representation.
%
% @arg NumberOfHours A nonnegative integer.
% @arg NumberOfMinutes A nonnegative integer.
% @arg NumberOfSeconds A nonnegative decimal number.

duTimeCanonicalFragmentMap(H, M, S) -->
  {H =:= 0, M =:= 0, S =:= 0.0}, !.
duTimeCanonicalFragmentMap(H, M, S) -->
  `T`,
  duHourCanonicalFragmentMap(H),
  duMinuteCanonicalFragmentMap(M),
  duSecondCanonicalFragmentMap(S).


%! duYearMonthCanonicalFragmentMap(+NumberOfMonths:nonneg)//
% Maps a nonnegative integer, presumably the absolute value of the months
% of a duration value, to a duYearMonthFrag//, a fragment of a duration
% lexical representation.
%
% @arg NumberOfMonths A nonnegative integer.

duYearMonthCanonicalFragmentMap(NumberOfMonths1) -->
  {xsd_number_generic:(NumberOfYears is NumberOfMonths1 xsd_div 12)},
  (
    {NumberOfYears =:= 0}
  ;
    unsignedNoDecimalPtCanonicalMap(NumberOfYears),
    `Y`
  ), !,
  {xsd_number_generic:(NumberOfMonths2 is NumberOfMonths1 xsd_mod 12)},
  (
    {NumberOfMonths2 =:= 0}
  ;
    unsignedNoDecimalPtCanonicalMap(NumberOfMonths2),
    `M`
  ), !.



% LEXICAL MAP %

%! xsd_duration_lexical_map(-Duration:compound)// is det.
% ~~~{.ebnf}
% xsd_duration_lexical_map ::=
%     '-'? 'P' ((duYearMonthFrag duDayTimeFrag?) | duDayTimeFrag)
% ~~~
%
% ### RE
%
% Seperate REs:
%   * =|-?P[0-9]+Y?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+(\.[0-9]+)?S)?)?|=
%   * =|.*[YMDHS].*|=, at least one field occurs.
%   * =|.*[^T]|=, where =T= cannot be the final character.
%
% Combined RE:
% ~~~{.re}
% -?P( ( ( [0-9]+Y([0-9]+M)?([0-9]+D)?
%      | ([0-9]+M)([0-9]+D)?
%      | ([0-9]+D)
%      )
%      (T ( ([0-9]+H)([0-9]+M)?([0-9]+(\.[0-9]+)?S)?
%         | ([0-9]+M)([0-9]+(\.[0-9]+)?S)?
%         | ([0-9]+(\.[0-9]+)?S)
%         )
%      )?
%   )
% | (T ( ([0-9]+H)([0-9]+M)?([0-9]+(\.[0-9]+)?S)?
%      | ([0-9]+M)([0-9]+(\.[0-9]+)?S)?
%      | ([0-9]+(\.[0-9]+)?S)
%      )
%   )
% )
% ~~~
%
% @arg Duration A compound term of the form
%      =|duration(Months:integer,Seconds:float)|=.

xsd_duration_lexical_map(duration(M,S)) -->
  (`-` -> {Sign = -1} ; {Sign = 1}),
  `P`,
  (
    duYearMonthFrag(Y),
    (duDayTimeFrag(D) ; {D = 0})
  ;
    {Y = 0},
    duDayTimeFrag(D)
  ),
  {
    M is copysign(Y, Sign),
    S is copysign(D, Sign)
  }.


%! duDayFrag(-Day:nonneg)//
% ~~~{.ebnf}
% duDayFrag ::= unsignedNoDecimalPtNumeral 'D'
% ~~~

duDayFrag(D) -->
  unsignedNoDecimalPtNumeral(D),
  `D`.


%! duDayTimeFrag(-Seconds:nonneg)//
% ~~~{.ebnf}
% duDayTimeFrag ::= (duDayFrag duTimeFrag?) | duTimeFrag
% ~~~

duDayTimeFrag(DT) -->
  (
    duDayFrag(D), (duTimeFrag(T) ; {T = 0})
  ;
    {D = 0}, duTimeFrag(T)
  ),
  {DT is 86400 * D + T}.


%! duHourFrag(-Hour:nonneg)//
% ~~~{.ebnf}
% duHourFrag ::= unsignedNoDecimalPtNumeral 'H'
% ~~~

duHourFrag(H) -->
  unsignedNoDecimalPtNumeral(H),
  `H`.


%! duMinuteFrag(-Minute:nonneg)//
% ~~~{.ebnf}
% duMinuteFrag ::= unsignedNoDecimalPtNumeral 'M'
% ~~~

duMinuteFrag(M) -->
  unsignedNoDecimalPtNumeral(M),
  `M`.


%! duSecondFrag(-Second:or([float,nonneg]))//
% ~~~{.ebnf}
% duSecondFrag ::= (unsignedNoDecimalPtNumeral | unsignedDecimalPtNumeral) 'S'
% ~~~

duSecondFrag(S) -->
  (unsignedNoDecimalPtNumeral(S) ; unsignedDecimalPtNumeral(S)),
  `S`.


%! duTimeFrag(-Second:nonneg)//
% ~~~{.ebnf}
% duTimeFrag ::=
%     'T'
%     ((duHourFrag duMinuteFrag? duSecondFrag?)
%     | (duMinuteFrag duSecondFrag?)
%     | duSecondFrag)
% ~~~

duTimeFrag(S2) -->
  `T`,
  (
    (duHourFrag(H), (duMinuteFrag(M) ; ""), (duSecondFrag(S1) ; ""))
  ;
    {H = 0}, (duMinuteFrag(M), (duSecondFrag(S1) ; ""))
  ;
    {H = 0}, {M = 0}, duSecondFrag(S1)
  ),
  {S2 is 3600 * H + 60 * M + S1}.



% RELATIONS %

%! xsd_duration_compare(
%!   -Order:oneof([<,=,>]),
%!   +Duration1:compound,
%!   +Duration2:compound
%! ) is semidet.
% Fails only if the given values are incomparable.
%
% Equality of duration is defined in terms of equality of dateTime;
% order for duration is defined in terms of the order of dateTime.
% Specifically, the equality or order of two duration values
% is determined by adding each duration in the pair
% to each of the following four dateTime values:
%   * `1696-09-01T00:00:00Z`
%   * `1697-02-01T00:00:00Z`
%   * `1903-03-01T00:00:00Z`
%   * `1903-07-01T00:00:00Z`
%
% If two duration values are ordered the same way when added to
% each of these four dateTime values, they will retain the same order
% when added to any other dateTime values.
% @tbd Where's the proof for this?

xsd_duration_compare(Order, Dur1, Dur2):-
  newDateTime(1696, 9, 1, 0, 0, 0.0, 0, DTa),
  newDateTime(1697, 2, 1, 0, 0, 0.0, 0, DTb),
  newDateTime(1903, 3, 1, 0, 0, 0.0, 0, DTc),
  newDateTime(1903, 7, 1, 0, 0, 0.0, 0, DTd),
  
  dateTimePlusDuration(Dur1, DTa, Dur1a),
  dateTimePlusDuration(Dur1, DTb, Dur1b),
  dateTimePlusDuration(Dur1, DTc, Dur1c),
  dateTimePlusDuration(Dur1, DTd, Dur1d),

  dateTimePlusDuration(Dur2, DTa, Dur2a),
  dateTimePlusDuration(Dur2, DTb, Dur2b),
  dateTimePlusDuration(Dur2, DTc, Dur2c),
  dateTimePlusDuration(Dur2, DTd, Dur2d),
  
  xsd_dateTime_compare(Order, Dur1a, Dur2a),
  xsd_dateTime_compare(Order, Dur1b, Dur2b),
  xsd_dateTime_compare(Order, Dur1c, Dur2c),
  xsd_dateTime_compare(Order, Dur1d, Dur2d).



% FUNCTIONS %

%! dateTimePlusDuration(
%!   +Duration:compound,
%!   +DateTime1:compound,
%!   -DateTime2:compound
%! ) is det.
% Adds a duration to a dateTime value, producing another dateTime value.
%
% ### Non-commutative
%
% This addition is not commutative, i.e.
% the order of addition of durations to instants is significant.
% See module [xsd_test].
%
% ### Algorithm
%
% Arguments:
%   * =du=: a duration value
%   * =dt=: a dateTime value
% Result: a dateTime value
%
% Let:
%   - =yr= be =dt='s year,
%   - =mo= be =dt='s month,
%   - =da= be =dt='s day,
%   - =hr= be =dt='s hour,
%   - =mi= be =dt='s minute, and
%   - =se= be =dt='s second.
%   - =tz= be =dt='s timezoneOffset.
%
% Steps:
%  1. Add =du='s months to =mo=.
%  2. =|normalizeMonth(yr, mo)|=
%     (I.e., carry any over- or underflow, adjust month.)
%  3. Set =da= to =|min(da, daysInMonth(yr, mo))|=.
%     (I.e., pin the value if necessary.)
%  4. Add =du='s seconds to =se=.
%  5. =|normalizeSecond(yr, mo, da, hr, mi, se)|=.
%     (I.e., carry over- or underflow of seconds up to minutes, hours, etc.)
%  6. Return =|newDateTime·(yr, mo, da, hr, mi, se, tz)|=
%
% @see http://www.w3.org/TR/xmlschema11-2/#vp-dt-dateTimePlusDuration

dateTimePlusDuration(duration(M1,S1), dateTime(Y2,M2,D2,H2,MM2,S2,TZ), DT):-
  M3 is M1 + M2,
  normalizeMonth(Y2, M3, NormY, NormM),
  daysInMonth(NormY, NormM, Days),
  D3 is min(D2, Days),
  S3 is S1 + S2,
  normalizeSecond(NormY, NormM, D3, H2, MM2, S3, Y0, M0, D0, H0, MM0, S0),
  newDateTime(Y0, M0, D0, H0, MM0, S0, TZ, DT).

