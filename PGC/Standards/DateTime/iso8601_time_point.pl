:- module(
  iso8601_time_point,
  [
    iso8601_local_time//4 % -Tree:compound
                          % ?Format:oneof([basic,extended])
                          % ?TimeDesignator:boolean
                          % ?UTC_Time:compound
  ]
).

/** <module> ISO8601_TIME_POINT

# Custom datetypes

Time:
~~~
time(
  ?Hour:between(0.0,24.0),
  ?Minute:between(0.0,60.0),
  ?Second:between(0.0,60.0)
)
~~~

UTC correction:
~~~
utc(
  ?UTC_Designator:boolean,
  ?Sign:boolean,
  ?Hour:between(0.0,24.0),
  ?Minute:between(0.0,60.0)
)
~~~

UTC-corrected time:
~~~
utc_time(
  ?Time:compound,
  ?UTC_Correction:compound
)
~~~

--

# Time of day representations

*Hour* is represented by two digits from =00= to =24=.
The representation of the hour by =24= is only allowed to indicate the end of
a calendar day.

*Minute* is represented by two digits from =00= to =59=.

*Second* is represented by two digits from =00= to =60=.

The representation of the second by =60= is only allowed to indicate
a positive leap second or an instant within that second.

These expressions apply to both UTC and non-UTC based time scales
for time of day.

## Local time

### Complete representations

Basic format:
~~~
hhmmss
~~~
Example: =|232050|=

Extended format:
~~~
hh:mm:ss
~~~
Example: =|23:20:50|=

### Reduced accuracy representations

Basic format:
~~~
hhmm
~~~
Example: =|2320|=

Extended format:
~~~
hh:mm
~~~
Example: =|23:20|=

Basic format:
~~~
hh
~~~
Example: =|23|=

### Representations with decimal fraction

If a decimal fraction is included, lower order time elements (if any) shall be
omitted and the decimal fraction shall be divided from the integer part by the
decimal sign specified in ISO 31-0, i.e. the comma =|[,]|= (preferred)
or full stop =|[.]|=.

If the magnitude of the number is less than unity
(i.e. $0,0 \leq d \leq 1,0$), the decimal sign shall be preceded by two zeros
in accordance with zero padding in the rest of this specification.

The interchange parties, dependent upon the application, shall agree
the number of digits in the decimal fraction. The format shall be
=|[hhmmss,ss]|=, =|[hhmm,mm]|= or =|[hh,hh]|= as appropriate, with as many
digits as necessary following the decimal sign.

A decimal fraction shall have at least one digit.

Basic format:
~~~
hhmmss,ss
~~~
Example: =|232050,5|=

Extended format:
~~~
hh:mm:ss,ss
~~~
Example: =|23:20:50,5|=

Basic format:
~~~
hhmm,mm
~~~
Example: =|2320,8|=

Extended format:
~~~
hh:mm,mm
~~~
Example: =|23:20,8|=

Basic format:
~~~
hh,hh
~~~
Example: =|23,3|=

### Time designator

In expressions of local time, applications may put the time designator =|[T]|=
immediately in front of the representations defined above.

### Midnight

The complete representations in basic and extended format for midnight:

#### The beginning of a calendar day

Basic format:
~~~
000000
~~~

Extended format:
~~~
00:00:00
~~~

#### The end of a calendar day

Basic format:
~~~
240000
~~~

Extended format:
~~~
24:00:00
~~~

These representations may have reduced accuracy and/or may contain the time
designator.

These representations may be expanded with a decimal fraction containing only
zeros.

Midnight will normally be represented as =|00:00|= or =|24:00|=.

The end of one calendar day =|24:00|= coincides with =|00:00|= at the start
of the next calendar day. For example, =|24:00|= on 12 April 1985 is the same
as =|00:00|= on 13 April 1985. If there is no association with a date or a
time interval, then these representations represent the same local time in
the 24-hour timekeeping system.

Representations where =|hh|= has the value =|24|= are only preferred to
represent the end of a time interval or recurring time interval (see below).

### UTC of day

To express UTC of day the complete local time representations, possibly
including decimal fractions and/or reduced accuracy, are used.

This means that the time designator is not allowed.

Such a local time representation is followed immediately by
the UTC designator =|[Z]|=.

The examples below are complete and reduced
accuracy representations of the UTC of day 20 minutes and 30 seconds past 23 hours:

Basic format:
~~~
hhmmssZ
hhmmZ
hhZ
~~~
Examples:
  * =|232030Z|=
  * =|2320Z|=
  * =|23Z|=

Extended format:
~~~
hh:mm:ssZ
hh:mmZ
~~~
Examples:
  * =|23:20:30Z|=
  * =|23:20Z|=

### Local time and UTC time

When it is required to indicate the difference between local time and UTC of
day, the representation of the difference can be expressed in hours and
minutes, or hours only. It shall be expressed as positive (i.e. with the
leading plus sign =|[+]|=) if the local time is ahead of or equal to UTC
of day and as negative (i.e. with the leading minus sign =|[-]|=) if it is
behind UTC of day.

The minutes time element of the difference may only be omitted if the
difference between the time scales is exactly an integral number of hours.

Basic format:
~~~
±hhmm
±hh
~~~
Examples:
  * =|+0100|=
  * =|+01|=

Extended format:
~~~
±hh:mm
~~~
Example: =|+01:00|=

Expressions of the difference between local time and UTC of day are a
component in the representations defined below.
They are not used in isolation.

When it is required to indicate local time and the difference between
the time scale of local time and UTC, the representation of the difference
shall be appended to the representation of the local time following
immediately, without space, the lowest order (extreme right-hand) time
element of the local time expression.

The difference between the time scale of local time and UTC shall be
expressed in hours-and-minutes, or hours-only independent of the accuracy of
the local time expression associated with it.

In the following examples we represent the complete representation of the time
of 27 minutes and 46 seconds past 15 hours locally in Geneva (in winter one
hour ahead of UTC), and in New York (in winter five hours behind UTC),
together with the indication of the difference between the time scale of
local time and UTC, are used as examples.

Basic format:
~~~
hhmmss±hhmm
hhmmss±hh
~~~
Examples:
  * =|152746+0100|=
  * =|152746−0500|=
  * =|152746+01|=
  * =|152746−05|=

Extended format:
~~~
hh:mm:ss±hh:mm
hh:mm:ss±hh
~~~

Examples:
  * =|15:27:46+01:00|=
  * =|15:27:46−05:00|=
  * =|15:27:46+01|=
  * =|15:27:46−05|=

In these expressions the local time component may be represented with
reduced accuracy and/or with decimal fraction.

@author Wouter Beek
@version 2013/08
*/

:- use_module(datetime(iso8601_generic)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(parse_tree)).
:- use_module(math(math_ext)).



%! iso8601_local_time(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?UTC_Time:oneof([boolean,compound])
%! )//
% Processes local time representations.
%
% ### Midnight
%
% The representation of midnight necessitates the following restrictions
% on processing representations of local time:
%
% *|[A1]|* If hour is =24=, then minute (if present) must be =00=,
%          and any decimal expansion must consist of zeros.
%
% *|[A2]|* If hour is =24=, then second (if present) must be =00=,
%          and any decimal expansion must consist of zeros.
%
% *|[B1]|* If hour is =00= and hour has a decimal expansion, than that
%          expansion must consist of zeros exclusively.
%
% *|[B2]|* If hour and minute are =00= and minute has
%          a decimal expansion than that expansion must consist
%         of zeros exclusively.
%
% *|[B3]|* If hour, minute, and second are =00= and second has
%          a decimal expansion, than that expansion must consist
%          of zeros exclusively.
%
% *|[C1]|* If minute is instantiated, then hour must not have
%          a decimal expansion.
%
% *|[C2]|* If minute and second are instantiated, then hour and
%          minute must not have a decimal expansion.
%
% @arg Tree A compound term representing a parse tree.
% @arg Format Either `basic` for the basic or compressed format,
%      or `extended` for the extended or human readable format.
% @arg TimeDesignator A boolean indicating whether the time designator is
%      included or not.
% @arg UTC_Time A compound term.

iso8601_local_time(T0, Format, T, utc_time(time(H,M,S),UTC)) -->
  % Time designator.
  ({T = false} ; {T = true}, iso8601_time_designator(T1)),

  % Hour
  iso8601_hour_in_day(T2, H),
  {number_parts(H, H_I, H_F)},

  % Minute and second
  (
    {(var(M), var(S))},
    % [A1] If hour is =24=, then its decimal expansion must
    %      consist of zeros exclusively.
    % [B1] If hour is =00=, then its decimal expansion must
    %      consist of zeros exclusively.
    {(H_I =:= 0 -> H_F = 0 ; true)}
  ;
    % [C1] If minute is instantiated, then hour must not have
    %      a decimal expansion.
    {H_F = 0},
    (colon, {Format = extended} ; {Format = basic}),
    iso8601_minute_in_hour(T3, M),
    {number_parts(M, M_I, M_F)},
    % [A1] If hour is =24=, then minutes must be =00= and its
    %      decimal expansion (if any) must consist of zeros exclusively.
    {(H_I =:= 24 -> M_I =:= 0, M_F = 0 ; true)},

    % Second
    (
      {var(S)},
      % [B2] If hour and minute are =00=, then minutes's decimal extension
      %      must consist of zeros exclusively.
      {(H_I =:= 0, M_I =:= 0 -> M_F = 0 ; true)}
    ;
      % [C2] If minute and second are instantiated, then hour and minute
      %      must not have a decimal expansion.
      {H_F = 0, M_F = 0},
      (colon, {Format = extended} ; {Format = basic}),
      iso8601_second_in_minute(T4, S),
      {number_parts(S, S_I, S_F)},
      % [C3] If hour, minute, and second are =00=, then minutes's decimal
      %      extension must consist of zeros exclusively.
      {(H_I =:= 0, M_I =:= 0, S_I =:= 0 -> S_F = 0 ; true)},
      % [A2] If hour is =24=, then second must be =00= and its
      %      decimal expansion (if any) must consist of zeros exclusively.
      {(H_I =:= 24 -> S_I =:= 0, S_F = 0 ; true)}
    )
  ),

  % The UTC correction or designator.
  (
    % UTC time
    iso8601_utc_designator(T5), {UTC = true}
  ;
    % Local time
    {UTC = false}
  ;
    % Local time + UTC correction
    iso8601_utc_correction(T6, Format, UTC)
  ),

  % Parse tree
  {parse_tree(local_time, [T1,T2,T3,T4,T5,T6], T0)}.

%! iso8601_utc_correction(
%!   -Tree:compound,
%!   ?Format:oneof([basic,extended]),
%!   ?UTC_Correction:compound
%! )//

iso8601_utc_correction(T0, Format, utc(Sign,H,M)) -->
  % Sign
  iso8601_sign(T1, Sign),

  % Hour
  iso8601_hour_in_day(T2, H),

  % Minute
  (
    {var(M)}
  ;
    (colon, {Format = extended} ; {Format = basic}),
    iso8601_minute_in_hour(T3, M)
  ),

  % Parse tree
  {parse_tree(utc_correction, [T1,T2,T3], T0)}.



% SPECIFIC SUPPORT PREDICATES %

iso8601_hour_in_day(T0, H) -->
  iso8601_float(T0, hour, 2, H),
  {iso8601_hour_in_day(H)}.

iso8601_minute_in_hour(T0, M) -->
  iso8601_float(T0, minute, 2, M),
  {iso8601_minute_in_hour(M)}.

iso8601_second_in_minute(T0, S) -->
  iso8601_float(T0, second, 2, S),
  {iso8601_second_in_minute(S)}.

iso8601_sign(sign('+'), true) -->
  "+".
iso8601_sign(sign('-'), false) -->
  "-".

iso8601_utc_designator(utc_designator('Z')) -->
  "Z".

