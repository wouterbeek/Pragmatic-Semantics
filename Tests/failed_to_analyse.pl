:- module(failed_to_analyse, []).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(settings)).

% Meta-option `separator`.
:- meta_predicate(dcg_line_wrap(:,?,?)).
% Meta-argument (DCG rule) `separator`.
:- meta_predicate(dcg_line_wrap(+,//,+,+,?,?)).

:- meta_predicate(dcg_multi(2,?,?)).
:- meta_predicate(dcg_multi(2,?,?,?)).
:- meta_predicate(dcg_multi(2,?,:,?,?)).
:- meta_predicate(dcg_multi(2,?,:,-,?,?)).
:- meta_predicate(dcg_multi_no_arguments(2,+,+,-,+,?,?)).

is_meta(separator).

:- setting(wrap_margin, integer, 80, 'The default wrap margin.').

:- initialization((
  phrase(
    dcg_line_wrap([padding(true)]),
    `Line wrapping ends a line after the given number of characters has been parsed, or once there are no more characters left.`,
    Codes
  ),
  maplist(put_code(user_output), Codes)
)).



%! dcg_line_wrap(+Options)//
% Emits the parsed codes list with interspersed separators using line wrap.
%
% Line wrapping ends a line after the given number of characters
%  has been parsed, or once there are no more characters left.
%
% The following options are supported:
%   1. =|padding(+Padding:boolean)|=
%      Whether padding occurs at the right hand side of the last line
%      Spaces are used for padding.
%      Default: `false`.
%   2. =|separator(:DCG_Body)|=
%      The separator that is emitted between the wrapped lines
%      Default: `newline`.
%   3. =|wrap_margin(+WrapMargin:integer)|=
%      The maxmim width of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.
%
% @arg Options A list of name-value pairs.

dcg_line_wrap(O1) -->
  {
    meta_options(is_meta, O1, O2),
    option(padding(Padding), O2, false),
    option(separator(Separator), O2, newline),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), O2, DefaultWrapMargin)
  },
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).

% The number of characters for one line have been parsed,
%  so it is time for a separator.
% Also, reset the character count and start parsing the next line.
dcg_line_wrap(Padding, Separator, 0, WrapMargin), Separator --> !,
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).
% In the midst of parsing a line.
% Process the next character and decrease the counter.
dcg_line_wrap(Padding, Separator, Remaining1, WrapMargin), [Code] -->
  [Code],
  {Remaining2 is Remaining1 - 1}, !,
  dcg_line_wrap(Padding, Separator, Remaining2, WrapMargin).
% The last character was consumed and no space padding occurs (option).
dcg_line_wrap(false, _Separator, _Remaining, _WrapMargin) --> !, [].
% The last character was consumed add space padding occurs (option).
dcg_line_wrap(true, _Separator, Remaining, _WrapMargin),
    dcg_multi(" ", Remaining-Remaining) --> !, [].

newline --> line_feed.
line_feed --> [10].

dcg_multi(DCG) -->
  dcg_multi(DCG, _Rep).
dcg_multi(DCG, Rep) -->
  dcg_multi(DCG, Rep, []).
dcg_multi(DCG, Rep, O1) -->
  dcg_multi(DCG, Rep, O1, _C).
dcg_multi(DCG, Rep, O1, C) -->
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_no_arguments(DCG, Max, 0, C, O2),
  {in_between(Min, Max, C)}.

% Zero arguments: no distinction between `var` and `nonvar`.
dcg_multi_no_arguments(_DCG, _Max, C, C, _O1) --> [].
dcg_multi_no_arguments(DCG, Max, C1, C3, O1) -->
  phrase(DCG),
  % Check that counter does not exceed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_no_arguments(DCG, Max, C2, C3, O1).

in_between(Min, Max, N):-
  greater_than_or_equal_to(N, Min),
  greater_than_or_equal_to(Max, N).

greater_than_or_equal_to(inf, _):- !.
greater_than_or_equal_to(_, inf):- !, fail.
greater_than_or_equal_to(X, Y):-
  X >= Y.

%! repetition(
%!   +Repetitions:or([pair(or([nonneg,oneof([inf])])),or([nonneg,oneof([inf])])]),
%!   -Minimum:nonneg,
%!   -Maximum:nonneg
%! ) is det.
% Determines a repetition interval for DCG-multi.
%
% ## Examples
%
% ~~~
% ?- dcg_multi:repetition(10, Min, Max).
% Min = Max, Max = 10.
% ~~~
%
% ~~~
% ?- dcg_multi:repetition(45-_, Min, Max).
% Min = 45,
% Max = inf.
% ~~~

repetition(Rep, Min2, Max2):-
  (
    Rep == 0
  ->
    Min2 = 0,
    Max2 = 0
  ;
    % A single value.
    is_repetition_value(Rep)
  ->
    Min2 = 1,
    Max2 = Rep
  ;
    Rep = Min1-Max1,
    default(Min1, 0, Min2),
    is_repetition_value(Min2),
    default(Max1, inf, Max2),
    is_repetition_value(Max2)
  ),
  greater_than_or_equal_to(Max2, Min2).

%! is_repetition_value(+Value) is semidet.
% Succeeds if the given value could be used to designate
% a DCG-multi repetition interval,
% i.e., is either an integer or the atom `inf`.

is_repetition_value(V):-
  must_be(nonneg, V), !.
is_repetition_value(V):-
  V == inf.

%! default(?Value, +Default:term, -SetValue:term) is det.
% Returns either the given value or the default value in case there is no
% value given.
%
% @arg Value A term or a variable.
% @arg Default A term.
% @arg SetValue A term.

default(Value, Default, Default):-
  var(Value), !.
default(Value, _Default, Value).

