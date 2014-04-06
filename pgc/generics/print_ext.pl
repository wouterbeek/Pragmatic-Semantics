:- module(
  print_ext,
  [
    formatnl/1, % +Format
    formatnl/2, % +Format
                % :Arguments
    formatnl/3, % +Output
                % +Format
                % :Arguments
    indent/1 % +Indent:integer
  ]
).

/** <module> Print extensions

Predicates for printing.

## Proof

A datatype of the following form:
~~~{.pl}
proof(Conclusion, Premises)
~~~

@author Wouter Beek
@tbd Remove all predicate variants that have an `Out` parameter.
     The calling context should use with_output_to/2 instead.
@version 2013/01-2013/02, 2013/04-2013/05, 2013/07-2013/09, 2013/11
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(dcg_os)).
:- use_remote_module(generics(codes_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(option_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_remote_module(os(shell_ext)).
:- use_remote_module(pl(pl_control)).
:- use_remote_module(pl(pl_mode)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).

:- setting(
  screen_width,
  integer,
  80,
  'The default width of the screen in number of characters.'
).



%! formatnl(+Format) is det.
% @see Variant of format/1 with a newline appended.

formatnl(Format1):-
  term_to_atom(Format1, Format2),
  format(Format2),
  nl.

%! formatnl(+Format, :Arguments) is det.
% @see Variant of format/2 with a newline appended.

formatnl(Format, Arguments):-
  format(Format, Arguments),
  nl.

%! formatnl(+Output, +Format, :Arguments) is det.
% @see Variant of format/3 with a newline appended.

formatnl(Out, Format, Arguments):-
  format(Out, Format, Arguments),
  nl(Out).

%! indent(+Indent:integer) is det.
% @see Like tab/1, but writes the given number of indents, where
%      a single indent can be multiple spaces.
%      See setting `indent_size`.

indent(Indent):-
  setting(indent_size, IndentSize),
  NumberOfSpaces is IndentSize * Indent,
  tab(NumberOfSpaces).

