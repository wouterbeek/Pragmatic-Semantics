:- module(
  abnf2dcg,
  [
    abnf2dcg/1 % +File:atom
  ]
).

/** <module> ABNF2DCG

Converts ABNF grammars to DCGs.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(flp(rfc4234_basic)).
:- use_module(generics(db_ext)).
:- use_module(library(pio)).
:- use_module(library(plunit)).
:- use_module(math(radix)).

abnf2dcg(File):-
  access_file(File, read),
  phrase_from_file(abnf, File).



% GRAMMAR %

abnf -->
  dcg_multi(rule).

%! base(?Radix:oneof([2,10,16]))//

base(2)  --> b.
base(10) --> d.
base(16) --> x.

elements(Name) -->
  element(Name).

element(Name) -->
  single_terminal_value(Code),
  {db_add_dcg_rule(Name, [Code])}.

name(Name) -->
  ci_string(Name).

rule -->
  name(Name),
  blanks, equals_sign, blanks,
  elements(Name),
  'CRLF'.

%! single_terminal_value(-Code:positive_integer)//
% Reads a character code from the input stream.
% The character code is described in a given radix or base
% (either `2`, `10`, or `16`).

single_terminal_value(Code) -->
  percent_sign,
  base(Radix),
  hexadecimal_digit(_, H1),
  hexadecimal_digit(_, H2),
  {digits_to_decimal([H1,H2], Radix, Code)}.



:- begin_tests(abnf2dcg).

test(abnf1, [true]):-
  absolute_file_name(project('abnf.abnf'), File, [access(read)]),
  abnf2dcg(File).

:- end_tests(abnf2dcg).

