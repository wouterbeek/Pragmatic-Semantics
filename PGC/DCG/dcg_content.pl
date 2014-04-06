:- module(
  dcg_content,
  [
    arrow//2, % +Options:list(nvpair)
              % +Length:nonneg
    between//3, % +Low:nonneg
                % +High:nonneg
                % ?Code:code
    between_hex//3, % +LowHex:atom
                    % +HighHex:atom
                    % ?Code:code
    bracketed//1, % :DCG
    bracketed//2, % +Type:oneof([angular,curly,round,square])
                  % :DCG
    capitalize//0,
    ci_code//1, % ?Code:code
    ci_string//1, % ?String:string
    code//1, % ?Code:code
    codes//1, % +Codes:list(code)
    end_of_line//0,
    graphic//1, % ?Codes:list(code)
    horizontal_line//0,
    horizontal_line//1, % +Length:nonneg
    indent//0,
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :DCG
    nl//0,
    pl_term//1, % +PrologTerm
    quoted//1, % :DCG
    transition//2, % :From
                   % :To
    void//0,
    word//1 % ?Word:atom
  ]
).
:- reexport(
  library(dcg/basics),
  [
    alpha_to_lower//1,
    atom//1, % +Atom:atom
    blank//0,
    blanks//0,
    blanks_to_nl//0,
    nonblank//1,
    nonblanks//1,
    prolog_var_name//1,
    white//0,
    whites//0
  ]
).

/** <module> DCG Content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_unicode)).
:- use_module(generics(error_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(math(radix)).
:- use_module(os(shell_ext)).

:- meta_predicate(quoted(//,?,?)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).

:- meta_predicate(indent(+,//,?,?)).
:- meta_predicate(bracketed(//,?,?)).
:- meta_predicate(bracketed(+,//,?,?)).
:- meta_predicate(transition(//,//,?,?)).



%! arrow(+Options:list(nvpair), +Length:nonneg)// .
% A simple ASCII arrow.
%
% The following options are supported:
%   * =|head(+HeadType:oneof([both,left,right]))|=
%
% Example:
% ~~~{.txt}
% -------->
% ~~~
%
% @arg Options A list of name-value pairs.
% @arg Length A non-negative integer.

arrow(O1, L1) -->
  % Set the default arrow head.
  {option(head(Head), O1, right)},

  % The left arrow head.
  (
    {arrow_left_head(Head)}
  ->
    `<`,
    {L2 is L1 - 1}
  ;
    {L2 = L1}
  ),

  % The dashes in between the arrow heads.
  (
    {arrow_right_head(Head)}
  ->
    {L3 is L2 - 1}
  ;
    {L3 = L2}
  ),
  {L3 >= 0},
  dcg_multi(`-`, L3), !,

  % The right arrow head.
  (
    {arrow_right_head(Head)}
  ->
    `>`
  ;
    ``
  ).

arrow_left_head(both).
arrow_left_head(left).
arrow_right_head(both).
arrow_right_head(right).


%! between(+Low:nonneg, +High:nonneg, +Code:code)// is semidet.
%! between(+Low:nonneg, +High:nonneg, -Code:code)// is nondet.
% Parses or generates an integer between the given limits.
%
% @tbd Support for negative integers and zero.
% @tbd Support for `minf` and `inf`.

between(Low, High, Code) -->
  [Code],
  {between(Low, High, Code)}.


%! between_hex(+LowHex:atom, +HighHex:atom, +Code:code)// is semidet.
%! between_hex(+LowHex:atom, +HighHex:atom, -Code:code)// is nondet.

between_hex(LowHex, HighHex, Code) -->
  {number_to_decimal(LowHex, 16, Low)},
  {number_to_decimal(HighHex, 16, High)},
  between(Low, High, Code).


%! bracketed(:DCG)// .
%! bracketed(+Mode:oneof([angular,curly,round,square]), :DCG)// .

bracketed(DCG) -->
  bracketed(round, DCG).

bracketed(Mode, DCG) -->
  dcg_between(
    opening_bracket(_, Mode),
    DCG,
    closing_bracket(_, Mode)
  ),
  % Remove choicepoints for brackets of other types in [dcg_ascii].
  !.


%! capitalize// .

capitalize, [Upper] -->
  [Lower],
  {code_type(Upper, to_upper(Lower))}, !,
  dcg_copy.
capitalize -->
  dcg_end.


%! ci_code(?Code:code)// .
% Generates the case-insensitive variants of the given code.

ci_code(Code) -->
  {nonvar(Code)}, !,
  (
    {code_type(Code, lower(Upper))}
  ->
    ([Code] ; [Upper])
  ;
    {code_type(Code, upper(Lower))}
  ->
    ([Code] ; [Lower])
  ;
    [Code]
  ).
ci_code(CI_Code) -->
  % This allows ci_string//1 to use this DCG rule for reading words
  % in a case-sensitive way.
  u_graphic(Code),
  {(
    code_type(Code, upper(CI_Code))
  ;
    CI_Code = Code
  )}, !.


%! ci_string(?String:list(code))// .
% Generates the case-insensitive variants of the given string.
%
% Example:
% ~~~
% ?- phrase(ci_string("http"), Codes) ,atom_codes(Atom, Codes).
% Codes = "http",
% Codes = "httP",
% Codes = "htTp",
% Codes = "htTP",
% Codes = "hTtp",
% Codes = "hTtP",
% Codes = "hTTp",
% Codes = "hTTP",
% Codes = "Http",
% Codes = "HttP",
% Codes = "HtTp",
% Codes = "HtTP",
% Codes = "HTtp",
% Codes = "HTtP",
% Codes = "HTTp",
% Codes = "HTTP",
% false.
% ~~~

ci_string([]) --> [].
ci_string([H|T]) -->
  ci_code(H),
  ci_string(T).


%! code(+Code:code)// .

code(C) -->
  [C].


%! codes(+Codes:list(code))// .

codes([]) -->
  [].
codes([H|T]) -->
  [H],
  codes(T).


%! end_of_line// .

end_of_line -->
  carriage_return,
  line_feed, !.
end_of_line -->
  line_feed, !.
end_of_line -->
  carriage_return.


%! graphic(?Codes:list(code))// .

graphic([H|T]) -->
  u_graphic(H),
  graphic(T).
graphic([]) --> [].


%! horizontal_line// .
%! horizontal_line(+Lenght:nonneg)// .

horizontal_line -->
  {terminal_screen_width(ScreenWidth)},
  horizontal_line(ScreenWidth).

horizontal_line(Length) -->
  dcg_multi(hyphen, Length).


%! indent// is det.
%! indent(+Indent:nonneg)// is det.
%! indent(+Indent:nonneg, :DCG)// is det.

indent -->
  indent(1).

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  dcg_multi(space, NumberOfSpaces).

indent(I, DCG) -->
  indent(I),
  dcg_call(DCG).


%! nl// is det.

nl -->
  line_feed.


%! pl_term(+PrologTerm)// is det.

pl_term(PrologTerm) -->
  {with_output_to(codes(Codes), write_canonical_catch(PrologTerm))},
  Codes.


%! quoted(:DCG)// .

quoted(DCG) -->
  dcg_between(double_quote, DCG).


%! transition(:From, :To)// is det.

transition(From, To) -->
  dcg_call(From),
  dcg_between(space, arrow([head(right)], 2)),
  dcg_call(To).


void --> [].


%! word(?Word:atom)// .
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.

word(Word) -->
  {nonvar(Word)}, !,
  {atom_codes(Word, Codes)},
  word_codes(Codes).
word(Word) -->
  word_codes(Codes),
  {atom_codes(Word, Codes)}.

word_codes([H|T]) -->
  u_letter(H),
  word_codes(T).
word_codes([]) -->
  [].

