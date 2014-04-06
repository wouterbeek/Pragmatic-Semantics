:- module(
  xsd_number_generic,
  [
    decimalPtNumeral//2, % -Sign:oneof([-1,1])
                         % -Decimal:float
    fractionDigitsCanonicalFragmentMap//1, % ?Fraction:rational
    noDecimalPtCanonicalMap//1, % +Integer:integer
    noDecimalPtNumeral//2, % -Sign:oneof([-1,1])
                           % -Integer:integer
    unsignedDecimalPtCanonicalMap//1, % +Decimal:rational
    unsignedDecimalPtNumeral//1, % -Decimal:float
    unsignedNoDecimalPtCanonicalMap//1, %+Integer:nonneg
    unsignedNoDecimalPtNumeral//1, % -Integer:nonneg
    op(400, yfx, xsd_div),
    op(400, yfx, xsd_mod)
  ]
).

/** <module> XSD number generic

Grammar rules that are used by various XSD numeric datatypes.

@author Wouter Beek
@version 2013/07-2013/08, 2013/10, 2014/03-2014/04
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(library(arithmetic)).
:- use_module(math(rational_ext)).

:- op(400, yfx, xsd_div).
:- arithmetic_function(xsd_div/2).

:- op(400, yfx, xsd_mod).
:- arithmetic_function(xsd_mod/2).



%! decimalPtNumeral(-Sign:oneof([-1,1]), -Decimal:float)//
% ~~~{.ebnf}
% decimalPtNumeral ::= ('+' | '-')? unsignedDecimalPtNumeral
% ~~~

decimalPtNumeral(Sign, N) -->
  (sign(Sign) ; {Sign = 1}),
  unsignedDecimalPtNumeral(N1),
  {N is copysign(N1, Sign)}.


%! fracFrag(-Fraction:between(0.0,1.0))//
% ~~~{.ebnf}
% fracFrag ::= digit+
% ~~~

fracFrag(F) -->
  fracFrag(0, F).

fracFrag(I, NewSum) -->
  decimal_digit(_, D),
  {succ(I, NewI)},
  fracFrag(NewI, Sum),
  {NewSum is Sum + D * 10 ** (-1 * NewI)}.
fracFrag(_, 0.0) --> !, [].


%! fractionDigitsCanonicalFragmentMap(?Fraction:rational)//

fractionDigitsCanonicalFragmentMap(F) -->
  {F =:= 0.0}, !,
  [].
fractionDigitsCanonicalFragmentMap(F) -->
  {
    G is F * 10,
    H is floor(G / 1.0),
    NewF is G - 1.0 * floor(G / 1.0)
  },
  decimal_digit(_, H), !,
  fractionDigitsCanonicalFragmentMap(NewF).


%! noDecimalPtCanonicalMap(+Integer:integer)//

noDecimalPtCanonicalMap(I) -->
  {I < 0},
  `-`, !,
  {J is copysign(I, 1)},
  unsignedNoDecimalPtCanonicalMap(J).
noDecimalPtCanonicalMap(I) -->
  unsignedNoDecimalPtCanonicalMap(I).


%! noDecimalPtNumeral(-Sign:oneof([-1,1]), -Integer:integer)//
% ~~~{.ebnf}
% noDecimalPtNumeral ::= ('+' | '-')? unsignedNoDecimalPtNumeral
% ~~~

noDecimalPtNumeral(Sign, N) -->
  (sign(Sign), ! ; {Sign = 1}),
  unsignedNoDecimalPtNumeral(N1),
  {N is copysign(N1, Sign)}.


%! unsignedDecimalPtCanonicalMap(+Decimal:rational)//

unsignedDecimalPtCanonicalMap(F) -->
  {rational_parts(F, F_I, F_F)},
  unsignedNoDecimalPtCanonicalMap(F_I),
  `.`,
  fractionDigitsCanonicalFragmentMap(F_F).


%! unsignedDecimalPtNumeral(-Decimal:float)//
% ~~~{.ebnf}
% unsignedDecimalPtNumeral ::= (unsignedNoDecimalPtNumeral '.' fracFrag?)
%                              | ('.' fracFrag)
% ~~~

unsignedDecimalPtNumeral(N) -->
  unsignedNoDecimalPtNumeral(I),
  `.`,
  (fracFrag(F), {N is I + F} ; {N = I}).
unsignedDecimalPtNumeral(F) -->
  `.`,
  fracFrag(F).


%! unsignedNoDecimalPtCanonicalMap(+Integer:nonneg)//

% For =0= the emitted string should be `0` and not the empty string!
% Check \ref{def:unsignedNoDecimalPtCanonicalMap} in the PraSem document.
unsignedNoDecimalPtCanonicalMap(0) --> !, `0`.
unsignedNoDecimalPtCanonicalMap(F) -->
  unsignedNoDecimalPtCanonicalMap_(F).

% Done!
unsignedNoDecimalPtCanonicalMap_(0) --> !, [].
unsignedNoDecimalPtCanonicalMap_(F) -->
  {
    xsd_number_generic:(G is F xsd_mod 10),
    xsd_number_generic:(H is F xsd_div 10)
  },
  unsignedNoDecimalPtCanonicalMap_(H),
  decimal_digit(_, G).


%! unsignedNoDecimalPtNumeral(-Integer:nonneg)//
% ~~~{.ebnf}
% unsignedNoDecimalPtNumeral ::= digit+
% ~~~

unsignedNoDecimalPtNumeral(N) -->
  unsignedNoDecimalPtNumeral(_, N).

unsignedNoDecimalPtNumeral(NewToEnd, NewN) -->
  decimal_digit(_, D), !,
  unsignedNoDecimalPtNumeral(ToEnd, N),
  {
    NewN is N + D * 10 ** ToEnd,
    succ(ToEnd, NewToEnd)
  }.
unsignedNoDecimalPtNumeral(0, 0) --> [].


xsd_div(X, Y, Z):-
  Z is floor(X / Y).


xsd_mod(X, Y, Z):-
  Z is X - Y * (X xsd_div Y).

