:- module(
  xsd_decimal,
  [
    xsd_decimal_canonical_map//1, % +Decimal:or([atom,integer,rational])
    xsd_decimal_lexical_map//1 % -Decimal:number
  ]
).

/** <module> XSD decimal datatype

*=decimal=* represents a subset of the real numbers, which can be represented
by decimal numerals.

#### Prolog compound term

~~~{.pl}
decimal(I:integer,N:integer)
~~~

#### Value space

The value space of decimal is the set of numbers that
can be obtained by dividing an integer by a non-negative power of ten,
i.e., expressible as $\frac{i}{10n} where $i$ and $n$ are integers and
$n \geq 0$.

Precision is not reflected in this value space; the number 2.0
is not distinct from the number 2.00. The order relation on decimal is the
order relation on real numbers, restricted to this subset.

#### Lexical space

Decimal has a lexical representation consisting of a non-empty finite-length
sequence of decimal digits (#x30–#x39) separated by a period as a decimal
indicator. An optional leading sign is allowed. If the sign is omitted,
"+" is assumed. Leading and trailing zeroes are optional. If the fractional
part is zero, the period and following zero(es) can be omitted.

Examples:
  * =|-1.23|=
  * =|12678967.543233|=
  * =|+100000.00|=
  * =|210|=

~~~{.ebnf}
xsd_decimal_lexical_map ::= decimalPtNumeral | noDecimalPtNumeral
decimalPtNumeral ::= ('+' | '-')? unsignedDecimalPtNumeral
digit ::= [0-9]
fracFrag ::= digit+
noDecimalPtNumeral ::= ('+' | '-')? unsignedNoDecimalPtNumeral
unsignedDecimalPtNumeral ::=
    (unsignedNoDecimalPtNumeral '.' fracFrag?) | ('.' fracFrag)
unsignedNoDecimalPtNumeral ::= digit+
~~~

~~~{.re}
(\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)
~~~

#### Canonical representation

For integers, the decimal point and fractional part are prohibited.
For other values, the preceding optional "+" sign is prohibited.
The decimal point is required.
In all cases, leading and trailing zeroes are prohibited subject to the
following: there must be at least one digit to the right and to the left
of the decimal point which may be a zero.

--

@author Wouter Beek
@tbd Have a look at the bidirectional implementation.
@version 2013/07-2013/08, 2013/10, 2014/03
*/

:- use_module(xsd(xsd_number_generic)).


% CANONICAL MAP %

%! xsd_decimal_canonical_map(+Decimal:oneof([integer,rational]))// is det.

xsd_decimal_canonical_map(I) -->
  {integer(I)}, !,
  noDecimalPtCanonicalMap(I).
xsd_decimal_canonical_map(F) -->
  {rational(F)}, !,
  decimalPtCanonicalMap(F).
%%%%xsd_decimal_canonical_map(F1) -->
%%%%  {F2 is rationalize(F1)},
%%%%  xsd_decimal_canonical_map(F2).


%! decimalPtCanonicalMap(+Decimal:rational)//

decimalPtCanonicalMap(F) -->
  ({F < 0} ->  `-` ; ``),
  {G is copysign(F, 1)},
  unsignedDecimalPtCanonicalMap(G).



% LEXICAL MAP %

%! xsd_decimal_lexical_map(-Decimal:float)// is det.
% ~~~{.ebnf}
% decimalRep ::= decimalPtNumeral | noDecimalPtNumeral
% ~~~

xsd_decimal_lexical_map(N) -->
  decimalPtNumeral(_Sign, N).
xsd_decimal_lexical_map(N) -->
  noDecimalPtNumeral(_Sign, N).





/* BIDIRECTIONAL IMPLEMENTATION
:- use_module(dcg(parse_tree)).
:- use_module(library(lists)).
:- use_module(math(radix)).

decimalLexicalMap1(Lexical, D):-
  phrase(decimalLexicalRep1(D), Lexical).

decimalLexicalMap2(Lexical, D):-
  phrase(decimalLexicalRep2(_Tree, D), Lexical).

%! decimalLexicalRep1(D)//
% Processes a decimal value that is internally represented as
% a SWI-Prolog float.
%
% @see For a full DCG alternative, that does not use number_codes/2
%      nor format/3, see decimalLexicalRep2//2.

decimalLexicalRep1(D) -->
  {var(D)}, !,
  (sign(H1) -> {L1 = [H1|T1]} ; {L1 = T1}),
  dcg_multi1(decimal_digit, 1-_, T1),
  (  dot(H2),
     dcg_multi1(decimal_digit, T2)
  -> {L2 = [H2|T2]}
  ;  {L2 = []}
  ),
  {append(L1, L2, L), number_codes(D, L)}.
decimalLexicalRep1(D, H, T):-
  number(D), !,
  format(codes(H,T), '~w', [D]).

%! decimalLexicalRep2(-Tree:compound, ?Decimal:compound)//
% Processes a decimal value using the compound term notation for a decimal.
% Also returns the parse tree.

decimalLexicalRep2(T0, decimal(I,N)) -->
  {var(I), var(N)}, !,
  (sign(T1, Sign) ; {Sign = 1}),
  dcg_multi1(decimal_digit, 1-_, I1s),
  (
    dot, {T3 = '.'},
    dcg_multi1(decimal_digit, I2s)
  ;
    {I2s  = []}
  ),
  {
    append(I1s, I2s, Is),
    digits_to_decimal(Is, I_),
    I is copysign(I_, Sign),
    length(I2s, N),
    parse_tree(xsd_decimal_lexical_map, [T1,I1s,T3,I2s], T0)
  }.
decimalLexicalRep2(T0, decimal(I,N)) -->
  {
    integer(I), integer(N), !,
    length(I2s, N),
    (I < 0 -> Sign = -1 ; Sign = 1),
    I_ is copysign(I, Sign),
    decimal_to_digits(I_, Is),
    append(I1s, I2s, Is)
  },
  (sign(T1, Sign) ; {Sign = 1}),
  dcg_multi1(decimal_digit, 1-_, I1s),
  (
    dot,
    dcg_multi1(decimal_digit, I2s)
  ->
    {T3 = '.'}
  ;
    {I2s  = []}
  ),
  {parse_tree(xsd_decimal_lexical_map, [T1,I1s,T3,I2s], T0)}.
*/

