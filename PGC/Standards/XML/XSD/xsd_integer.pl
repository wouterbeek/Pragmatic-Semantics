:- module(
  xsd_integer,
  [
    xsd_integer_canonical_map//1, % +Integer:integer
    xsd_integer_lexical_map//1 % -Integer:integer
  ]
).

/** <module> XSD integer datatype

*=integer=* is derived from decimal by fixing the value of =fractionDigits=
to be 0 and disallowing the trailing decimal point. This results in the
standard mathematical concept of the integer numbers. The value space of
=integer= is the infinite set =|{...,-2,-1,0,1,2,...}|=. The base type of
integer is decimal.

### Lexical representation

=integer= has a lexical representation consisting of a finite-length sequence
of one or more decimal digits (=|#x30|=-=|#x39|=) with an optional leading
sign. If the sign is omitted, "=|+|=" is assumed.

Examples: =|-1|=, =0=, =12678967543233=, =|+100000|=.

### Canonical representation

The canonical representation for integer is defined by prohibiting certain
options from the Lexical representation. Specifically, the preceding optional
"=|+|=" sign is prohibited and leading zeroes are prohibited.

--

@author Wouter Beek
@version 2013/08-2013/09, 2014/03
*/

:- use_remote_module(xsd(xsd_decimal)).
:- use_remote_module(xsd(xsd_number_generic)).



%! xsd_integer_canonical_map(+Integer:integer)// is det.

xsd_integer_canonical_map(Integer) -->
  xsd_decimal_canonical_map(Integer).


%! xsd_integer_lexical_map(-Integer:integer)// is det.

xsd_integer_lexical_map(Integer) -->
  noDecimalPtNumeral(_, Integer).

