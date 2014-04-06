:- module(bnf, []).

/** <module> BNF

Support for various Backus-Naur Forms.

# ABNF

Defined by RFC.

# EBNF

Defined by W3C.

Used by:
* XML

Symbol definition:
~~~
symbol ::= expression
~~~

Symbols are written with an initial capital letter if they are the start
symbol of a regular language, otherwise with an initial lowercase letter.
Literal strings are quoted.

Simple expressions used in the right-hand side of symbol definitions:
  * =|#xN|=
    Where N is a hexadecimal integer, the expression matches the character
    whose number (code point) in ISO/IEC 10646 is N. The number of leading
    zeros in the #xN form is insignificant.
  * =|[a-zA-Z], [#xN-#xN]|=
    Matches any xml_char// with a value in the range(s) indicated
    (inclusive).
  * =|[abc], [#xN#xN#xN]|=
    Matches any xml_char// with a value among the characters enumerated.
    Enumerations and ranges can be mixed in one set of brackets.
  * =|[^a-z], [^#xN-#xN]|=
    Matches any xml_char// with a value outside the range indicated.
  * =|[^abc], [^#xN#xN#xN]|=
    Matches any xml_char// with a value not among the characters given.
    Enumerations and ranges of forbidden values can be mixed in one set
     of brackets.
  * =|"string"|=
    Matches a literal string matching that given inside the double quotes.
  * =|'string'|=
    Matches a literal string matching that given inside the single quotes.

Complex expressions used in the right-hand side of symbol definitions,
based on simple expressions =A= and =B=.
  * =|(expression)|=
    The expression is treated as a unit and may be combined as described
    in this list.
  * =|A?|=
    Matches =A= or nothing; optional =A=.
  * =|A B|=
    Matches =A= followed by =B=. This operator has higher precedence than
    alternation; thus =|A B | C D|= is identical to =|(A B) | (C D)|=.
  * =|A | B|=
    Matches =A= or =B=; alternation.
  * =|A - B|=
    Matches any string that matches =A= but does not match =B=.
  * =|A+|=
    Matches one or more occurrences of =A=.
    Concatenation has higher precedence than alternation;
    thus =|A+ | B+|= is identical to =|(A+) | (B+)|=.
  * =|A*|=
    Matches zero or more occurrences of =A=.
    Concatenation has higher precedence than alternation;
    thus =|A* | B*|= is identical to =|(A*) | (B*)|=.

Other notations used in the productions:
  * Multi-line comments in standard C style.
  * =|[ wfc: ... ]|=
    Well-formedness constraint; this identifies by name a constraint on
    well-formed documents associated with a production.
  * =|[ vc: ... ]|=
    Validity constraint; this identifies by name a constraint on valid
    documents associated with a production.

--

@author Wouter Beek
@see http://www.w3.org/TR/xml11/#sec-notation
@version 2013/08
*/




