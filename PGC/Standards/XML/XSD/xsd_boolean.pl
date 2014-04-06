:- module(
  xsd_boolean,
  [
    xsd_boolean_canonical_map//1, % +Boolean:boolean
    xsd_boolean_lexical_map//1 % -Boolean:boolean
  ]
).

/** <module> XSD boolean

*Boolean* represents the values of two-valued logic.

### Value space

The value space of two-valued logic:  $\{ \text{true}, \text{false} \}$.

### Lexical representation

~~~{.ebnf}
xsd_boolean_map ::= 'true' | 'false' | '1' | '0'
~~~

--

@author Wouter Beek
@version 2013/08, 2014/03
*/



%! xsd_boolean_canonical_map(+Boolean:boolean)// is det.
% Maps a boolean value to a xsd_boolean_map//1.
%
% Returns `true` when `Boolean` is true, and
% returns `false` otherwise (i.e., when `Boolean` is false).
%
% @arg Boolean A boolean value.

xsd_boolean_canonical_map(Boolean) -->
  xsd_boolean_map(Boolean), !.


%! xsd_boolean_lexical_map(-Boolean:boolean)// is det.
% Maps a literal matching the xsd_boolean_map//1 production
% to a boolean value.
%
% Returns true when =Lexical= is `true` or `1` , and
% returns false otherwise (i.e., when `Lexical` is `false` or `0`).
%
% @arg Boolean A boolean value.

xsd_boolean_lexical_map(Boolean) -->
  xsd_boolean_map(Boolean).


%! xsd_boolean_map(?Boolean:boolean)// .

xsd_boolean_map(true) -->
  `true`.
xsd_boolean_map(true) -->
  `1`.
xsd_boolean_map(false) -->
  `false`.
xsd_boolean_map(false) -->
  `0`.

