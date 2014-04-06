:- module(
  xsd_string,
  [
    xsd_string_canonical_map//1, % +String:atom
    xsd_string_lexical_map//1 % -String:atom
  ]
).

/** <module> XSD string datatype

The *=string=* datatype represents character strings in XML.

#### Value space

The value space of =string= is the set of finite-length sequences
of characters that match the xml_char//1 production from either XML 1.0
or XML 1.1.

A character is an atomic unit of communication; it is not further
specified except to note that every character has a corresponding
Universal Character Set code point, which is an integer.

Many human languages have writing systems that require child elements
for control of aspects such as bidirectional formating or ruby annotation
Thus, string, as a simple type that can contain only characters but not
child elements, is often not suitable for representing text.
In such situations, a complex type that allows mixed content should be
considered.

The fact that this specification does not specify an order-relation
for strings does not preclude other applications from treating strings
as being ordered.

#### Lexical mapping

The lexical space of =string= is the set of finite-length sequences of
zero or more characters (as defined in [XML]) that match the xml_char//
production from XML 1.0 or XML 1.1.

~~~{.ebnf}
stringRep ::= Char*
~~~

The lexical mapping for =string= is xsd_string_lexical_map/2.
The canonical mapping·for =string= is xsd_string_canonical_map/2.

--

@author Wouter Beek
@version 2013/08, 2014/03
*/

:- use_remote_module(xml(xml_datatypes)).



%! xsd_string_canonical_map(+String:atom)// is det.
% The function is the identity function on the domain.
%
% @arg String An XML string value; a Prolog atom.
% @arg StringRep The canonical XML string serialization. A list of codes.

xsd_string_canonical_map(String) -->
  stringRep(String).


%! xsd_string_lexical_map(-String:atom) is det.
% Maps a literal matching the stringRep//1 production to a string value.
%
% The function is the identity function on the domain.
%
% @arg Literal A literal matching stringRep//1.
% @arg String An XML string value; a Prolog atom.

xsd_string_lexical_map(String) -->
  stringRep(String).


%! stringRep(?String:atom)// is det.

stringRep(String) -->
  {var(String)}, !,
  xml_chars_11(XML_Characters), !,
  {atom_codes(String, XML_Characters)}.
stringRep(String) -->
  {atom_codes(String, XML_Characters)},
  xml_chars_11(XML_Characters), !.

