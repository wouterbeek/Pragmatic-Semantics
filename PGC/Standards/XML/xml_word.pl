:- module(
  xml_word,
  [
    xml_empty_tag//1, % ?Name:atom
    xml_end_tag//1, % ?Name:atom
    xml_start_tag//1 % ?Name:atom
  ]
).

/** <module> XML word

@author Wouter Beek
@see http://www.w3.org/TR/REC-xml/
@version 2014/03
*/

:- use_module(xml(xml_datatypes)).



%! xml_empty_tag(?Name:atom)// .
% An empty XML element.
% ~~~
% [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%      [WFC: Unique Att Spec]
% ~~~
%
% @tbd Add support for attributes.

xml_empty_tag(Name) -->
  `<`,
  xml_name(Name),
  (`` ; xml_space),
  `/>`, !.


%! xml_end_tag(Name)// .
% The end tag of an XML element.
%
% ~~~
% [42] ETag ::= '</' Name S? '>'
% ~~~

xml_end_tag(Name) -->
  `</`,
  xml_name(Name),
  (`` ; xml_space),
  `>`, !.


% xml_start_tag(?Name:atom)// .
% The start tag of an XML element.
%
% ~~~
% [40] STag ::= '<' Name (S Attribute)* S? '>'   [WFC: Unique Att Spec]
% ~~~
%
% @tbd Add support for attributes.

xml_start_tag(Name) -->
  `<`,
  xml_name(Name),
  (`` ; xml_space),
  `>`, !.

