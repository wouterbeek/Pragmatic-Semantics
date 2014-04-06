:- module(
  xml_datatypes,
  [
    xml_boolean//2, % -Tree:compound
                    % ?Value:boolean
    xml_char_10//1, % ?Char:code
    xml_char_11//1, % ?Char:code
    xml_chars_10//1, % ?Chars:list(code)
    xml_chars_11//1, % ?Chars:list(code)
    xml_name//1, % ?Name:atom
    xml_namespaced_name//2, % :DCG_Namespace
                            % :DCG_Name
    xml_space//0,
    xml_space//1, % ?Code:code
    xml_yes_no//2 % -Tree:compound
                  % ?Boolean:boolean
  ]
).

/** <module> XML_DATATYPES

DCG rules for XML datatypes.

@author Wouter Beek
@version 2013/07-2013/08, 2014/02-2014/04
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_unicode)).

:- meta_predicate(xml_namespaced_name(//,//,?,?)).



xml_boolean(xml_boolean(false), false) --> "false".
xml_boolean(xml_boolean(true),  true) --> "true".


%! xml_char_10(?Char:between(9,1114111))//
% An **XML Character** is an atomic unit of text specified by ISO/IEC 10646.
%
% ~~~{.bnf}
% Char ::= #x9               | // Horizontal tab
%          #xA               | // Line feed
%          #xD               | // Carriage return
%          [#x20-#xD7FF]     | // Space, punctuation, numbers, letters
%          [#xE000-#xFFFD]   |
%          [#x10000-#x10FFFF]
% ~~~
%
% Avoid comapatibility characters [Unicode, section 2.3].
% Avoid the following characters (control characters,
% permanently undefined Unicode characters):
%
% ~~~{.txt}
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ~~~
%
% @see XML 1.0 Fifth Edition
% @tbd Add Unicode support and make sure the right character ranges
%      are selected.

% Horizontal tab =|#x9|=
xml_char_10(X) -->
  horizontal_tab(X).
% Line feed =|#xA|=
xml_char_10(X) -->
  line_feed(X).
% Carriage return =|#xD|=
xml_char_10(X) -->
  carriage_return(X).
% Space, punctuation, numbers, letters
% =|#x20-#xD7FF|=
xml_char_10(X) -->
  between(20, 55295, X).
% =|#xE000-#xFFFD|=
xml_char_10(X) -->
  between(57344, 65533, X).
% =|#x10000-#x10FFFF|=
xml_char_10(X) -->
  between(65536, 1114111, X).


%! xml_char_11(?Code:between(1,1114111))// is nondet.
% ~~~{.bnf}
% [2] Char ::= [#x1-#xD7FF]
%            | [#xE000-#xFFFD]
%            | [#x10000-#x10FFFF]
%            /* any Unicode character, excluding the surrogate blocks,
%               FFFE, and FFFF. */
% ~~~
%
% @see XML 1.1 Second Edition

% =|#x1-#xD7FF|=
xml_char_11(X) -->
  between(1, 55295, X).
% =|#xE000-#xFFFD|=
xml_char_11(X) -->
  between(57344, 65533, X).
% =|#x10000-#x10FFFF|=
xml_char_11(X) -->
  between(65536, 1114111, X).


xml_chars_10([H|T]) -->
  xml_char_10(H),
  xml_chars_10(T).
xml_chars_10([]) --> [].


xml_chars_11([H|T]) -->
  xml_char_11(H),
  xml_chars_11(T).
xml_chars_11([]) --> [].


%! xml_name(?Name:atom)//
% A **XML Name** is an Nmtoken with a restricted set of initial characters.
%
% Disallowed initial characters for names include digits, diacritics,
% the full stop and the hyphen.
%
% ~~~{.bnf}
% Name ::= NameStartChar (NameChar)*
% ~~~
%
% ## Reserved names
%
% Names beginning with `(x,m,l)` are reserved for standardization in this
% or future versions of this specification.
%
% ## XML Namespaces
%
% The Namespaces in XML Recommendation assigns a meaning to names containing
% colon characters. Therefore, authors should not use the colon in XML names
% except for namespace purposes, but XML processors must accept the colon as
% a name character.
%
% @see http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name

xml_name(Name) -->
  {nonvar(Name)}, !,
  {atom_codes(Name, Codes)},
  xml_name_(Codes).
xml_name(Name) -->
  xml_name_(Codes),
  {atom_codes(Name, Codes)}.

xml_name_([H1,H2,H3|T]) -->
  xml_name_start_char(H1),
  xml_name_chars([H2,H3|T]),
  {\+ phrase((x,m,l), [H1,H2,H3])}.
xml_name_([H1,H2]) -->
  xml_name_start_char(H1),
  xml_name_char(H2).
xml_name_([H]) -->
  xml_name_start_char(H).


%! xml_name_char(?Char:code)//
% ~~~{.bnf}
% NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
%              [#x203F-#x2040]
% ~~~

xml_name_char(C) --> xml_name_start_char(C).
xml_name_char(C) --> hyphen_minus(C).
xml_name_char(C) --> dot(C).
xml_name_char(C) --> decimal_digit(C).
% #xB7
xml_name_char(C) --> middle_dot(C).
% #x0300-#x036F
xml_name_char(C) --> [C], {between(768, 879, C)}.
% #x203F
xml_name_char(C) --> undertie(C).
% #x2040
xml_name_char(C) --> character_tie(C).

xml_name_chars([H|T]) -->
  xml_name_char(H),
  xml_name_chars(T).
xml_name_chars([]) --> [].


%! xml_name_start_char(?Code:code)//
% ~~~{.bnf}
% NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] |
%                   [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
%                   [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
%                   [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
%                   [#x10000-#xEFFFF]
% ~~~

xml_name_start_char(C) --> colon(C).
xml_name_start_char(C) --> ascii_letter(C).
xml_name_start_char(C) --> underscore(C).
% #xC0-#xD6
xml_name_start_char(C) --> between(192, 214, C).
% #xD8-#xF6
xml_name_start_char(C) --> between(216, 246, C).
% #xF8-#x2FF
xml_name_start_char(C) --> between(248, 767, C).
% #x370-#x37D
xml_name_start_char(C) --> between(880, 893, C).
% #x37F-#x1FFF
xml_name_start_char(C) --> between(895, 8191, C).
% #x200C-#x200D
xml_name_start_char(C) --> zero_width_non_joiner(C).
xml_name_start_char(C) --> zero_width_joiner(C).
% #x2070-#x218F
xml_name_start_char(C) --> between(8304, 8591, C).
% #x2C00-#x2FEF
xml_name_start_char(C) --> between(11264, 12271, C).
% #x3001-#xD7FF
xml_name_start_char(C) --> between(12289, 55295, C).
% #xF900-#xFDCF
xml_name_start_char(C) --> between(63744, 64975, C).
% #xFDF0-#xFFFD
xml_name_start_char(C) --> between(65008, 65533, C).
% #x10000-#xEFFFF
xml_name_start_char(C) --> between(65536, 983039, C).


%! xml_namespaced_name(:DCG_Namespace, :DCG_Name)//

xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  {phrase(DCG_Namespace, [])},
  DCG_Name.
xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  DCG_Namespace,
  colon,
  DCG_Name.


%! xml_restricted_char(?Char:code)//
% ~~~{.bnf}
% RestrictedChar ::= [#x1-#x8] |
%                    [#xB-#xC] |
%                    [#xE-#x1F] |
%                    [#x7F-#x84] |
%                    [#x86-#x9F]
% ~~~
%
% @see XML 1.1 Second Edition

xml_restricted_char(C) -->
  xml_char_11(C),
  % Not a start of heading, start of text, end of text, end of transmission,
  % enquiry, positive_acknowledgement, bell, backspace.
  {\+ between(1, 8, C)},
  % Not a vertical tab, form feed.
  {\+ between(11, 12, C)},
  % Not a shift out, shift in, data link escape, device control (1, 2, 3, 4),
  % negative acknowledgement, synchronous idle, end of transmission block,
  % cancel, end of medium, substitute, escape, file separator,
  % group separator, record separator, unit separator.
  {\+ between(14, 31, C)},
  % Not delete, ...
  {\+ between(127, 132, C)},
  % Not ..
  {\+ between(134, 159, C)}.


%! xml_space// .
%! xml_space(?Code:code)// .
% White space.
%
% ~~~{.bnf}
% S ::= (#x20 | #x9 | #xD | #xA)+   // Any consecutive number of spaces,
%                                   // carriage returns, line feeds, and
%                                   // horizontal tabs.
% ~~~
%
% The presence of carriage_return// in the above production is maintained
% purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either removed
% or replaced by line_feed// (i.e., `#xA`) characters before any other
% processing is done.

xml_space -->
  xml_space(_).

xml_space(C) --> carriage_return(C).
xml_space(C) --> horizontal_tab(C).
xml_space(C) --> line_feed(C).
xml_space(C) --> space(C).

xml_yes_no(xml_yes_no(no), false) --> "no".
xml_yes_no(xml_yes_no(yes), true) --> "yes".

