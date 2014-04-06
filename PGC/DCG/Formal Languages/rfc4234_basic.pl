:- module(
  rfc4234,
  [
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'BIT'//0,
    'BIT'//1, % ?Bit:between(0,1)
    '1*BIT'//1, % ?Bits:list(between(0,1))
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'LWSP'//0,
    'VCHAR'//0,
    'VCHAR'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).
:- reexport(
  http(rfc2616_basic),
  [
    '"'//0 as 'DQUOTE',
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//0,
    'DIGIT'//2, % ?Code:code
                % ?Integer:between(0,9)
    'HEX'//0 as 'HEXDIG',
    'HEX'//1 as 'HEXDIG', % ?Integer:between(0,15)
    'HT'//0 as 'HTAB',
    'HT'//1 as 'HTAB',
    'LF'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1
  ]
).

/** <module> RFC 4234 basic rules

DCGs for the basic rules defined in RFC 4234,
 Augmented Backus Naur Form (ABNF).

@author Wouter Beek
@see RFC 4234
@version 2013/07-2013/08, 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).



%! 'ALPHA'// .
%! 'ALPHA'(?Code:code)// .
% The ASCII letters, i.e. =|A-Z / a-z|=.
%
% Hexadecimal character range: =|%x41-5A|= through =|%x61-7A|=.
%
% ~~~{.abnf}
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ~~~
%
% @see The same characters as 'ALPHA'//[0,1] in [rfc2616_basic],
%      but defined differently.
% @see RFC 4234

'ALPHA' -->
  'ALPHA'(_).

'ALPHA'(C) -->
  ascii_letter(C).



%! 'BIT'// .
%! 'BIT'(?Integer:between(0,1))// .
% A binary digit, i.e. `0` or `1`.
%
% ~~~{.abnf}
% BIT = "0" / "1"
% ~~~
%
% @see RFC 4234

'BIT' -->
  'BIT'(_).

'BIT'(D) -->
  binary_digit(_, D).


%! '1*BIT'(?Bits:list(between(0,1)))//

'1*BIT'([H|T]) -->
  'BIT'(H),
  '1*BIT'(T).
'1*BIT'([H]) -->
  'BIT'(H).


%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% ~~~{.abnf}
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ~~~
%
% @see Different from 'CHAR'//[0,1] in [rfc2616_basic],
%      which includes the NULL character.
% @see RFC 4234

'CHAR' -->
  'CHAR'(_).

'CHAR'(C) -->
  [C],
  {between(1, 127, C)}.



%! 'DQUOTE'// .
%! 'DQUOTE'(?Code:code)// .
%
% ~~~{.abnf}
% DQUOTE = %x22   ; " (Double Quote)
% ~~~
%
% @see Same as '"'//0 in [rfc2616_basic]
% @see RFC 4234



%! 'HEXDIG'// .
%! 'HEXDIG'(?Integer:between(0,15))// .
% Hexadecimal digits.
%
% ~~~{.abnf}
% HEXDIG =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ~~~
%
% @see Same as 'HEX'//[0,1] in [rfc2616_basic]
% @see RFC 4234





%! 'HTAB'// .
% Horizontal tab.
%
% ~~~{.abnf}
% HTAB = %x09   ; horizontal tab
% ~~~



%! 'LWSP'// .
% Linear white space.
%
% ~~~{.abnf}
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ~~~
%
% @see RFC 2616 defines this differently.
% @see RFC 4234

'LWSP' -->
  'WSP',
  'LWSP'.
'LWSP' -->
  'CRLF',
  'WSP'.
'LWSP' -->
  [].



%! 'VCHAR'// .
%! 'VCHAR'(?Code:code)// .
% Visible characters.
%
% ~~~{.abnf}
% VCHAR = %x21-7E   ; visible (printing) characters
% ~~~
%
% @see RFC 4324

'VCHAR' -->
  'VCHAR'(_).

'VCHAR'(C) -->
  ascii_graphic(C).



%! 'WSP'// .
% Whitesapace, defined as sequences of space and horizontal tab.
%
% ~~~{.abnf}
% WSP = SP / HTAB   ; white space
% ~~~
%
% @see RFC 4234

'WSP' -->
  'WSP'(_).

'WSP'(C) -->
  'SP'(C).
'WSP'(C) -->
  'HTAB'(C).

