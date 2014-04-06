:- module(
  rfc2616_basic,
  [
    '"'//0,
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1,
    'DIGIT'//0,
    'DIGIT'//2, % ?Code:code
                % ?Integer:between(0,9)
    'HEX'//0,
    'HEX'//1, % ?HexadecimalDigit:between(0,15)
    'HT'//0,
    'HT'//1, % ?Code:code
    'LF'//0,
    'LOALPHA'//0,
    'LOALPHA'//1, % ?Code:code
    'LWS'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'TEXT'//0,
    'TEXT'//1, % ?Code:code
    'UPALPHA'//0,
    'UPALPHA'//1 % ?Code:code
  ]
).

/** <module> RFC 2616 ABNF rules

DCGs for the basic rules defined in RFC 2616 (HTTP 1.1).

# RFC 2616

The following basic rules are specified in RFC 2616 (HTTP 1.1).

## Basic rules that are conforming with RFC 4234 (ABNF)

~~~{.abnf}
<">     = <US-ASCII double-quote mark (34)>
ALPHA   = UPALPHA | LOALPHA
CHAR    = <any US-ASCII character (octets 0 - 127)>
CR      = <US-ASCII CR, carriage return (13)>
CRLF    = CR LF
CTL     = <any US-ASCII control character
          (octets 0 - 31) and DEL (127)>
DIGIT   = <any US-ASCII digit "0".."9">
HT      = <US-ASCII HT, horizontal-tab (9)>
LF      = <US-ASCII LF, linefeed (10)>
LOALPHA = <any US-ASCII lowercase letter "a".."z">
LWS     = [CRLF] 1*(SP|HT)
OCTET   = <any 8-bit sequence of data>
SP      = <US-ASCII SP, space (32)>
TEXT    = <any OCTET except CTLs, but including LWS>
UPALPHA = <any US-ASCII uppercase letter "A".."Z">
~~~

--

@author Wouter Beek
@see RFC 2616
@see US-ASCII is defined in ANSI X3.4-1986
@version 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_multi)).



%! '"'// .
% US-ASCII double-quote mark.
%
% ~~~{.abnf}
% <"> = <US-ASCII double-quote mark (34)>
% ~~~
%
% @see RFC 2616

'"' -->
  double_quote(_).



%! 'ALPHA'// .
%! 'ALPHA'(?Code:code)// .
% Alphabetic character.
%
% ~~~{.abnf}
% ALPHA = UPALPHA | LOALPHA
% ~~~
%
% @see RFC 2616

'ALPHA' -->
  'UPALPHA'.
'ALPHA' -->
  'LOALPHA'.

'ALPHA'(C) -->
  'UPALPHA'(C).
'ALPHA'(C) -->
  'LOALPHA'(C).



%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% US-ASCII character (including NULL).
%
% ~~~{.abnf}
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ~~~
%
% @see RFC 2616

'CHAR' -->
  'CHAR'(_).

'CHAR'(C) -->
  ascii(C).



%! 'CR'// .
% The carriage return.
%
% ~~~{.abnf}
% CR = %x0D   ; carriage return
% ~~~
%
% @see RFC 2616

'CR' -->
  carriage_return.



%! 'CRLF'// .
% Internet standard newline.
%
% ~~~{.abnf}
% CRLF = CR LF   ; Internet standard newline
% ~~~
%
% # RFC 2616
%
% ## Syntax
%
% The end-of-line marker within an entity-body is defined by
%  its associated media type, as described in section 3.7.
%
% A `CRLF` is allowed in the definition of `TEXT` only as part of a header
%  field continuation.
% It is expected that the folding `LWS` will be replaced with a single `SP`
%  before interpretation of the `TEXT` value.
%
% ## Semantics
%
% HTTP/1.1 defines the sequence `CR LF` as the end-of-line marker for
%  all protocol elements except the entity-body (see appendix 19.3 for
%  tolerant applications).
%
% --
%
% @see RFC 2616

'CRLF' -->
  'CR',
  'LF'.



%! 'CTL'// .
%! 'CTL'(?Code:code)// .
% Control character.
%
% ~~~{.abnf}
% CTL = %x00-1F / %x7F   ; controls
% ~~~
%
% @see RFC 2616

'CTL' -->
  'CTL'(_).

'CTL'(C) -->
  control(C).



%! 'DIGIT'// .
%! 'DIGIT'(?Code:code, ?Integer:between(0,9))// .
% Decimal digit.
%
% ~~~{.abnf}
% DIGIT = %x30-39   ; 0-9
% ~~~
%
% @see RFC 2616

'DIGIT' -->
  'DIGIT'(_, _).

'DIGIT'(C, D) -->
  decimal_digit(C, D).




%! 'HEX'// .
%! 'HEX'(?Integer:between(0.15))// .
% Hexadecimal digit.
%
% # RFC 2616
%
% Hexadecimal numeric characters are used in several protocol elements.
%
% ~~~{.abnf}
% HEX = "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f" | DIGIT
% ~~~
%
% @see RFC 2616

'HEX' -->
  'HEX'(_).

'HEX'(D) -->
  hexadecimal_digit(_, D).



%! 'HT'// .
%! 'HT'(?Code:code)// .
% The horizontal tab.
%
% ~~~{.abnf}
% HT = <US-ASCII HT, horizontal-tab (9)>
% ~~~
%
% @see RFC 2616

'HT' -->
  'HT'(_).

'HT'(C) -->
  horizontal_tab(C).



%! 'LF'// .
% The linefeed.
%
% ~~~{.abnf}
% LF = %x0A   ; linefeed
% ~~~
%
% @RFC 2616

'LF' -->
  line_feed.



%! 'LOALPHA'// .
%! 'LOALPHA'(?Code:code)// .
% US-ASCII lowercase letter.
%
% ~~~{.abnf}
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ~~~
%
% @see RFC 2616

'LOALPHA' -->
  'LOALPHA'(_).

'LOALPHA'(C) -->
  ascii_letter_lowercase(C).



%! 'LWS'// .
% Linear white space.
%
% # RFC 2616
%
% ## Syntax
%
% ~~~{.abnf}
% LWS = [CRLF] 1*(SP|HT)
% ~~~
%
% HTTP/1.1 header field values can be folded onto multiple lines if
%  the continuation line begins with a space or horizontal tab.
%
% ## Semantics
%
% All linear white space, including folding, has the same semantics as `SP`.
%
% ## Pragmatics
%
% A recipient MAY replace any linear white space with a single `SP`
%  before interpreting the field value or forwarding the message downstream.
%
% --
%
% @see RFC 2616

'LWS' -->
  dcg_multi('CRLF', 0-1),
  dcg_multi('SP_or_HT', 1-_).

'SP_or_HT' -->
  'SP'.
'SP_or_HT' -->
  'HT'.


%! 'OCTET'// .
%! 'OCTET'(?Code:code)// .
% An octect, i.e. 8 bits of data.
%
% ~~~{.abnf}
% OCTET = %x00-FF   ; 8 bits of data
% ~~~
%
% @see RFC 2616

'OCTET' -->
  'OCTET'(_).

'OCTET'(C) -->
  [C],
  {between(0, 255, C)}.



%! 'SP'// .
%! 'SP'(?Code:code)// .
% The space.
%
% ~~~{.abnf}
% SP = %x20
% ~~~
%
% @see RFC 2612

'SP' -->
  'SP'(_).

'SP'(C) -->
  space(C).



%! 'TEXT'// .
%! 'TEXT'(?Code:code)// .
%
% # RFC 2616
%
% ## Syntax
%
% ~~~{.abnf}
% TEXT = <any OCTET except CTLs, but including LWS>
% ~~~
%
% The `TEXT` rule is only used for descriptive field contents and
%  values that are not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
%  ISO-8859-1 only when encoded according to the rules of RFC 2047.
%
% @see ISO-8859-1
% @see RFC 2047
%
% --
%
% @see RFC 2616

'TEXT' -->
  'TEXT'(_).

'TEXT'(C) -->
  'OCTET'(C),
  {\+ code_type(C, cntrl)}.



%! 'UPALPHA'// .
%! 'UPALPHA'(?Code:code)// .
% US-ASCII uppercase letter.
%
% ~~~{.abnf}
% UPALPHA = <any US-ASCII uppercase letter "A".."Z">
% ~~~
%
% @see RFC 2616

'UPALPHA' -->
  'UPALPHA'(_).

'UPALPHA'(C) -->
  ascii_letter_uppercase(C).

