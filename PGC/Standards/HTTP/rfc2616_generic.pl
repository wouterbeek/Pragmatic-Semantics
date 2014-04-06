:- module(
  rfc2616_generic,
  [
    charset//2, % -ParseTree:compound
                % ?Charset:atom
    comment//2, % -ParseTree:compound
                % ?Codes:list(code)
    parameter//2, % -ParseTree:compound
                  % ?Parameter:pair(atom,atom)
    'quoted-string'//1, % ?Codes:list(code)
    separator//0,
    token//1 % ?Token:atom
  ]
).

/** <module> HTTP basic rules

Some basic DCG rules that are too specific to be reused outside of
  the HTTP specification, but that are too generic to be included in
  the main DCGs.

@author Wouter Beek
@see RFC 2616
@see Redundant `LWS`
@see Case sensitivity
@see Backwards compatibility
@version 2013/12
*/

:- use_remote_module(generics(codes_ext)). % Used in dcg_multi1//4.
:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(http(rfc2616_basic)).



%! attribute(-ParseTree:compound, ?Attribute:atom)//
% ~~~{.abnf}
% attribute = token
% ~~~

attribute(attribute(Attribute), Attribute) -->
  token(Attribute).



%! charset(-ParseTree:compound, ?Charset:atom)//
% HTTP character sets are identified by case-insensitive tokens.
%
% # Syntax
%
% HTTP character sets are identified by case-insensitive tokens.
%
% ~~~{.abnf}
% charset = token
% ~~~
%
% # Semantics
%
% The term **character set** is used to refer to a method used with
%  one or more tables to convert a sequence of octets into a sequence of
%  characters.
%
% Note that unconditional conversion in the other direction is not required,
%  in that not all characters may be available in a given character set and
%  a character set may provide more than one sequence of octets
%  to represent a particular character.
%
% # Pragmatics
%
% This definition is intended to allow various kinds of character encoding,
%  from simple single-table mappings such as US-ASCII to
%  complex table switching methods such as those that use ISO-2022's
%  techniques.
% However, the definition associated with a MIME character set name
%  MUST fully specify the mapping to be performed from octets to characters.
% In particular, use of external profiling information to determine
%  the exact mapping is not permitted.
%
% ## IANA
%
% The complete set of tokens is defined by the IANA Character Set registry.
%
% Although HTTP allows an arbitrary token to be used as a charset value,
%  any token that has a predefined value within the IANA Character Set
%  registry MUST represent the character set defined by that registry.
% Applications SHOULD limit their use of character sets to those defined
%  by the IANA registry.
%
% @see IANA Character Set registry
%
% ## Compatibility
%
% Some HTTP/1.0 software has interpreted a Content-Type header without
%  charset parameter incorrectly to mean "recipient should guess."
% Senders wishing to defeat this behavior MAY include a charset parameter
%  even when the charset is ISO-8859-1 and SHOULD do so when it is known
%  that it will not confuse the recipient.
%
% Unfortunately, some older HTTP/1.0 clients did not deal properly with
%  an explicit charset parameter. HTTP/1.1 recipients MUST respect the
%  charset label provided by the sender; and those user agents that have
%  a provision to "guess" a charset MUST use the charset from the
%  content-type field if they support that charset, rather than the
%  recipient's preference, when initially displaying a document.
%
% # Terminology
%
% Note: This use of the term "character set" is more commonly referred to
%  as a "character encoding."
% However, since HTTP and MIME share the same registry,
%  it is important that the terminology also be shared.
%
% --
%
% @see Implementors should be aware of IETF character set requirements.

charset(charset(Charset), Charset) -->
  token(Charset).



%! comment(-ParseTree:compound, ?Codes:list(code))//
% Comments can be included in some HTTP header fields by surrounding
%  the comment text with parentheses.
% Comments are only allowed in fields containing `"comment"` as part of
%  their field value definition.
% In all other fields, parentheses are considered part of the field value.
%
% ~~~{.abnf}
% comment = "(" *( ctext | quoted-pair | comment ) ")"
% ~~~

comment(comment(Comm), Cs) -->
  bracketed(dcg_multi1('ctext_or_quoted-pair_or_comment', _-_, Css)),
  {flatten(Css, Cs)},
  {atom_codes(Comm, Cs)}.

'ctext_or_quoted-pair_or_comment'([C]) -->
  ctext(C).
'ctext_or_quoted-pair_or_comment'([C]) -->
  'quoted-pair'(C).
'ctext_or_quoted-pair_or_comment'(Cs) -->
  comment(_, Cs).



%! ctext(?Code:code)//
% Maybe this stands for 'comment text'.
%
% ~~~{.abnf}
% ctext = <any TEXT excluding "(" and ")">
% ~~~
%
% Notice that the round brackets are used to indicate
%  the begin and end of an HTTP comment.
%
% @see RFC 2616

ctext(C) -->
  'TEXT'(C),
  {C \= 40}, % "("
  {C \= 41}. % ")"



%! parameter(-ParseTree:compound, ?AttributeValuePair:kvpair(atom,atom))//
% Parameters are in the form of attribute/value pairs.
% ~~~{.abnf}
% parameter = attribute "=" value
% ~~~

parameter(paramter(T1,T2), Attribute-Value) -->
  attribute(T1, Attribute),
  "=",
  value(T2, Value).



%! qdtext(?Code:code)//
% ~~~{.abnf}
% qdtext = <any TEXT except <">>
% ~~~

qdtext(C) -->
  'TEXT'(C),
  {C \= 34}.



%! 'quoted-pair'(?Code:code)//
% The backslash character MAY be used as a single-character quoting mechanism
%  only within `quoted-string` and `comment` constructs.
%
% ~~~{.abnf}
% quoted-pair = "\" CHAR
% ~~~
%
% @see RFC 2616

'quoted-pair'(C) -->
  "\\",
  'CHAR'(C).



%! 'quoted-string'(?QuotedString:atom)//
% A string of text is parsed as a single word if it is quoted using
%  double-quote marks.
%
% ~~~{.abnf}
% quoted-string = ( <"> *(qdtext | quoted-pair ) <"> )
% ~~~
%
% @see RFC 2616

'quoted-string'(QuotedString) -->
  quoted(dcg_multi1('qdtex_or_quoted-pair', Cs)),
  {atom_codes(QuotedString, Cs)}.

'qdtex_or_quoted-pair'(C) -->
  qdtext(C).
'qdtex_or_quoted'(C) -->
  'quoted-pair'(C).



%! separator//
% Many HTTP/1.1 header field values consist of words separated by `LWS`
%  or special characters.
% These special characters MUST be in a `quoted-string`
%  to be used within a parameter value.
%
% ~~~{.abnf}
% separators = "(" | ")" | "<" | ">" | "@"
%            | "," | ";" | ":" | "\" | <">
%            | "/" | "[" | "]" | "?" | "="
%            | "{" | "}" | SP | HT
% ~~~
%
% @see RFC 2616

separator --> bracket. % 40,41,91,93,123,125
separator --> ">". % 62
separator --> "<". % 60
separator --> "@". % 64
separator --> ",". %44
separator --> ";". % 59
separator --> ":". % 58
separator --> "\\". % 92
separator --> '"'. %34
separator --> "/". % 47, 92
separator --> "?". % 63
separator --> "=". % 61
separator --> 'SP'. % 32
separator --> 'HT'. % 9



%! token(?Token:atom)//
% ~~~{.abnf}
% token = 1*<any CHAR except CTLs or separators>
% ~~~
%
% @see RFC 2616

token(Token) -->
  dcg_multi1(token_, 1-_, Token, [convert(codes_to_atom)]).

token_(C) -->
  'CHAR'(C),
  {\+ phrase('CTL', [C]), \+ phrase(separator, [C])}.



%! value(-ParseTree:compound, ?Value:atom)//
% ~~~{.abnf}
% value     = token | quoted-string
% ~~~

value(value(Value), Value) -->
  token(Value).
value(value(Value), Value) -->
  'quoted-string'(Value).

