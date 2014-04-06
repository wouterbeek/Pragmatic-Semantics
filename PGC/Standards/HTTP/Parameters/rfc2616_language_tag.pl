:- module(
  rfc2616_language_tag,
  [
    'language-tag'//3 % -ParseTree:compound
                      % ?PrimaryTag:atom
                      % ?Subtags:list(atom)
  ]
).

/** <module> RFC 2616 language tag

DCG for RFC 2616 language tags.

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).



%! 'language-tag'(-ParseTree:compound, ?PrimaryTag:atom, Subtags:list(atom))//
% HTTP uses language tags within the `Accept-Language` and `Content-Language`
%  fields.
%
% # Syntax
%
% The syntax and registry of HTTP language tags is the same as that
%  defined by RFC 1766.
% In summary, a language tag is composed of 1 or more parts:
%  A primary language tag and a possibly empty series of subtags.
%
% ~~~{.abnf}
% language-tag = primary-tag *( "-" subtag )
% ~~~
%
% # Semantics
%
% A language tag identifies a natural language spoken, written, or otherwise
%  conveyed by human beings for communication of information to other
%  human beings.
% Computer languages are explicitly excluded.

'language-tag'(T0, PrimaryTag, Subtags) -->
  'primary-tag'(T1, PrimaryTag),
  dcg_multi2('_language-tag', _-_, Ts, Subtags),
  {parse_tree('language-tag', [T1|Ts], T0)}.
'_language-tag'(T0, Subtag) -->
  "-",
  subtag(T0, Subtag).



%! 'primary-tag'(-ParseTree:compound, ?Tag:atom)// .

%! subtag(-ParseTree:compound, ?Subtag:atom)// .
%
% # Syntax
%
% White space is not allowed within the tag and all tags are case-insensitive.
% The name space of language tags is administered by the IANA.
%
% ~~~{.abnf}
% primary-tag = 1*8ALPHA
% subtag      = 1*8ALPHA
% ~~~
%
% # Semantics
%
% Any two-letter primary-tag is an ISO-639 language abbreviation
%  and any two-letter initial subtag is an ISO-3166 country code.
%
% # Pragmatics
%
% # Examples
%
% ~~~{.http}
% en
% en-US
% en-cockney
% i-cherokee
% x-pig-latin
% ~~~
%
% The last three tags above are not registered tags;
%  all but the last are examples of tags which could be registered in future.
%
% @see ISO 639
% @see ISO 3166

'primary-tag'('primary-tag'(Tag), Tag) -->
  dcg_multi('ALPHA', 1-8, Cs),
  {atom_codes(Tag, Cs)}.
subtag(subtag(Tag), Subtag) -->
  dcg_multi('ALPHA', 1-8, Cs),
  {atom_codes(Subtag, Cs)}.

