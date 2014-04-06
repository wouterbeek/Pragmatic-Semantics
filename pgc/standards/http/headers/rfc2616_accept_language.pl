:- module(
  rfc2616_accept_language,
  [
    'Accept-Language'//2, % -ParseTree:compound
                          % ?QualityValueLanguageTagPairs:list(pair(between(0.0,1.0),list(atom)))
    'parse_Accept-Language'/2 % +Request:list
                              % -LanguagesTO:list(atom)
  ]
).

/** <module> HTTP Accept-Language

DCG for the `Accept-Language` request header in RFC 2616.

@author Wouter Beek
@see RFC 2616
@tbd 'Accept-Language' does *not* use 'language-tag'! Faulty spec.
@version 2013/12
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(flp(rfc2616_abnf)).
:- use_remote_module(http(rfc2616_basic)).
:- use_remote_module(http_parameters(rfc2616_quality_value)).
:- use_module(library(lists)).
:- use_module(library(pairs)).



%! 'Accept-Language'(
%!   -ParseTree:compound,
%!   ?QualityValueLanguageTagPairs:list(pair(between(0.0,1.0),list(atom)))
%! )//
% The `Accept-Language` request-header field is similar to `Accept`,
%  but restricts the set of natural languages that are preferred
%  as a response to the request.
%  Language tags are defined in [rfc2616_language_tag].
%
% # Syntax
%
% ~~~{.abnf}
% Accept-Language = "Accept-Language" ":"
%                   1#( language-range [ ";" "q" "=" qvalue ] )
% ~~~
%
% # Semantics
%
% ## Quality factors
%
% The language quality factor assigned to
%  a language-tag by the Accept-Language field is
%  the quality value of the longest language-range in the field
%  that matches the language-tag.
% If no language-range in the field matches the tag,
%  the language quality factor assigned is 0.
% If no Accept-Language header is present in the request,
%  the server SHOULD assume that all languages are equally acceptable.
% If an Accept-Language header is present,
%  then all languages which are assigned a quality factor greater than 0
%  are acceptable.
%
% ## Example
%
% [1] means "I prefer Danish, but will accept British English
%  and other types of English."
%
% ~~~{.http}
% [1]   Accept-Language: da, en-gb;q=0.8, en;q=0.7
% ~~~
%
% # Pragmatics
%
% ## Privacy concerns
%
% It might be contrary to the privacy expectations of the user to send
%  an `Accept-Language` header with the complete linguistic preferences
%  of the user in every request.
%
% ## Application support for user-setting
%
% As intelligibility is highly dependent on the individual user,
%  it is recommended that client applications make the choice of
%  linguistic preference available to the user.
% If the choice is not made available,
%  then the `Accept-Language` header field MUST NOT be given in the request.
%
% When making the choice of linguistic preference available to the user,
%  we remind implementors of the fact that users are not familiar with
%  the details of language matching as described above,
%  and should provide appropriate guidance.
% As an example, users might assume that on selecting `en-gb`,
%  they will be served any kind of English document if British English
%  is not available.
% A user agent might suggest in such a case to add `en` to get
%  the best matching behavior.

'Accept-Language'('Accept-Language'(Ts), TO5) -->
  "Accept-Language:",
  abnf_list2('_Accept-Language', 1-_, Ts, TO1),
  dcg_end,
  {
    keysort(TO1, TO2),
    reverse(TO2, TO3),
    pairs_values(TO3, TO4),
    maplist('_atomic_list_concat'('-'), TO4, TO5)
  }.
'_Accept-Language'(T0, QualityValue-LanguageTag) -->
  'language-range'(T1, LanguageTag),
  (
    ";q=",
    qvalue(T2, QualityValue)
  ;
    "",
    {QualityValue = 1.0}
  ),
  {parse_tree('Accept-Language', [T1,T2], T0)}.
'_atomic_list_concat'(Separator, Atoms, Atom):-
  atomic_list_concat(Atoms, Separator, Atom).



%! 'language-range'(-ParseTree:compound, ?LanguageTag:list(atom))//
% # Syntax
%
% ~~~{.abnf}
% language-range = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
% ~~~
%
% ## Prefix matching
%
% A `language-range` matches a `language-tag` if it exactly equals the tag,
%  or if it exactly equals a prefix of the tag such that
%  the first tag character following the prefix is dash (`-`).
% The special range asterisk (`*`), if present in the `Accept-Language` field,
%  matches every tag not matched by any other range present in
%  the `Accept-Language` field.
%
% # Semantics
%
% Each language-range MAY be given an associated quality value
%  which represents an estimate of the user's preference for
%  the languages specified by that range.
% The quality value defaults to "q=1".
%
% ## Prefix matching
%
% This use of a prefix matching rule does not imply that language tags
%  are assigned to languages in such a way that it is always true that
%  if a user understands a language with a certain tag,
%  then this user will also understand all languages with tags for which
%  this tag is a prefix.
% The prefix rule simply allows the use of prefix tags if this is the case.

'language-range'('language-range'(LanguageTag), LanguageTag) -->
  dcg_multi1('_language-range', 1-_, LanguageTag, [separator(hyphen)]).
'language-range'('language-range'('*'), '*') -->
  "*".
'_language-range'(LanguageSubtag) -->
  dcg_multi1('ALPHA', 1-8, Codes),
  {atom_codes(LanguageSubtag, Codes)}.



'parse_Accept-Language'(Request, TO):-
  memberchk(accept_language(AcceptLanguage), Request),
  atomic_list_concat(['Accept-Language',AcceptLanguage], ':', Atom),
  atom_codes(Atom, Codes),
  phrase('Accept-Language'(_ParseTree, TO), Codes).

