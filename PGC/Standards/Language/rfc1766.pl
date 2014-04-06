:- module(
  rfc1766,
  [
    rfc1766_language_header//2, % -Tree:compound
                                % ?LanguageTags:list(atom)
    rfc1766_language_tag//2 % -Tree:compound
                            % ?LanguageTag:atom
  ]
).

/** <module> RFC_1766

Stupport for RFC 1766: *Tags for the Identification of Languages*.

@auhtor Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(generics(atom_ext)).



%! rfc1766_language_header(-Tree:compound, ?LanguageTags:list(atom))//
% The **Language header** is intended for use in the case where one desires
% to indicate the language(s) of something that has RFC-822-like
% headers, like MIME body parts or Web documents.
%
% ~~~{.bnf}
% Language-Header = "Content-Language" ":" 1#Language-tag
% ~~~
%
% Note that language_header//2 is allowed to list several languages in
% a comma-separated list.
%
% Whitespace is allowed, which means also that one can place
% parenthesized comments anywhere in the language sequence.
%
% # Examples
%
% ~~~
% Content-Type: multipart/alternative;
%        differences=content-language
%      Content-Language: no-nynorsk, no-bokmaal
% ~~~
%
% ~~~
% Content-type: audio/basic
% Content-Language: en-cockney
% ~~~
%
% ~~~
% Content-type: text/plain; charset=iso-8859-10
% Content-Language: i-sami-no (North Sami)
% ~~~
%
% ~~~
% Content-type: application/dictionary
% Content-Language: en, fr (This is a dictionary)
% ~~~
%
% ~~~
% Content-type: multipart/alternative
% Content-Language: en, fr, de, da, el, it
% ~~~
%
% ~~~
% Content-type: video/mpeg
% Content-Language: x-klingon
% ~~~

rfc1766_language_header(language_header('Content-Language',':',T), LanguageTags) -->
  "Content-Language",
  colon,
  rfc1766_language_tags(T, LanguageTags).

%! rfc1766_language_tag(-Tree:compound, ?LanguageTag:atom)//
% A language tag identifies a natural language spoken, written, or
% otherwise conveyed by human beings for communication of information
% to other human beings. Computer languages are explicitly excluded.
%
% ~~~{.bnf}
% Language-Tag = Primary-tag *( "-" Subtag )
% ~~~
%
% There is no guaranteed relationship between languages whose tags
% start out with the same series of subtags; especially, they are NOT
% guraranteed to be mutually comprehensible.
%
% Applications should always treat language tags as a single token; the
% division into main tag and subtags is an administrative mechanism,
% not a navigation aid.
%
% # Examples
%
% ~~~
% en
% en-US
% en-cockney
% i-cherokee
% x-pig-latin
% ~~~

rfc1766_language_tag(language_tag(T1,T2), LanguageTag) -->
  {
    nonvar(LanguageTag), !,
    atomic_list_concat([PrimaryTag|SubTags], '-', LanguageTag)
  },
  primary_tag(T1, PrimaryTag),
  subtags(T2, SubTags).
rfc1766_language_tag(language_tag(T1,T2), LanguageTag) -->
  primary_tag(T1, PrimaryTag),
  subtags(T2, SubTags),
  {atomic_list_concat([PrimaryTag|SubTags], '-', LanguageTag)}.

%! rfc1766_language_tags(-Tree:compound, ?LanguageTags:list(atom))//
% @tbd Allow comments in between list items.

rfc1766_language_tags(language_tags(T), [H]) -->
  rfc1766_language_tag(T, H).
rfc1766_language_tags(language_tags(T1,',',T2), [H|T]) -->
  rfc1766_language_tag(T1, H),
  rfc1766_language_tags(T2, T).

%! primary_tag(-Tree:compound, ?PrimaryTag:atom)//
% ~~~{.bnf}
% Primary-tag = 1*8ALPHA
% ~~~
%
% Predefined registrations for the primary language tag:
%   * All 2-letter tags are interpreted according to ISO standard
%     639, *Code for the representation of names of languages*.
%   * The value `i` is reserved for IANA-defined registrations.
%   * The value `x` is reserved for private use.
%     Subtags of `x` will not be registered by the IANA.
%   * Other values cannot be assigned except by updating this standard.

primary_tag(primary_tag(PrimaryTag), PrimaryTag) --> tag(PrimaryTag).

%! subtag(-Tree:compound, ?SubTag:atom)//
% ~~~{.bnf}
% Subtag = 1*8ALPHA
% ~~~

subtag(subtag(SubTag), SubTag) --> tag(SubTag).

%! subtags(-Tree:compound, ?SubTags:list(atom))//
% In the first subtag:
%   * All 2-letter codes are interpreted as ISO 3166 alpha-2.
%     country codes denoting the area in which the language is used.
%   * Codes of 3 to 8 letters may be registered with the IANA.
%
% In the second and subsequent subtag, any value can be registered.
%
% Possible uses of subtags:
%   * Country identification, such as `en-US` (this usage is
%     described in ISO 639).
%   * Dialect or variant information, such as `no-nynorsk` or `en-cockney`.
%   * Languages not listed in ISO 639 that are not variants of
%     any listed language, which can be registered with the `i`-prefix,
%     such as `i-cherokee`.
%   * Script variations, such as `az-arabic` and `az-cyrillic`.

subtags(subtags([]), []) --> [].
subtags(subtags('-',T1,T2), [H|T]) -->
  hyphen_minus,
  subtag(T1, H),
  subtags(T2, T).

%! tag(?Tag:atom)//
% Whitespace is not allowed within the tag.
%
% All tags are to be treated as case insensitive; there exist
% conventions for capitalization of some of them, but these should not
% be taken to carry meaning.
%
% Note that the ISO 639/ISO 3166 convention is that language names are
% written in lower case, while country codes are written in upper case.
% This convention is recommended, but not enforced; the tags are case
% insensitive.

tag(Tag) -->
  {
    nonvar(Tag),
    atom_length(Tag, Length),
    between(1, 8, Length)
  }, !,
  dcg_multi1(ascii_letter, _Rep, Tag, [convert(atom_codes)]).
tag(Tag) -->
  dcg_multi1(ascii_letter, Codes),
  {
    length(Codes, Length),
    between(1, 8, Length),
    atom_codes(Tag, Codes)
  }.
