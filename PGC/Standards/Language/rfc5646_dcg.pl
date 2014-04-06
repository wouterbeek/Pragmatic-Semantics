:- module(
  rfc5646_dcg,
  [
    rfc5646_language_tag//2 % -Tree:compound
                            % ?LanguageTag:compound
  ]
).

/** <module> RFC5646

Support for RFC 5646 on tags for identifying languages.

## Concepts

  * **Code**
    A value defined in external standards (used as a subtag in this document).
    E.g., `Hant` is an ISO15924 script code that was used to define the
    `Hant` script subtag for use in a language tag.
  * **Language collection**
    A group of languages that are descended from a common ancestor, are
    spoken in the same geographical area, or are otherwise related.
  * **Primary language subtag**
    The first language subtag in a language tag.
    I.e., language// in rfc5646_standard_language_tag//, x// in privateuse//,
    and i// in some grandfathered//.
  * **Subtag**
    A specific section of a tag, delimited by a hyphen, such as the subtags
    `zh`, `Hant`, and `CN` in the tag `zh-Hant-CN`.
  * **Tag**
    A complete language tag, such as `sr-Latn-RS` or `az-Arab-IR`.

## Case

Language tags and their subtags, including private use and extensions,
are case insensitive.
Consistent formatting and presentation of language tags does aid users.
The existing conventions for case in language tags are used:
  * *ISO639-1* recommends that language codes be written in lowercase
    Example: `mn` for Mongolian.
  * *ISO15924* recommends that script codes use lowercase with the
    initial letter capitalized.
    Example" `Cyrl` for Cyrillic.
  * *ISO3166-1* recommends that country codes be capitalized.
    Example: `MN` for Mongolia.

### How to implement case

All subtags, including extension and private use subtags, use lowercase
letters with two exceptions: 2-letter and 4-letter subtags that neither
appear at the start of the tag nor occur after singletons.
Such 2-letter subtags are all uppercase (e.g., `en-CA-x-ca`, `sgn-BE-FR`)
and 4-letter subtags are titlecase (e.g., `az-Latn-x-latn`).

### Case folding outside of the character set used for encoding

Case folding of ASCII letters in certain locales, unless
carefully handled, sometimes produces non-ASCII character values.
The Unicode Character Database file `SpecialCasing.txt`
defines the specific cases that are known to cause
problems with this. In particular, the letter 'i' (U+0069) in
Turkish and Azerbaijani is uppercased to `U+0130` (`LATIN CAPITAL LETTER
I WITH DOT ABOVE`). Implementers SHOULD specify a locale-neutral
casing operation to ensure that case folding of subtags does not
produce this value, which is illegal in language tags.  For example,
if one were to uppercase the region subtag `in` using Turkish locale
rules, the sequence `U+0130 U+004E` would result, instead of the
expected `IN`.

## Language tag formal properties

Language tags are designed so that each subtag type has unique length and
content restrictions.

Sequences of private use and extension subtags MUST occur at the end
of the sequence of subtags and MUST NOT be interspersed with subtags
defined elsewhere in this document. These sequences are introduced
by single-character subtags, which are reserved as follows:
  *  `x`
     Introduces a sequence of private use subtags. The interpretation of any
     private use subtag is defined solely by private agreement.
  *  `i`
     Used by some grandfathered tags, e.g. *i-default*, where it always
     appears in the first position and cannot be confused with an extension.
  *  All other single-letter and single-digit subtags are reserved to
     introduce standardized extension subtag sequences.

## Non-standard language tags

Some of the subtags in the IANA registry do not come from an underlying
standard. These can only appear in specific positions in a tag: they can only
occur as primary language subtags or as variant subtags.

## Well-formedness

A tag is considered well-formed if it conforms to the ABNF.
Language tags may be well-formed in terms of syntax but not valid
in terms of content.

## Validity

A tag is valid if:
  1. The tag is well-formed.
  2. Either the tag is in the list of grandfathered tags or all of its
     primary language, extended language, script, region, and variant
     subtags appear in the IANA Language Subtag Registry as of the
     particular registry date.
  3. There are no duplicate variant subtags.
  4. There are no duplicate singleton (extension) subtags.

## Guidelines for the use of language tags

### Precision

Use as precise a tag as possible, but no more specific than is
justified.  Avoid using subtags that are not important for
distinguishing content in an application.

For example, `de` might suffice for tagging an email written in German,
while `de-CH-1996` is probably unnecessarily precise for such a task.

Some subtag sequences might not represent the language a casual user might
expect. For example, the Swiss German (Schweizerdeutsch) language is
represented by `gsw-CH` and not by `de-CH`. This latter tag represents German
(`de`) as used in Switzerland (`CH`), also known as Swiss High German
(Schweizer Hochdeutsch). Both are real languages, and distinguishing between
them could be important to an application.

### Script

The script subtag SHOULD NOT be used to form language tags unless
the script adds some distinguishing information to the tag.

Some applications can benefit from the use of script subtags in language tags,
as long as the use is consistent for a given context.

Script subtags are never appropriate for unwritten content.

The field `Suppress-Script` in the primary or extended language indicates
script subtags that do not add distinguishing information for most
applications. Users SHOULD NOT include such a script subtag with a particular
primary language subtag.

For example, the subtag `Latn` should not be used with the primary language
`en` because nearly all English documents are written in the Latin script
and it adds no distinguishing information.
Moreover, Basic Filtering with filter `en-US` will not match `en-Latn-US`.
However, if a document were written in English mixing Latin
script with another script such as Braille (`Brai`), then it
might be appropriate to choose to indicate both scripts to aid
in content selection, such as the application of a style sheet.

For example, the subtitles to a movie might use the tag `uz-Arab` (Uzbek,
Arabic script), but the audio track for the same language would be tagged
simply `uz`.

## Preferred value

If a tag or subtag has a `Preferred-Value` field in its registry
entry, then the value of that field SHOULD be used to form the
language tag in preference to the tag or subtag in which the
preferred value appears.

For example, use `jbo` for Lojban in preference to the grandfathered tag
`art-lojban`.

## Language collections

Use subtags or sequences of subtags for individual languages in
preference to subtags for language collections.

Certain language collections are assigned codes by ISO 639-5 (and some of
these are also defined as collections in ISO 639-2).
These codes are included as primary language subtags in the registry.
Subtags for a language collection in the registry have a `Scope` field with a
value of `collection`.
A subtag for a language collection is always preferred to less specific
alternatives such as `mul` and `und` (see below), and a subtag representing
a language collection MAY be used when more specific language information is
not available. However, most users and implementations do not know there is a relationship between the collection and its individual languages. In addition,
the relationship between the individual languages in the collection is not
well defined; in particular, the languages are usually not mutually
intelligible.

Since the subtags are different, a request for the collection will typically
only produce items tagged with the collection's subtag, not items tagged with
subtags for the individual languages contained in the collection.

For example, collections are interpreted inclusively, so the subtag `gem`
(Germanic languages) could, but SHOULD NOT, be used with content that would be
better tagged with `en` (English), `de` (German), or `gsw` (Swiss German,
Alemannic). While `gem` collects all of these (and other) languages, most
implementations will not match `gem` to the individual languages; thus, using
the subtag will not produce the desired result.

## Non-language tags

ISO 639-2 has defined several codes included in the subtag registry that
require additional care when choosing language tags.
In most of these cases, where omitting the language tag is permitted, such
omission is preferable to using these codes.

Language tags SHOULD NOT incorporate these subtags as a prefix, unless the
additional information conveys some value to the application.

### Multiple primary lanugages subtag

The `mul` (Multiple) primary language subtag identifies content in multiple
languages. This subtag SHOULD NOT be used when a list of languages or
individual tags for each content element can be used instead.

For example, the `Content-Language` header [RFC 3282] allows a list of
languages to be used, not just a single language tag.

### Undetermined primary language subtag

The `und` (Undetermined) primary language subtag identifies linguistic content
whose language is not determined. This subtag SHOULD NOT be used unless a
language tag is required and language information is not available or cannot
be determined.

Omitting the language tag (where permitted) is preferred.

The `und` subtag might be useful for protocols that require a language tag to
be provided or where a primary language subtag is required (such as in
`und-Latn`). The `und` subtag MAY also be useful when matching language tags
in certain situations.

### Not-applicable primary language subtag

The `zxx` (Non-Linguistic, Not Applicable) primary language subtag identifies
content for which a language classification is inappropriate or does not
apply.

E.g., instrumental or electronic music; sound recordings consisting of
nonverbal sounds; audiovisual materials with no narration, dialog,
printed titles, or subtitles; machine-readable data files consisting of
machine languages or character codes; or programming source code.

### Uncoded primary language subtag

The `mis` (Uncoded) primary language subtag identifies content whose language
is known but that does not currently have a corresponding subtag.
This subtag SHOULD NOT be used, because the addition of other codes in the
future can render its application invalid, it is inherently unstable and hence
incompatible with the stability goals of BCP 47. It is always preferable to
use other subtags: either `und` or (with prior agreement) private use subtags.

## Variant subtags

Use variant subtags sparingly and in the correct order.

Variants SHOULD only be used with subtags that appear in one of
the `Prefix` fields for their registration.

If a variant lists a second variant in one of its `Prefix` fields, the first
variant SHOULD appear directly after the second variant in any language tag
where both occur.

General purpose variants (those with no `Prefix` fields) SHOULD appear after
any other variant subtags.

Order any remaining variants by placing the most significant subtag first.
If none of the subtags is more significant or no relationship can be
determined, alphabetize the subtags.

Because variants are very specialized, using many of them together generally
makes the tag so narrow as to override the additional precision gained.

Putting the subtags into another order interferes with interoperability,
as well as the overall interpretation of the tag.

E.g., the tag `en-scotland-fonipa` (English, Scottish dialect, IPA phonetic
transcription) is correctly ordered because `scotland` has a `Prefix` of `en`,
while `fonipa` has no `Prefix` field.

E.g., the tag `sl-IT-rozaj-biske-1994` is correctly ordered: `rozaj` lists
`sl` as its sole `Prefix`; `biske` lists `sl-rozaj` as its sole `Prefix`.
The subtag `1994` has several prefixes, including `sl-rozaj`. However, it
follows both `rozaj` and `biske` because one of its `Prefix` fields is
`sl-rozaj-biske`.

## Default language tag

The grandfathered tag `i-default` (Default Language) was originally registered
according to RFC 1766 to meet the needs of RFC 2277.
It is not used to indicate a specific language, but rather to identify the
condition or content used where the language preferences of the user cannot
be established.
It SHOULD NOT be used except as a means of labeling the default content for
applications or protocols that require default language content to be labeled
with that specific tag. It MAY also be used by an application or protocol to
identify when the default language content is being returned.

## Encompassed primary language subtags

Some primary language records in the registry have a `Macrolanguage` field
that contains a mapping from each "encompassed language" to its macrolanguage.

The `Macrolanguage` mapping does not define what the relationship between the
encompassed language and its macrolanguage is, nor does it define how
languages encompassed by the same macrolanguage are related to each other.

Two different languages encompassed by the same macrolanguage may differ from
one another more than, say, French and Spanish do. [W]This seems besided the
point to me. The question is whether languages L1 and L2 under the same
macrolanguage L3 are more alike than any pair <L1,L'> or <L2,L'> for an
arbitrary language L' that is not encomapssed by L3.[/W]

A few specific macrolanguages, such as Chinese (`zh`) and Arabic
(`ar`), are handled differently (see below).

The more specific encompassed language subtag SHOULD be used to form
the language tag, although either the macrolanguage's primary
language subtag or the encompassed language's subtag MAY be used.
This means, for example, tagging Plains Cree with `crk` rather than
`cr` (Cree), and so forth.

Each macrolanguage subtag's scope, by definition, includes all of its
encompassed languages. Since the relationship between encompassed
languages varies, users cannot assume that the macrolanguage subtag
means any particular encompassed language, nor that any given pair of
encompassed languages are mutually intelligible or otherwise
interchangeable.

Applications MAY use macrolanguage information to improve matching or
language negotiation.  For example, the information that `sr` (Serbian) and
`hr` (Croatian) share a macrolanguage expresses a closer relation between
those languages than between, say, `sr` (Serbian) and `ma` (Macedonian).

However, this relationship is not guaranteed nor is it exclusive.
For example, Romanian (`ro`) and Moldavian (`mo`) do not share a
macrolanguage, but are far more closely related to each other than
Cantonese (`yue`) and Wu (`wuu`), which do share a macrolanguage.
[W]This seems besided the point to me. The question is whether languages
L1 and L2 under the same macrolanguage L3 are more alike than any pair <L1,L'>
or <L2,L'> for an arbitrary language L' that is not encomapssed by L3.[/W]

## Compatibility mechanism: extended language subtag

To accommodate language tag forms used prior to the adoption of this
document, language tags provide a special compatibility mechanism:
the extended language subtag.

Selected languages have been provided with both primary and extended language
subtags. These include macrolanguages, such as Malay (`ms`) and Uzbek (`uz`),
that have a specific dominant variety that is generally synonymous with the
macrolanguage. Other languages, such as the Chinese (`zh`) and Arabic (`ar`)
macrolanguages and the various sign languages (`sgn`), have traditionally used
their primary language subtag, possibly coupled with various region subtags
or as part of a registered grandfathered tag, to indicate the language.

With the adoption of this document, specific ISO 639-3 subtags became
available to identify the languages contained within these diverse
language families or groupings. This presents a choice of language
tags where previously none existed:
  * Each encompassed language's subtag SHOULD be used as the primary
    language subtag. For example, a document in Mandarin Chinese
    would be tagged `cmn` (the subtag for Mandarin Chinese) in
    preference to `zh` (Chinese).
  * If compatibility is desired or needed, the encompassed subtag MAY
    be used as an extended language subtag. For example, a document
    in Mandarin Chinese could be tagged `zh-cmn` instead of either
    `cmn` or `zh`.
  * The macrolanguage or prefixing subtag MAY still be used to form
    the tag instead of the more specific encompassed language subtag.
    That is, tags such as `zh-HK` or `sgn-RU` are still valid.

Sign languages share a mode of communication rather than a linguistic
heritage. There are many sign languages that have developed independently,
and the subtag `sgn` indicates only the presence of a sign language.

## Semantics

Each subtag implies a range of expectations one might have for related
content, but this is not guaranteed.

Valid language tags might not represent any real-world language usage (e.g.,
`tlh-Kore-AQ-fonipa` which means Klingon, Korean script, as used in Antarctica,
IPA phonetic transcription).

### Meaning relative to information object type

The meaning of a given tag does not depend on the context in which it
appears. The relationship between a tag's meaning and the information objects
to which that tag is applied, however, can vary.
  * For a single information object, the associated language tags
    might be interpreted as the set of languages that is necessary for
    a complete comprehension of the complete object.
  * For an aggregation of information objects, the associated language
    tags could be taken as the set of languages used inside components
    of that aggregation.
  * For information objects whose purpose is to provide alternatives, the
    associated language tags could be regarded as a hint that the content is
    provided in several languages and that one has to inspect each of the
    alternatives in order to find its language or languages. In this case,
    the presence of multiple tags might not mean that one needs to be
    multilingual to get complete understanding of the document.
    E.g., MIME multipart/alternative [RFC 2046].
  * For markup languages, language information can be added to each part of
    the document identified by the markup structure.
  * For markup languages and document formats that allow the audience
    to be identified, a language tag could indicate the audience(s)
    appropriate for that document.
  * For systems and APIs, language tags form the basis for most
    implementations of locale identifiers. E.g., Unicode's CLDR
    (Common Locale Data Repository) project.

### Relatedness & narrower / more specific

Language tags are related when they contain a similar sequence of
subtags. For example, if a language tag B contains language tag A as
a prefix, then B is typically "narrower" or "more specific" than A.
Thus, `zh-Hant-TW` is more specific than `zh-Hant`.

This relationship is not guaranteed in all cases: specifically, languages
that begin with the same sequence of subtags are NOT guaranteed to be mutually
intelligible, although they might be. E.g., the tag `az` shares a prefix with
both `az-Latn` (Azerbaijani written using the Latin script) and
`az-Cyrl` (Azerbaijani written using the Cyrillic script). A person fluent in
one script might not be able to read the other, even though the linguistic
content (e.g., what would be heard if both texts were read aloud) might be
identical. Content tagged as `az` most probably is written in just one script
and thus might not be intelligible to a reader familiar with the other script.

Similarly, not all subtags specify an actual distinction in language.
For example, the tags `en-US` and `en-CA` mean, roughly, English with
features generally thought to be characteristic of the United States
and Canada, respectively. They do not imply that a significant
dialectical boundary exists between any arbitrarily selected point in
the United States and any arbitrarily selected point in Canada.
Neither does a particular region subtag imply that linguistic
distinctions do not exist within that region.

## Multiple language tags

In some applications, a single content item might best be associated
with more than one language tag:
  * Content items that contain multiple, distinct varieties. Often this is
    used to indicate an appropriate audience for a given content item when
    multiple choices might be appropriate. Examples:
    * A DVD might label its individual audio tracks `de` (German),
      `fr` (French), and `es` (Spanish), but the overall title would list
      =|de, fr, es|= as its overall audience.
    * A French/English, English/French dictionary tagged as both `en` and `fr`
      to specify that it applies equally to French and English.
    * A side-by-side or interlinear translation of a document, as is commonly
      done with classical works in Latin or Greek.
  * Content items that contain a single language but that require
    multiple levels of specificity. For example, a library might wish
    to classify a particular work as both Norwegian (`no`) and as
    Nynorsk (`nn`) for audiences capable of appreciating the
    distinction or needing to select content more narrowly.

## Tag length

Since language tags have no length limit according to this specification,
a buffer limit in combination with truncation may be applied:
  * Implementations SHOULD NOT truncate language tags unless the
    meaning of the tag is purposefully being changed, or unless the
    tag does not fit into a limited buffer size specified by a
    protocol for storage or transmission.
  * Implementations SHOULD warn the user when a tag is truncated since
    truncation changes the meaning of the tag.
  * Implementations of protocols or specifications that are space constrained
    but do not have a fixed limit SHOULD use the longest possible tag in
    preference to truncation.
  * Protocols or specifications that specify limited buffer sizes for
    language tags MUST allow for language tags of at least 35 characters.
    (language      =  8 ; longest allowed registered value
                      ;   longer than primary+extlang
                      ;   which requires 7 characters
   script        =  5 ; if not suppressed: see Section 4.1
   region        =  4 ; UN M.49 numeric region code
                      ;   ISO 3166-1 codes require 3
   variant1      =  9 ; needs 'language' as a prefix
   variant2      =  9 ; very rare, as it needs
                      ;   'language-variant1' as a prefix

--

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(lang('iso639-1')). % Used in meta-call.
:- use_module(lang('iso639-2')). % Used in meta-call.
:- use_module(lang(rfc5646_iana)).
:- use_module(library(plunit)).

:- db_add_novel(user:prolog_file_type(txt, text)).



%! irregular(?LanguageSubtags:list(atom))//

irregular([en,'GB',oed]).
irregular([i,ami]).
irregular([i,bnn]).
irregular([i,default]).
irregular([i,enochian]).
irregular([i,hak]).
irregular([i,klingon]).
irregular([i,lux]).
irregular([i,mingo]).
irregular([i,navajo]).
irregular([i,pwn]).
irregular([i,tao]).
irregular([i,tay]).
irregular([i,tsu]).
irregular([sgn,'BE','FR']).
irregular([sgn,'BE','NL']).
irregular([sgn,'CH','DE']).

%! regular(?LanguageSubtags:list(atom))//

regular([art,lojban]).
regular([cel,gaulish]).
regular([no,bok]).
regular([no,nyn]).
regular([zh,guoyu]).
regular([zh,hakka]).
regular([zh,min]).
regular([zh,min,nan]).
regular([zh,xiang]).

%! rfc5646_extended_language_subtag(
%!   -Tree:compound,
%!   ?LanguageExtensions:atom
%! )//
% Extended language subtags are used to identify certain specially
% selected languages that, for various historical and compatibility
% reasons, are closely identified with or tagged using an existing
% primary language subtag.
%
% All languages that have an extended language subtag in the registry
% also have an identical primary language subtag record in the
% registry.
% This primary language subtag is RECOMMENDED for forming the language tag.
%
% All extended language subtags are 3-letter subtags fom ISO 639-3.
%
% Although the ABNF production `extlang` permits up to three
% extended language tags in the language tag, extended language
% subtags MUST NOT include another extended language subtag in
% their `Prefix`. That is, the second and third extended language
% subtag positions in a language tag are permanently reserved and
% tags that include those subtags in that position are, and will
% always remain, invalid.
%
% ~~~{.abnf}
% extlang = 3ALPHA *2("-" 3ALPHA)
% ~~~
%
% ## Example
%
% Gan Chinese is represented with tags beginning `zh-gan` or `gan`.
%
% @tbd Find a better/shorter way to write this down: =|M*N(DCG_Rule)|=
%      with arguments.

rfc5646_extended_language_subtag(T0, L) -->
  dcg_multi1(ascii_letter, 3, X1, [out(atom)]),
  {rfc5646_class(X1, 'Subtag')},
  (
    "", {L = [X1]}
  ;
    hyphen_minus, {H1 = '-'},
    dcg_multi1(ascii_letter, 3, X2, [convert(atom_codes)]),
    {rfc5646_class(X2, 'Subtag')},
    (
      "",
      {L = [X1,X2]}
    ;
      hyphen_minus, {H2 = '-'},
      dcg_multi1(ascii_letter, 3, X3, [convert(atom_codes)]),
      {rfc5646_class(X3, 'Subtag')},
      (
        "",
        {L = [X1,X2,X3]}
      ;
        hyphen_minus, {H3 = '-'},
        dcg_multi1(ascii_letter, 3, X4, [convert(atom_codes)]),
        {rfc5646_class(X4, 'Subtag')},
        {L = [X1,X2,X3,X4]}
      )
    )
  ),
  {parse_tree(extended_language_subtag, [X1,H1,X2,H2,X3,H3,X4], T0)}.

%! rfc5646_extension(-Tree:compound, ?Extension:list(atomic))//
% Extensions provide a mechanism for extending language tags for use in
% various applications. They are intended to identify information that
% is commonly used in association with languages or language tags but
% that is not part of language identification.
%
% ~~~{.abnf}
% extension = singleton 1*("-" (2*8alphanum))
% ~~~
%
% ## Position in language tag
%
% Extension subtags MUST follow all primary language, extended language,
% script, region, and variant subtags in a tag and MUST precede any private
% use subtag sequences.
%
% An extension MUST follow at least a primary language subtag.
% Extensions extend language tags, they do not override or replace them.
% Note that extensions cannot be used in tags that are entirely private use
% (i.e., those starting with `x-`).
%
% ## Singleton subtags
%
% Extension subtags are separated from the other subtags defined in this
% document by a single-character subtag singleton//.
% The singleton MUST be one allocated to a registration authority and
% MUST NOT be the letter `x` which is reserved for private use.
%
% Singleton subtags MUST NOT be repeated.
% Note that the tag `en-a-bbb-x-a-ccc` is valid because the second
% appearance of the singleton `a` is in a private use sequence.
%
% Each singleton MUST be followed by at least one extension subtag.
%
% All subtags following the singleton and before another singleton are part
% of the extension.
% Example: in the tag `fr-a-Latn`, the subtag `Latn` does not represent the
% script subtag `Latn`; instead its meaning is defined by the extension `a`.
%
% ## Formal properties
%
% Each extension subtag MUST be from 2 to 8 characters long and consist
% solely of letters or digits, with each subtag separated by a single `-`.
% Normalized extension subtags are expected to be in lowercase.
%
% ## Canonical form
%
% In the event that more than one extension appears in a single tag,
% the tag SHOULD be canonicalized as described in Section 4.5, by ordering
% the various extension sequences into case-insensitive ASCII order.
% For example, if an extension were defined for the singleton `r` and
% it defined the subtags shown, then the following tag would be a valid
% example: `en-Latn-GB-boont-r-extended-sequence-x-private`.
%
% @tbd Document and implement canonical form.

rfc5646_extension(extension(T1, T2), [Singleton|ExtensionComponents]) -->
  rfc5646_singleton(T1, Singleton),
  rfc5646_extension_components(T2, ExtensionComponents).

%! rfc5646_extension_components(-Tree:compound, ?ExtensionComponents:list(atom))//

rfc5646_extension_components(extension_components('-',H,T1), [H|T]) -->
  hyphen_minus,
  dcg_multi1(ascii_alpha_numeric, 2-8, H, [convert(atom_codes)]),
  {rfc5646_class(H, 'Extension')},
  rfc5646_extension_components(T1, T).
rfc5646_extension_components(extension_components('-',H), [H]) -->
  hyphen_minus,
  dcg_multi1(ascii_alpha_numeric, 2-8, H, [convert(atom_codes)]),
  {rfc5646_class(H, 'Extension')}.

%! rfc5646_extensions(-Tree:compound, ?Extensions:list(list(atomic)))//

rfc5646_extensions(extensions('-',T1,T2), [H|T]) -->
  hyphen_minus,
  rfc5646_extension(T1, H),
  rfc5646_extensions(T2, T).
rfc5646_extensions(extensions('-',T1), [H]) -->
  hyphen_minus,
  rfc5646_extension(T1, H).

%! rfc5646_grandfathered_language_tag(-Tree:compound, ?LanguageTag:list(atom))//
% Non-redundant (see below) tags registered during the RFC 1766 and/or
% RFC 3066 era.
%
% ~~~{.abnf}
% grandfathered = irregular / regular
% ~~~
%
% ## Non-grandfathered redundant tags
%
% Many of the previously registered language tags are now redundant.
% A **redundant tag** is a grandfathered registration whose individual
% subtags appear with the same semantic meaning in the registry.
% For example, the tag `zh-Hant` (Traditional Chinese) can now be composed
% from the subtags `zh` (Chinese) and `Hant` (Han script traditional variant).
% These redundant tags are maintained in the registry as records of type
% `redundant`, mostly as a matter of historical curiosity.
%
% ## Grandfathered tags 1: Rergular
%
% Grandfathered tags that (appear to) match the
% rfc5646_standard_language_tag// production are
% considered regular grandfathered tags.
% Such a tag contains one or more subtags that either do not
% individually appear in the registry or appear but with a different
% semantic meaning: each tag, in its entirety, represents a language or
% collection of languages.
%
% ## Grandfathered tags 2: Non-rergular
%
% Grandfathered tags that do not match the rfc5646_standard_language_tag//
% production in the ABNF
% and would otherwise be invalid are considered irregular grandfathered tags.
% With the exception of `en-GB-oed`, which is a variant of `en-GB`, each of
% them, in its entirety, represents a language.
%
% ## Phasing out procedure for grandfathered tags
%
% Many of the grandfathered tags have been superseded by the subsequent
% addition of new subtags: each superseded record contains a `Preferred-Value`
% field that ought to be used to form language tags representing that value.
% For example, the tag `art-lojban` is superseded by the primary language
% subtag `jbo`.
%
% ## Irregular
%
% Irregular tags do not match the rfc5646_standard_language_tag// production
% and would not otherwise
% be considered well-formed. These tags are all valid, but most are deprecated
% in favor of more modern subtags or subtag combinations.
%
% The single-character subtag `i` is used by some grandfathered tags.
% (Other grandfathered tags have a primary language subtag in their first
% position.)
%
% ## Regular
%
% These tags match the rfc5646_standard_language_tag// production,
% but their subtags are not extended language or variant subtags:
% their meaning is defined by their registration and all of these are
% deprecated in favor of a more modern subtag or sequence of subtags.

rfc5646_grandfathered_language_tag(T0, Tag) -->
  dcg_multi1(word, _Rep, L1, [separator(hyphen)]),
  {
    atomic_list_concat(L1, '-', Tag),
    rfc5646_class(Tag, 'Grandfathered'),
    append_intersperse(L1, '-', L2),
    parse_tree(irregular_grandfathered_language_tag, L2, T0)
  }.

%! rfc5646_language_tag//
% ~~~{.abnf}
% Language-Tag = langtag / privateuse / grandfathered
% ~~~

% Normal language tags.
rfc5646_language_tag(
  rfc5646_language_tag(T1),
  rfc5646_language_tag(
    Primary,
    Extended,
    Script,
    Region,
    Variants,
    Extensions,
    Private
  )
) -->
  {rfc5646_init},
  rfc5646_standard_language_tag(
    T1,
    Primary,
    Extended,
    Script,
    Region,
    Variants,
    Extensions,
    Private
  ).
% Private use tags.
rfc5646_language_tag(rfc5646_language_tag(T1), LanguageTag) -->
  {rfc5646_init},
  rfc5646_privateuse(T1, LanguageTag).
% Grandfathered tags.
rfc5646_language_tag(rfc5646_language_tag(T1), LanguageTag) -->
  {rfc5646_init},
  rfc5646_grandfathered_language_tag(T1, LanguageTag).

%! rfc5646_language(
%!   -Tree:compound,
%!   ?PrimaryLanguage:atom,
%!   ?LanguageExtensions:list(atom)
%! )//
% The recommended format for language tags.
%
% 2-character language subtags are drawn from ISO 639-1.
%
% 3-character language subtags are drawn from ISO 639-2, -3, and -5.
%
% Subtags in the range `qaa`-`qtz` are reserved for private use
% (as in ISO 639-2).
%
% 4-character language subtags are reserved for future standardization.
%
% 5- to 8-character language subtags are reserved for future discouraged use.
%
% ~~~{.abnf}
% language = 2*3ALPHA ["-" extlang] / 4ALPHA / 5*8ALPHA
% ~~~
%
% ## Inclusion in multiple standards
%
% If languages have both a 2- and a 3-character ISO code, the 2-character code
% is defined in the IANA registry.
%
% When a language has no 2-character code and the ISO 639-2/T (Terminology)
% code differes from the ISO 639-2/B (Bibliographic) code, only the
% Terminology code is defined in the IANA registry.
%
% If a 2-character code is added to ISO 639-1 for a language for which a
% 3-character code was already included in either ISO 639-2 or ISO 639-3,
% the 2-character code MUST NOT be registered.

% The shortest ISO639 code, sometimes followed by extended language subtags.
rfc5646_language(T0, Language, LanguageExtensions) -->
  dcg_multi(ascii_letter, 2-3, Language, [convert(atom_codes)]),
  {rfc5646_class(Language, 'Language')},
  (
    hyphen,
    rfc5646_extended_language_subtag(T2, LanguageExtensions)
  ;
    ""
  ),
  {parse_tree(language, [Language,T2], T0)}.
rfc5646_language(language(Language), Language, []) -->
  (
    % Reserved for future use.
    dcg_multi(ascii_letter, 4, Language, [convert(atom_codes)])
  ;
    % Registered language subtag.
    dcg_multi(ascii_letter, 5-8, Language, [convert(atom_codes)])
  ),
  {rfc5646_class(Language, 'Language')}.

%! rfc5646_privateuse(-Tree:compound, ?PrivateLanguage:list(atom))//
% Private use subtags are used to indicate distinctions in language
% that are important in a given context by private agreement.
%
% ## Position in the language tag
%
% Private use subtags are separated from the other subtags by the reserved
% single-character subtag `x`.
%
% Private use subtags MUST follow all primary language, extended
% language, script, region, variant, and extension subtags in the tag.
%
% ## Formal properties
%
% The single-character subtag `x` as the primary (i.e., first) subtag
% indicates that the language tag consists solely of subtags whose meaning
% is defined by private agreement.
%
% Private use subtags MUST conform to the format and content constraints
% defined in the ABNF for all subtags; that is, they MUST consist solely of
% letters and digits and not exceed 8 characters in length.
%
% A tag MAY consist entirely of private use subtags.
%
% Private use subtags are NOT RECOMMENDED where alternatives exist or for
% general interchange.
%
% ## Example
%
% Suppose a group of scholars is studying some texts in medieval Greek.
% They might agree to use some collection of private use subtags to identify
% different styles of writing in the texts.
% They might use `el-x-koine` for documents in the "common" style while
% using `el-x-attic` for other documents that mimic the Attic style.
% These subtags would not be recognized by outside processes or systems,
% but might be useful in categorizing various texts for study by those
% in the group.
%
% ## Note on standards
%
% In the registry, there are also subtags derived from codes reserved
% by ISO 639, ISO 15924, or ISO 3166 for private use.  Do not confuse
% these with private use subtag sequences following the subtag 'x'.
%
% ~~~{.abnf}
% privateuse = "x" 1*("-" (1*8alphanum))
% ~~~

rfc5646_privateuse(pivateuse(T1), PrivateTags) -->
  x_lowercase,
  rfc5646_privateuse_components(T1, PrivateTags).

rfc5646_privateuse_components(privateuse_components('-',H), [H]) -->
  hyphen_minus,
  dcg_multi(ascii_alpha_numeric, 1-8, H, [convert(atom_codes)]).
rfc5646_privateuse_components(privateuse_components('-',H,T2), [H|T]) -->
  hyphen_minus,
  dcg_multi(ascii_alpha_numeric, 1-8, H, [convert(atom_codes)]),
  rfc5646_privateuse_components(T2, T).

%! rfc5646_region(-Tree:compound, ?Region:atomic)//
% Region subtags indicate linguistic variations associated with or appropriate
% to a specific country, territory, or region.
%
% ~~~{.abnf}
% region = 2ALPHA / 3DIGIT
% ~~~
%
% ## Intended use
%
% Typically, a region subtag is
% used to indicate variations such as regional dialects or usage, or
% region-specific spelling conventions. It can also be used to indicate that
% content is expressed in a way that is appropriate for use throughout a
% region, for instance, Spanish content tailored to be useful throughout Latin
% America.
%
% ## Position within language tag
%
% Region subtags MUST follow any primary language, extended language,
% or script subtags and MUST precede any other type of subtag.
%
% There MUST be at most one region subtag in a language tag and the region
% subtag MAY be omitted, as when it adds no distinguishing value to the tag.
%
% ## Possible values 1: Private use
%
% The region subtags `AA`, `QM`-`QZ`, `XA`-`XZ`, and `ZZ` are reserved for
% private use (as in ISO 3166.
%
% ## Possible values 2: ISO
%
% 2-letter region subtags are defined according to ISO 3166-1, using the list
% of alpha-2 country codes.
% In addition, the codes that are "exceptionally reserved" (as opposed to
% "assigned") in ISO 3166-1 were also defined in the registry, with
% the exception of `UK`, which is an exact synonym for the assigned code `GB`.
%
% ## Possible values 2: UN
%
% 3-character region subtags consist solely of digit (number) characters
% and are defined according to the assignments found in the UN Standard
% Country or Area Codes for Statistical Use (UN_M.49).
% Not all of the UN M.49 codes are defined in the IANA registry.
% The following rules define which codes are entered into the registry as
% valid subtags:
%   1. UN numeric codes assigned to `macro-geographical (continental)` or
%      sub-regions MUST be registered in the registry. These codes are not
%      associated with an assigned ISO 3166-1 alpha-2 code and represent
%      supra-national areas, usually covering more than one nation, state,
%      province, or territory.
%   2. UN numeric codes for `economic groupings` or `other groupings`
%      MUST NOT be registered in the IANA registry and MUST NOT be used
%      to form language tags.
%   3. When ISO 3166-1 reassigns a code formerly used for one country or
%      area to another country or area and that code already is present in
%      the registry, the UN numeric code for that country or area MUST be
%      registered in the registry and MUST be used to form language tags
%      that represent the country or region for which it is defined
%      (rather than the recycled ISO 3166-1 code).
%   4. UN numeric codes for countries or areas for which there is an
%      associated ISO 3166-1 alpha-2 code in the registry MUST NOT be
%      entered into the registry and MUST NOT be used to form language
%      tags.
%   5. For historical reasons, the UN numeric code 830 (Channel Islands),
%      which was not registered at the time this document was adopted
%      and had, at that time, no corresponding ISO 3166-1 code, MAY be
%      entered into the IANA registry.
%   6. All other UN numeric codes for countries or areas that do not
%      have an associated ISO 3166-1 alpha-2 code MUST NOT be
%      entered into the registry and MUST NOT be used to form
%      language tags.
%   7. The alphanumeric codes in Appendix X of the UN document MUST NOT
%      be entered into the registry and MUST NOT be used to form language
%      tags.
%
% ## Examples
%
% `de-AT` represents German (`de`) as used in Austria (`AT`).
%
% `sr-Latn-RS` represents Serbian (`sr`) written using Latin script
% (`Latn`) as used in Serbia (`RS`).
%
% `es-419` represents Spanish (`es`) appropriate to the UN-defined
% Latin America and Caribbean region (`419`).

rfc5646_region(region(Region), Region) -->
  dcg_multi(ascii_letter, 2, Region, [convert(atom_codes)]),
  {rfc5646_class(Region, 'Region')}.
rfc5646_region(region(Region), Region) -->
  dcg_multi(decimal_digit, 3, Region, [convert(codes_number)]),
  {rfc5646_class(Region, 'Region')}.

%! rfc5646_script(-Tree:compound, ?Script:atom)//
% Script or writing system variations that distinguish the written forms of a
% language or its dialects.
%
% ~~~{.abnf}
% script = 4ALPHA
% ~~~
%
% ## Position in language tag
%
% Script subtags MUST follow any primary and extended language subtags and
% MUST precede any other type of subtag.
%
% ## Supported values
%
% Script subtags consist of 4 letters and are defined according to the
% assignments in ISO 15924.
%
% ## Private use
%
% Script subtags `Qaaa`-`Qabx` are reserved for private use (as in ISO 15924).
%
% ## Intended use
%
% There MUST be at most one script subtag in a language tag, and the script
% subtag SHOULD be omitted when it adds no distinguishing value to the tag or
% when the primary or extended language subtag's record in the subtag registry
% includes a `Suppress-Script` field listing the applicable script subtag.
%
% ## Example
%
% `sr-Latn` represents Serbian written using the Latin script.

rfc5646_script(script(Script), Script) -->
  dcg_multi(ascii_letter, 4, Script, [convert(atom_codes)]),
  {rfc5646_class(Script, 'Script')}.

%! singleton(-Tree:compound, ?Char:atomic)//
% Single alphanumerics. x// is reserved for private use.
%
% ~~~{.abnf}
% singleton = DIGIT     ; 0 - 9
%           / %x41-57   ; A - W
%           / %x59-5A   ; Y - Z
%           / %x61-77   ; a - w
%           / %x79-7A   ; y - z
% ~~~
%
% @see extension//2

rfc5646_singleton(singleton(Singleton), Singleton) -->
  decimal_digit(_, Singleton).
rfc5646_singleton(singleton(Singleton), Singleton) -->
  ascii_letter(Code),
  {
    \+ memberchk(Code, [88,120]),
    char_code(Singleton, Code)
  }.

%! rfc5646_standard_language_tag//
% ~~~{.abnf}
% langtag = language
%           ["-" script]
%           ["-" region]
%           *("-" variant)
%           *("-" extension)
%           ["-" privateuse]
% ~~~

rfc5646_standard_language_tag(
  T0,
  Primary,
  Extended,
  Script,
  Region,
  Variants,
  Extensions,
  PrivateLanguage
) -->
  rfc5646_language(T1, Primary, Extended),
  (hyphen_minus, {H1 = '-'}, rfc5646_script(T2, Script) ; ""),
  (hyphen_minus, {H2 = '-'}, rfc5646_region(T3, Region) ; ""),
  (rfc5646_variants(T4, Variants) ; ""),
  (rfc5646_extensions(T5, Extensions) ; ""),
  (hyphen_minus, {H3 = '-'}, rfc5646_privateuse(T6, PrivateLanguage) ; ""),
  {parse_tree(standard_language_tag, [T1,H1,T2,H2,T3,T4,T5,H3,T6], T0)}.

%! rfc5646_variant(-Tree:compound, ?Variant:atom)//
% Variant subtags indicate additional, well-recognized variations that define
% a language or its dialects that are not covered by other available subtags.
%
% ~~~{.abnf}
% variant = 5*8alphanum / (DIGIT 3alphanum)
% ~~~
%
% ## Position in language tag
%
% Variant subtags MUST follow any primary language, extended language,
% script, or region subtags and MUST precede any extension or private use
% subtag sequences.
%
% More than one variant MAY be used to form the language tag.
%
% The same variant subtag MUST NOT be used more than once within
% a language tag.
%
% ## Relation to standards
%
% Variant subtags, as a collection, are not associated with any particular
% external standard.
%
% ## Formal properties
%
% In order to distinguish variants from other types of subtags, variants
% MUST meet the following length and content restrictions:
%   1.  Variant subtags that begin with a letter (a-z, A-Z) MUST be
%       at least 5 characters long.
%   2.  Variant subtags that begin with a digit (0-9) MUST be at
%       least 4 characters long.
%
% ## Combinations
%
% Most variants that share a prefix are mutually exclusive.
% For example, the German orthographic variations `1996` and `1901` SHOULD
% NOT be used in the same tag, as they represent the dates of different
% spelling reforms.
% A variant that can meaningfully be used in combination with another
% variant SHOULD include a 'Prefix' field in its registry record that lists
% that other variant.
% For example, if another German variant `example` were created that made
% sense to use with `1996`, then `example` should include two `Prefix`
% fields: `de` and `de-1996`.
%
% ## Examples
%
% `sl-nedis` represents the Natisone or Nadiza dialect of Slovenian.
%
% `de-CH-1996` represents German as used in Switzerland and as
% written using the spelling reform beginning in the year 1996 C.E.

rfc5646_variant(variant(Variant), Variant) -->
  decimal_digit(H),
  dcg_multi(ascii_alpha_numeric, 3, T),
  {
    atom_codes(Variant, [H|T]),
    rfc5646_class(Variant, 'Variant')
  }.
rfc5646_variant(variant(Variant), Variant) -->
  dcg_multi(ascii_alpha_numeric, 5-8, Variant, [convert(atom_codes)]),
  {rfc5646_class(Variant, 'Variant')}.

%! rfc5646_variants(-Tree:compound, ?Variants:list(atom))//

rfc5646_variants(variants('-',T1,T2), [H|T]) -->
  hyphen_minus,
  rfc5646_variant(T1, H),
  rfc5646_variants(T2, T).
rfc5646_variants(variants('-',T1), [H]) -->
  hyphen_minus,
  rfc5646_variant(T1, H).



:- begin_tests(rfc5646_dcg).

:- use_module(generics(print_ext)).
:- use_module(library(apply)).

rfc5646_atom('zh').
rfc5646_atom('zh-Latn').
rfc5646_atom('zh-Latn-CN').
rfc5646_atom('zh-Latn-CN-variant1').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1-x-wadegile').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1-x-wadegile-private1').

test(rfc5646_parse, [forall(rfc5646_atom(LanguageTag))]):-
  atom_codes(LanguageTag, Codes),
  once(phrase(rfc5646_language_tag(Tree, Term), Codes)),
  maplist(formatnl, [Tree, Term]).

:- end_tests(rfc5646_dcg).

