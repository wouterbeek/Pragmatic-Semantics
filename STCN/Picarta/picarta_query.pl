:- module(
  picarta_query,
  [
    picarta_query_ppn/3 % +PPN:atom
                        % -PicartaURI:uri
                        % -Pairs:list(pair)
  ]
).

/** <module> Picarta query

This module contains predicate for querying the Picarta online dataset
via its URI-baset/REST API.

@author Wouter Beek
@version 2013/01-2013/04, 2013/06, 2013/09-2013/10
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(atom_ext)).
:- use_module(html(html)).
:- use_module(library(debug)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(stcn(stcn_generic)).



picarta_authority('picarta.pica.nl').

%! picarta_attribute(?Their:atom, ?Our:atom) is nondet.
% Translates attributes from the Picarta Website to a name we think is
% more descriptive and clean.

picarta_attribute('Adres:\240\',                        address                ).
picarta_attribute('Alternatieve\240\titel:\240\',       alternative_title      ).
picarta_attribute('Annotatie:\240\',                    annotation             ).
picarta_attribute('Auteur:\240\',                       author                 ).
picarta_attribute('Beroep:\240\',                       profession             ).
picarta_attribute('Beroep\240\/\240\Plaats:\240\',      profession_location    ).
picarta_attribute('Collatie:\240\',                     collation              ).
picarta_attribute('Drukker\240\/\240\Uitgever:\240\',   printer_publisher      ).
picarta_attribute('Editie:\240\',                       edition                ).
picarta_attribute('Formaat:\240\',                      format                 ).
picarta_attribute('Geografisch\240\trefwoord:\240\',    geographic_keyword     ).
picarta_attribute('Impressum:\240\',                    imprint                ).
picarta_attribute('Jaar:\240\',                         year                   ).
picarta_attribute('Leefjaren:\240\',                    life_years                  ).
picarta_attribute('Meisjesnaam:\240\',                  maiden_name            ).
picarta_attribute('Naam:\240\',                         name_normal            ).
picarta_attribute('Naamsvariant:\240\',                 name_variant           ).
picarta_attribute('Onderwerpstrefwoord:\240\',          topical_keyword        ).
picarta_attribute('Plaats:\240\',                       location               ).
picarta_attribute('Pseudoniem\240\van:\240\',           pseudonym              ).
picarta_attribute('Signatuur:\240\',                    signature              ).
picarta_attribute('Synoniem:\240\',                     synonym                ).
picarta_attribute('Term:\240\',                         term                   ).
picarta_attribute('Titel:\240\',                        title                  ).
picarta_attribute('Toelichting:\240\',                  explanation            ).
picarta_attribute('Typografische\240\informatie:\240\', typographic_information).
picarta_attribute('Uithangbord:\240\',                  sign_board             ).
picarta_attribute('URL:\240\',                          url                    ).
picarta_attribute('Vertaler\240\/\240\Bewerker:\240\',  translator_editor      ).
picarta_attribute('Vingerafdruk:\240\',                 fingerprint            ).
picarta_attribute('Volledige\240\naam:\240\',           name_full              ).
picarta_attribute('Zie\240\ook:\240\',                  see_also               ).

picarta_path('/DB=3.11/SET=1/TTL=1/CMD').

%! picarta_query_ppn(+PPN:atom, -PicartaURI:uri, -Pairs:list) is det.
% Queries for the given PPN code, returning the attribute/value-pairs that
% are contained in the Picarta dataset for the given PPN code.

picarta_query_ppn(PPN, URI, Pairs):-
  picarta_scheme(Scheme),
  picarta_authority(Authority),
  picarta_path(Path),
  Fragment = '',
  atom_concat('ppn+', PPN, TRM_Value),
  uri_query_components(
    Search,
    ['ACT'='SRCHA','IKT'=1016,'REC'='*','SRT'='YOP','TRM'=TRM_Value]
  ),
  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, Search, Fragment)
  ),
  download_html([html_dialect(html4)], URI, HTML),
  findall(
    TranslatedAttribute/ProcessedValue,
    (
      xpath(HTML, //tr(@style='display:table-row;'), TR),
      xpath(TR, td(@class='rec_lable'), TD1),
      xpath(TD1, div, DIV1),
      xpath(DIV1, span, SPAN1),
      xpath(SPAN1, /self(text), UntranslatedAttribute),
      xpath(TR, td(@class='rec_title'), TD2),
      xpath(TD2, div, DIV2),
      findall(
        StrippedValue,
        (
          xpath(DIV2, span, element(span, [], [UnstrippedValue])),
          strip_value(UnstrippedValue, StrippedValue)
        ),
        StrippedValues
      ),
      atomic_list_concat(StrippedValues, ' ', ProcessedValue),
      % Do not include non-breaking spaces (ISO 8859-1 character set).
      (atom_codes(ProcessedValue, [160]) -> fail ; true),
      translate_attribute(UntranslatedAttribute, TranslatedAttribute)
    ),
    Pairs
  ), !.
picarta_query_ppn(PPN, _URI, []):-
  debug(picarta_query, 'PPN ~w could not be parsed in Picarta.', [PPN]).

picarta_scheme(http).

ppn_link(PPN) -->
  dcg_until([end_mode(inclusive)], ppn_link_prefix, _), !,
  ppn(PPN),
  dcg_all.

ppn_link_prefix --> "PPN=".
ppn_link_prefix --> "TRM=".

%! strip_link(+Value, -Content) is det.
% Strips links in the given value, extracting its contents.
% There are three cases:
%     1. The value is a link and the hyperlink contains a PPN code.
%     2. The value is a link but the hyperlink contains no PPN code.
%     3. The value is not a link, and it returned as is.

% If the link points to a PPN, then we take that PPN code as the value.
strip_link(element(a, Attributes, _Content), PPN):-
  memberchk(href=URI1, Attributes),
  atom_codes(URI1, URI2),
  phrase(ppn_link(PPN), URI2), !.
% If the link does not point to a PPN, then we take the content.
strip_link(element(a, _Attributes, [Content]), Content):- !.
% Non-link content is returned as-is.
strip_link(Content, Content).

%! strip_value(+UnstrippedValue, -StrippedValue:atom) is det.
% Values a stripped by first performing link extraction, and then by
% stripping the trailing spaces.

strip_value(UnstrippedValue, StrippedValue):-
  strip_link(UnstrippedValue, X),
  % Strip all leading and training white space.
  normalize_space(atom(StrippedValue), X).

%! translate_attribute(
%!   +UntranslatedAttribute:atom,
%!   -TranslatedAttribute:atom
%! ) is det.
% Traslates between values found in the Picarta website crawl and attributes
% as they are asserted in the Picarta SW dataset.

translate_attribute(UntranslatedAttribute, TranslatedAttribute):-
  picarta_attribute(UntranslatedAttribute, TranslatedAttribute), !.
translate_attribute(UntranslatedAttribute, UntranslatedAttribute):-
  debug(
    picarta,
    'Unregistered Picarta attribute: ~w.',
    [UntranslatedAttribute]
  ).

