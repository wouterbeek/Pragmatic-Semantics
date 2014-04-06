:- module(
  sa_scrape,
  [
    sa_scrape/1 % +Graph:atom
  ]
).

/** <module> Sackner Archive Scraper

Constructs a Semantic Web database based on the Sackner Archive data,
by scraping an online Web site.

# Number of entries clawled.

47.374 entries, with codes between 00.000 and 50.122.

@author Wouter Beek
@version 2013/04, 2014/03
*/

:- use_module(generics(atom_ext)).
:- use_module(html(html)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_image)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(swag(swag)).
:- use_module(swag(swag_db)).
:- use_module(xsd(xsd)).



%! sa_scrape(+Graph:atom) is det.
% Crawls the Sackner Archive for all the entries that it contains.

sa_scrape(G):-
  sa_assert_schema(G),
  sa_scrape(G, 0).


%! sa_scrape(+Graph:atom, +FirstEntry:nonneg) is det.
% Crawls the Sackner Archive as of the given entry number.
% This allows for easy continuing of crawls
%  that broke before being completed.

sa_scrape(Graph, FirstNumber):-
  rdfs_assert_subclass(swag:'Entry', rdfs:'Resource', Graph),
  rdfs_assert_label(swag:'Entry', 'Entry in the Sackner Archive', Graph),

  rdfs_assert_subclass(swag:'Property', rdf:'Property', Graph),
  rdfs_assert_label(swag:'Property', 'Property used in the Sackner Archive',
    Graph),

  % The last entry has identifier 50,122, as of 2013/03/14.
  findall(
    ignore(catch(sa_scrape_entry(Graph, Entry), E, print_message(error, E))),
    between(FirstNumber, 100000, Entry),
    Goals
  ),
  maplist(call, Goals).


%% sa_scrape_entry(+Graph:atom, +Entry:nonneg) is det.
% Scrapes the Sackner Archive for the specific entry
%  with the given identifier.

sa_scrape_entry(Graph, EntryId1):-
  % The entry identifier has to be padded with zeros.
  format_integer(EntryId1, 5, EntryId2),

  rdf_create_next_resource(swag, 'Entry', Entry, Graph),
  xsd_lexical_map(xsd:integer, EntryId2, XsdValue),
  rdf_assert_datatype(Entry, swag:original_id, XsdValue, xsd:integer, Graph),

  atomic_concat('413201333230~', EntryId2, TemporaryNumber),
  uri_components(
    DescriptionUri,
    uri_components(
      http,
      'ww3.rediscov.com',
      '/sacknerarchives/ShowItem.aspx',
      TemporaryNumber,
      ''
    )
  ),
  download_html(
    [html_dialect(html4),never_give_up(true)],
    DescriptionUri,
    Html
  ),

  xpath_chk(Html, //table, Table),

  findall(
    PredicateName-Value,
    sa_nvpair(Table, PredicateName, Value),
    Pairs
  ),
  maplist(sa_assert_triple(Graph, Entry), Pairs),

  xpath_chk(Table, //tr(2)/td(2)/p, P),
  findall(
    ImageName,
    (
      xpath(P, input(@src), ImageSubpath1),
      downcase_atom(ImageSubpath1, ImageSubpath2),
      atom_concat('thumb\\', ImageName, ImageSubpath2),
      ImageName \== 'blank.jpg'
    ),
    ImageNames
  ),
  maplist(crawl_image(Graph, Entry), ImageNames).


sa_nvpair(Table, PredicateName2, Value2):-
  xpath(Table, //tr, Row),
  xpath_chk(Row, //td(1)/span(normalize_space), PredicateName1),
  PredicateName1 \== '',
  xpath_chk(Row, //td(2)/p, P),
  (
    xpath_chk(P, //input(@value), Values1)
  ;
    xpath_chk(P, //textarea(normalize_space), Values1)
  ),

  % Some values are enumarations separated by dashes.
  atomic_list_concat(Values2, ' --', Values1),
  member(Value1, Values2),

  % Some values have superfluous spaces pre- and/or postfixed.
  strip_atom([' '], Value1, Value2),
  once(sa_predicate_term(PredicateName1, PredicateName2, _)).


sa_assert_schema(G):-
  % Assert descriptive labels for the properties.
  forall(
    sa_predicate_term(_, PropertyName1, RdfsLabel),
    (
      atomic_list_concat(['Property',PropertyName1], '/', PropertyName2),
      rdf_global_id(swag:PropertyName2, Property),
      rdfs_assert_label(Property, en, RdfsLabel, G)
    )
  ),

  % Assert the domain and range restrictions of the properties.
  rdfs_assert_domain(swag:number_of_artist_proofs, swag:'Entry', G),
  rdfs_assert_range( swag:number_of_artist_proofs, xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_images,        swag:'Entry', G),
  rdfs_assert_range( swag:number_of_images,        xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_art_proofs,    swag:'Entry', G),
  rdfs_assert_range( swag:number_of_art_proofs,    xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_letter_copies, swag:'Entry', G),
  rdfs_assert_range( swag:number_of_letter_copies, xsd:integer,  G),
  rdfs_assert_domain(swag:announcement,            swag:'Entry', G),
  rdfs_assert_range( swag:announcement,            xsd:string,   G),
  rdfs_assert_domain(swag:annotation,              swag:'Entry', G),
  rdfs_assert_range( swag:annotation,              xsd:string,   G),
  rdfs_assert_domain(swag:author,                  swag:'Entry', G),
  rdfs_assert_range( swag:author,                  xsd:string,   G),
  rdfs_assert_domain(swag:catalog,                 swag:'Entry', G),
  rdfs_assert_range( swag:catalog,                 xsd:string,   G),
  rdfs_assert_domain(swag:city_country,            swag:'Entry', G),
  rdfs_assert_range( swag:city_country,            xsd:string,   G),
  rdfs_assert_domain(swag:classification,          swag:'Entry', G),
  rdfs_assert_range( swag:classification,          xsd:string,   G),
  rdfs_assert_domain(swag:container,               swag:'Entry', G),
  rdfs_assert_range( swag:container,               xsd:string,   G),
  rdfs_assert_domain(swag:contributor,             swag:'Entry', G),
  rdfs_assert_range( swag:contributor,             xsd:string,   G),
  rdfs_assert_domain(swag:exhibition_announcement, swag:'Entry', G),
  rdfs_assert_range( swag:exhibition_announcement, xsd:string,   G),
  rdfs_assert_domain(swag:exhibition_catalog,      swag:'Entry', G),
  rdfs_assert_range( swag:exhibition_catalog,      xsd:string,   G),
  rdfs_assert_domain(swag:dimensions,              swag:'Entry', G),
  rdfs_assert_range( swag:dimensions,              wb:box,       G),
  rdfs_assert_domain(swag:illustration_bwc,        swag:'Entry', G),
  rdfs_assert_range( swag:illustration_bwc,        xsd:string,   G),
  rdfs_assert_domain(swag:inscribed,               swag:'Entry', G),
  rdfs_assert_range( swag:inscribed,               xsd:string,   G),
  rdfs_assert_domain(swag:language,                swag:'Entry', G),
  rdfs_assert_range( swag:language,                xsd:string,   G),
  rdfs_assert_domain(swag:media,                   swag:'Entry', G),
  rdfs_assert_range( swag:media,                   xsd:string,   G),
  rdfs_assert_domain(swag:nationality,             swag:'Entry', G),
  rdfs_assert_range( swag:nationality,             xsd:string,   G),
  rdfs_assert_domain(swag:number_of_dups,          swag:'Entry', G),
  rdfs_assert_range( swag:number_of_dups,          xsd:decimal,  G),
  rdfs_assert_domain(swag:number_series_month,     swag:'Entry', G),
  rdfs_assert_range( swag:number_series_month,     xsd:string,   G),
  rdfs_assert_domain(swag:number_of_pages,         swag:'Entry', G),
  rdfs_assert_range( swag:number_of_pages,         xsd:integer,  G),
  rdfs_assert_domain(swag:periodical,              swag:'Entry', G),
  rdfs_assert_range( swag:periodical,              xsd:string,   G),
  rdfs_assert_domain(swag:publisher,               swag:'Entry', G),
  rdfs_assert_range( swag:publisher,               xsd:string,   G),
  rdfs_assert_domain(swag:purchase_year,           swag:'Entry', G),
  rdfs_assert_range( swag:purchase_year,           xsd:gYear,    G),
  rdfs_assert_domain(swag:series,                  swag:'Entry', G),
  rdfs_assert_range( swag:series,                  xsd:string,   G),
  rdfs_assert_domain(swag:signature,               swag:'Entry', G),
  rdfs_assert_range( swag:signature,               xsd:string,   G),
  rdfs_assert_domain(swag:subtitle_author,         swag:'Entry', G),
  rdfs_assert_range( swag:subtitle_author,         xsd:string,   G),
  rdfs_assert_domain(swag:subtitle,                swag:'Entry', G),
  rdfs_assert_range( swag:subtitle,                xsd:string,   G),
  rdfs_assert_domain(swag:title,                   swag:'Entry', G),
  rdfs_assert_range( swag:title,                   xsd:string,   G),
  rdfs_assert_domain(swag:number_of_copies,        swag:'Entry', G),
  rdfs_assert_range( swag:number_of_copies,        xsd:integer,  G),
  rdfs_assert_domain(swag:translator,              swag:'Entry', G),
  rdfs_assert_range( swag:translator,              xsd:string,   G),
  rdfs_assert_domain(swag:volume,                  swag:'Entry', G),
  rdfs_assert_range( swag:volume,                  xsd:string,   G),
  rdfs_assert_domain(swag:year,                    swag:'Entry', G),
  rdfs_assert_range( swag:year,                    xsd:gYear,    G).


sa_assert_triple(Graph, Entry, PredicateName-Value):-
  rdf_global_id(swag:PredicateName, Predicate),
  rdf_assert_string(Entry, Predicate, Value, Graph).


%! crawl_image(+Graph:atom, +Entry:integer, -ImageName:atom) is det.
% Returns an image for the given entry in the Sackner Archive.
%
% The image is retrieved from the server of the Sackner Archive and is
%  then stored locally.
%
% If an entry has more than one image, then subsequent runs of this
%  predicate will return those additional images.
%
% If the entry has no more images, then this method succeeds without
%  instantiating =|ImageName|=.

crawl_image(Graph, Entry, Name):-
  atomic_concat('/sacknerarchives/FULL/', Name, Path),
  uri_components(URL, uri_components(http, 'ww3.rediscov.com', Path, _, _)),
  rdf_assert_image([], Entry, swag:image, URL, Graph), !.
crawl_image(Graph, Entry, Name):-
  gtrace, %DEB
  crawl_image(Graph, Entry, Name).


%% sa_predicate_term(?Legacy:atom, ?Property:atom, ?Label:atom) is nondet.

sa_predicate_term('# Artist Proofs:',         number_of_artist_proofs, 'Number of artist proofs'     ).
sa_predicate_term('# Images:',                number_of_images,        'Number of images'            ).
sa_predicate_term('# Letter Art Proofs:',     number_of_art_proofs,    'Number of art proofs'        ).
sa_predicate_term('# Letter Copies:',         number_of_letter_copies, 'Number of letter copies'     ).
sa_predicate_term('Announcement:',            announcement,            'Announcement'                ).
sa_predicate_term('Annotation:',              annotation,              'Annotation'                  ).
sa_predicate_term('Author:',                  author,                  'Author'                      ).
sa_predicate_term('Catalog:',                 catalog,                 'Catalog'                     ).
sa_predicate_term('City County:',             city_country,            'City and/or country'         ).
sa_predicate_term('Classification:',          classification,          'Classification'              ).
sa_predicate_term('Container:',               container,               'Container'                   ).
sa_predicate_term('Contributors:',            contributor,             'Contributor'                 ).
sa_predicate_term('Exhibition Announcement:', exhibition_announcement, 'Exhibition announcement'     ).
sa_predicate_term('Exhibition Catalog:',      exhibition_catalog,      'Exhibition catalog'          ).
sa_predicate_term('Ht Wdt Dpth:',             dimensions,              'Dimensions'                  ).
sa_predicate_term('Illus BWC:',               illustration_bwc,        'Illustration BWC'            ).
sa_predicate_term('Inscribed:',               inscribed,               'Inscribed'                   ).
sa_predicate_term('Language:',                language,                'Language'                    ).
sa_predicate_term('Media:',                   media,                   'Media'                       ).
sa_predicate_term('Nationality:',             nationality,             'Nationality'                 ).
sa_predicate_term('Number of Dups:',          number_of_dups,          'Number of duplicates'        ).
sa_predicate_term('Nbr Ser Mn:',              number_series_month,     'Magazine number/series/month').
sa_predicate_term('Pages:',                   number_of_pages,         'Number of pages'             ).
sa_predicate_term('Periodical:',              periodical,              'Periodical'                  ).
sa_predicate_term('Publisher:',               publisher,               'Publisher'                   ).
sa_predicate_term('Purchase Year:',           purchase_year,           'Year of purchase'            ).
sa_predicate_term('Series:',                  series,                  'Series'                      ).
sa_predicate_term('Signature:',               signature,               'Signature'                   ).
sa_predicate_term('Sub Tit Au:',              subtitle_author,         'Subtitle author'             ).
sa_predicate_term('Subtitle:',                subtitle,                'Subtitle'                    ).
sa_predicate_term('Title:',                   title,                   'Title'                       ).
sa_predicate_term('Total Copies:',            number_of_copies,        'Number of copies'            ).
sa_predicate_term('Translator:',              translator,              'Translator'                  ).
sa_predicate_term('Volume:',                  volume,                  'Volume'                      ).
sa_predicate_term('Year:',                    year,                    'Year'                        ).
sa_predicate_term(Name, _, _):-
  nonvar(Name),
  gtrace, %DEB
  format(user_output, '~a', [Name]).

