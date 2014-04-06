:- module(
  kmc_4043,
  [
    assert_schema_kmc_4043/1, % +Graph:graph
    kmc_4043//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc4043/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 4043 - BOOKSELLER/PUBLISHER

We expect that there are 222.585 publication/printer-pairs in the
redactiebladen. This number is obtained using the search string "\n4043[^ ]".

# Parse cities

E.g. PPN 234597046.

!!kmc_4043!!city_printer!!2!!

# Parse errors/exceptions

## PPN 173117708

@author Wouter Beek
@version 2013/03, 2013/06, 2013/09, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



assert_schema_kmc_4043(G):-
  rdfs_assert_class(stcnv:'Printer', G),
  rdfs_assert_label(stcnv:'Printer', drukker, nl, G),
  
  rdf_assert_property(stcnv:printer, G),
  rdfs_assert_label(stcnv:printer, 'heeft drukker', nl, G),
  rdf_assert_string(stcnv:printer, stcnv:kb_name, 'KMC 4043', G),
  rdfs_assert_seeAlso(stcnv:printer,
      'http://www.kb.nl/kbhtml/stcnhandleiding/4043.html', G),
  rdf_assert_language_tagged_string(stcnv:printer, stcnv:picarta_name,
      'Drukker / Uitgever', nl, G),
  
  rdfs_assert_label(stcnv:'City', stad, nl, G),
  % Assert the cities that act as printers/publishers.
  forall(
    city_printer(CityName, PPN),
    (
      rdf_global_id(stcn:PPN, Resource),
      rdf_assert_individual(Resource, stcnv:'City', G),
      rdfs_assert_label(Resource, CityName, G)
    )
  ).

%! city_printer(?City:atom, ?PPN:atom) is nondet.
% A 'printer' of books that is only indicated with a city name.

city_printer('Aachen', '075569140').
city_printer('Alkmaar', '075565943').
city_printer('Alphen aan den Rijn', '1533 39799').
city_printer('Amersfoort', '075565951').
city_printer('Amsterdam', '07556596X').
city_printer('Antwerpen', '075567075').
city_printer('Arnhem', '075565978').
city_printer('Assen', '1679 64283').
city_printer('Batavia', '075569795').
city_printer('Bergen op Zoom', '075565986').
city_printer('Berlin', '075569582').
city_printer('Beverwijk', '075565994').
city_printer('Blokzijl', '1701 34997').
city_printer('Bologna', '075569531').
city_printer('Bolsward', '075566001').
city_printer('Bonn', '075570645').
city_printer('Borculo', '07556601X').
city_printer('Braunschweig', '1717 05831').
city_printer('Breda', '075566028').
city_printer('Bremen', '1769 61879').
city_printer('Brielle', '075566036').
city_printer('Brussel', '075571404').
city_printer('Buiksloot', '075566044').
city_printer('Constantinopel', '1439 32128').
city_printer('Culemborg', '075566052').
city_printer('Delft', '075566060').
city_printer('Delfzijl', '075566079').
city_printer('Deventer', '075566087').
city_printer('Dokkum', '075566095').
city_printer('Dordrecht', '075566109').
city_printer('Douai', '075566850').
city_printer('Dresden', '07556887X').
city_printer('Dunkerque', '1758 79281').
city_printer('Düsseldorf', '075568861').
city_printer('Edinburgh', '075570076').
city_printer('Emden', '07556873X').
city_printer('Emmerich', '075568640').
city_printer('Enkhuizen', '075566117').
city_printer('Franeker', '075566125').
city_printer('Frankfurt am Main', '075567385').
city_printer('Frankfurt a/d Oder', '075571013').
city_printer('Frederikstad', '075567946').
city_printer('Gdansk', '0860 65920').
city_printer('Genève', '07556856X').
city_printer('Gent', '1565 27405').
city_printer('Glückstadt', '075568446').
city_printer('Goes', '075566133').
city_printer('Gorinchem', '075566141').
city_printer('Gouda', '07556615X').
city_printer('\'s-Graveland', '170214281').
city_printer('\'s-Gravenhage', '075566168').
city_printer('Groenland', '165579471').
city_printer('Groenlo', '1683 6641X').
city_printer('Groningen', '075566176').
city_printer('Haarlem', '075566184').
city_printer('Haastrecht', '075566192').
city_printer('Halle', '183432746').
city_printer('Hamburg', '07556761X').
city_printer('Harderwijk', '075566206').
city_printer('Harlingen', '075566214').
city_printer('Hasselt', '075566222').
city_printer('Hattem', '172964709').
city_printer('Hellevoetsluis', '075566230').
city_printer('\'s-Hertogenbosch', '075566249').
city_printer('Heusden', '075566265').
city_printer('Hoorn', '075566257').
city_printer('Jerusalem', '07557148X').
city_printer('Kampen', '075566273').
city_printer('Kevelaer', '075567326').
city_printer('Kleve', '075569914').
city_printer('Kobenhavn', '075570491').
city_printer('Köln', '075567032').
city_printer('Kralingen', '075566281').
city_printer('Krommenie', '07556629X').
city_printer('La Rochelle', '075570548').
city_printer('Leerdam', '075566303').
city_printer('Leeuwarden', '075566311').
city_printer('Leiden', '075566338').
city_printer('Leiderdorp', '075577968').
city_printer('Leipzig', '1408 60169').
city_printer('Leuven', '075577321').
city_printer('Lexmond', '07556632X').
city_printer('Lille', '1752 31400').
city_printer('Lisboa', '075569809').
city_printer('Loenen', '075566354').
city_printer('Loevestein', '075566346').
city_printer('London', '075566842').
city_printer('Luik', '188045376').
city_printer('Lund', '075571498').
city_printer('Lyon', '101111185').
city_printer('Maastricht', '075566362').
city_printer('Madrid', '075570084').
city_printer('Mainz', '075569167').
city_printer('Medemblik', '075566370').
city_printer('Middelburg', '075566389').
city_printer('Milano', '1450 00567').
city_printer('Monnikendam', '075566397').
city_printer('Münster', '075569922').
city_printer('Muiden', '075566400').
city_printer('Naarden', '174348916').
city_printer('Nazareth', '180680072').
city_printer('Nîmes', '075568950').
city_printer('Noordbroek', '075566419').
city_printer('Norden (Oost-Friesland)', '109751809').
city_printer('Nijmegen', '075566427').
city_printer('Ootmarsum', '075566435').
city_printer('Oudewater', '075566443').
city_printer('Overschie', '075566451').
city_printer('Oxford', '075571447').
city_printer('Padova', '075570122').
city_printer('Paris', '075566885').
city_printer('Praha', '075571285').
city_printer('Purmerend', '07556646X').
city_printer('Raków', '075568454').
city_printer('Roermond', '075566753').
city_printer('Roma', '075566893').
city_printer('Rotterdam', '075566478').
city_printer('Rouen', '075566826').
city_printer('De Rijp', '075566486').
city_printer('Saumur', '110486277').
city_printer('Schagen', '075566494').
city_printer('Schiedam', '175912351').
city_printer('Schoonhoven', '075566516').
city_printer('Schoorl', '17204779X').
city_printer('Sevilla', '180897543').
city_printer('Sloten', '165725826').
city_printer('Sneek', '075566524').
city_printer('Spaarndam', '075566532').
city_printer('Steenwijk', '075566540').
city_printer('St. Germain en Laye', '075567989').
city_printer('St. Omer', '174888554').
city_printer('Stockholm', '075570203').
city_printer('Terneuzen', '075566559').
city_printer('Tholen', '075566567').
city_printer('Tiel', '075566575').
city_printer('Tournai', '15714903X').
city_printer('Tours', '075568802').
city_printer('Utrecht', '075566583').
city_printer('Veenendaal', '075566591').
city_printer('Veere', '075566605').
city_printer('Vianen', '075566613').
city_printer('Vierlingsbeek', '157485005').
city_printer('Villafranca', '07556789X').
city_printer('Vlaardingen', '075576961').
city_printer('Vlissingen', '07556663X').
city_printer('Voorburg', '075566648').
city_printer('Vrijburg', '090875923').
city_printer('Warnsveld', '168367130').
city_printer('Watou (Watten)', '173743838').
city_printer('Weesp', '075566656').
city_printer('Wesel', '075568071').
city_printer('Westzaan', '163012571').
city_printer('Wien', '075570181').
city_printer('Willemstad', '075566664').
city_printer('Woerden', '075566672').
city_printer('Wormerveer', '075566680').
city_printer('Zaandam', '075566699').
city_printer('Zaltbommel', '075566702').
city_printer('Zierikzee', '075566710').
city_printer('Zutphen', '075566729').
city_printer('Zwammerdam', '172217148').
city_printer('Zwolle', '075566737').

kmc_4043(_G, PPN) -->
  dcg_until([end_mode(exclusive)], exclamation_mark, _Codes),
  % PPN codes for printers can contain non-numbers (just like any other PPN).
  ppn('Printer', PrinterPPN),
  exclamation_mark,
  {
    ppn_resource(printer_publisher, PrinterPPN, Printer),
    rdf_assert(PPN, stcnv:printer, Printer, stcn)
  }.
% E.g. PPN 317155091.
kmc_4043(_G, _PPN) -->
  atom('THESAUREREN').
% E.g. PPN 234597046.
kmc_4043(G, PPN) -->
  word(PrinterName),
  {
    once(city_printer(PrinterName, PPN)),
    ppn_resource(printer_publisher, PPN, Printer),
    rdf_assert(PPN, stcnv:printer, Printer, G)
  }.

statistics_kmc4043(_G, []).

