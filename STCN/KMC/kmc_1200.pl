:- module(
  kmc_1200,
  [
    assert_schema_kmc_1200/1, % +Graph:graph
    kmc_1200//2, % +Graph:atom
                 % +PPN:uri
    kmc_1200_picarta//2, % +Graph:atom
                         % +PPN:uri
    statistics_kmc_1200/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 1200 - Typographic properties

Verplicht veld. Herhaalbaar.

KMC 1200 bevat de typografische kenmerken.

Vorm: één letter

# Parsing problems

## 1

PPN 14162843X seems to use the KMC code 1200 mistakenly.

## 2

'1' (the number between 0 and 2) is assumed to be a typo of 'l'
(the letter between 'k' and 'm'), e.g. PPN 321339320.

## 3

'X' (uppercase x) is assumed to be a type of 'x' (lowercase x),
e.g. PPN 298324997.

## 4

Some PPNs, e.g., PPN 271591978, have the undocumented value '5'.

## 5

Some PPNs, e.g., PPN 301032831, have the undocumented value '9'.

## 6

'w.' (with the dot) is assumed to be a typo of 'w' (without the dot),
e.g. PPN 204513316.

This one is particularly nasty, since it implies that the typographic property
cannot be assumed to be a single character.

## 7

'v' occurs in the STCN documentation, but not in the GGC documentation.

## 8

PPN 14162843X uses this code wrongly. The parser breaks on this.

## 9

PPN 261654624 uses 'b)' which is assumend to be a typo of 'b'.

## 10

'Y' is assumed to be a typo of 'y', e.g. PPN 305305972.

## 11

'C' --> 'c'
PPN 314559655

# Titelpagina

  * x, typografische titelpagina
  * y, geen titelpagina
  * a, illustratie op de titelpagina
       Op te vatten in ruime zin, dus ook een gegraveerd titelblad,
       een frontispice, een auteursportret voor of na de titel,
       een illustratie op de bedrukte omslag, enz., doch niet vignetten
       en dergelijke als typografisch materiaal op te vatten
       boekversieringen.
  * w, gegraveerde titelpagina
       Hieronder wordt verstaan een titelblad dat geheel door middel van
       een gravure of houtsnede is vervaardigd. Een gegraveerde titelpagina
       hoeft niet per definitie een illustratie te bevatten, maar kan louter
       uit gegraveerde tekst bestaan; in het eerste geval krijgt hij zowel
       code =a= als code =w=, in het tweede geval alleen code =w=. Een
       titelpagina die bijna geheel uit een gravure of houtsnede bestaat,
       maar waarvan de titel gevormd is door middel van zetsel, is een
       typografische titelpagina, met illustratie. (N.B. Het verschil
       tussen een gegraveerd titelblad en een frontispice is de aan- resp.
       afwezigheid van titel of impressum. Zowel gegraveerde titelpagina als
       frontispice worden opgenomen in de collatieformule.)
  * z, meerkleurig
       Ook als het gehele werk in een kleur (dus niet zwart) is gedrukt.
  * h, drukkersmerk
       Vignetten van bij voorbeeld de Amsterdamse Academie (de Bijenkorf)
       of van Nil Volentibus Arduum worden niet beschouwd als drukkersmerk,
       maar als illustratie op de titelpagina (a).
  * v, bedrukte omslag
       Een bedrukte omslag kan voorkomen naast een typografische of een
       gegraveerde titelpagina, maar ook in plaats daarvan, zie kmc 4000.
       Gegevens afkomstig van een bedrukte omslag zoals de prijs of een
       boekenlijst, krijgen ook de betreffende code.

 # Lettertype

 NB: Voor Romein, Gotisch, Grieks, Hebreeuws, Arabisch, Armeens
 en Cyrillish geldt dat er meer in dit lettertype gezet moet zijn dan een
 enkel citaat, dus substantieële tekstgedeelten.
   * i, romein
   * j, gotisch
   * k, cursief
   * l, civilité
     Klik hier voor een impressum in civilité.
   * m, Grieks
   * n, Hebreeuws
   * o, Arabisch
   * p, Armeens
   * r, cyrillisch
   * q, muzieknoten
     Ook als er maar één muzieknoot in de hele tekst staat.
   * s, overige

 # Illustraties

   * a, illustratie op de titelpagina
     Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een
     frontispice, een auteursportret voor of na de titel, een illustratie
     op de bedrukte omslag, enz., doch niet vignetten en dergelijke als
     typografisch materiaal op te vatten boekversieringen.
   * b, illustraties buiten collatie
     Als illustraties gelden ook kaarten.
   * c, andere illustraties binnen collatie
     Dit zijn alle illustraties binnen de collatie, met uitzondering van
     degene die code =a= krijgen. Als een boek geheel of gedeeltelijk uit
     gegraveerde bladen bestaat en de collatie luidt: `engraved folia',
     dan vallen deze illustraties binnen de collatie, en wordt dus kenmerk
     =c= toegekend.

 # Boekenlijsten

Waar mogelijk worden van deze lijsten kopieën gemaakt met aangeniet een
kopie van de tp. (zie ook kmc 7134)
   * d, van auteur	(geen kopie maken)
   * e, fondslijst
     De uitgever heeft de boeken gedrukt of houdt het kopijrecht en biedt
     ze in groten getale te koop (bij voorbeeld: `Catalogus van eenige
     Nederduytse boeken die gedrukt of te bekoomen zyn by Boudewyn vander
     Aa').
   * f, assortimentslijst
     Exemplaren van deze boeken zijn in de betreffende boekwinkel te koop
     (bij voorbeeld: `Register van Nederduytse reghts-geleerde boeken,
     dewelke [...] tot Amsterdam, by Hendrik en Dirk Boom, boekverkoopers,
     te bekomen zijn').
   * g, diversen
     Hieronder vallen bij voorbeeld veilingcatalogi, maar ook advertenties.
     Onder advertenties verstaan we aankondigingen van een of enkele boeken
     in een lopende tekst.

 # Diversen

   * 3, lijst van intekenaren
     Lijsten van intekenaren en oproepen tot intekening komen vooral voor
     als onderdeel van voor- of nawerk, maar kunnen ook, als een soort
     prospectus, zelfstandig voorkomen. Zelfstandig uitgekomen advertenties
     voor de inschrijving op een boek (kan ook een reeks of tijdschrift
     zijn), zonder lijst van intekenaars, krijgen een '3'en een 'g' en,
     indien uit de titel niet al duidelijk blijkt dat het om een
     inschrijving gaat, in kmc 4201 de annotatie Proposal for printing by
     subscription. Tevens komt de auteur van het beoogde werk in kmc 3011 en
     wordt ontsloten op Documentary information EN het onderwerp van het
     betreffende boek. Gaat het niet om een intekenlijst maar om een
     advertentie-achtig blaadje, dan 4201 advertisement brochure. Gebruik in
     het Engels niet het woord 'prospectus', dat is meer voor bedrijven of
     scholen die zichzelf aanprijzen. Boekverkopers die als intekenadres op
     zulke advertenties voorkomen, worden in beschrijving en thesaurus
     verder genegeerd, maar er wordt wel een annotatie toegevoegd (zie kmc
     4040).
   * 4, prijsopgave
     Ook bij opmerkingen als 'gratis' of 'bijdrage voor de armen'
   * 8, boekverkoperslijst
     De 8 wordt door het controlescript toegevoegd indien aanwezig in 700X.

@author Wouter Beek
@tbd http://www.kb.nl/kbhtml/stcnhandleiding/1200.html
@tbd http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-1200
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(picarta, 'http://picarta.pica.nl/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



% SCHEMA %

assert_schema_kmc_1200(G):-
  % Parent class of typographic values.
  rdfs_assert_class(stcnv:'TypografischKenmerk', G),

  % Classes of typograhic values.
  forall(
    kmc_1200_table(Category1, Label, Comment),
    (
      atomic_concat(Category1, 'TypografischKenmerk', Category2),
      rdf_global_id(stcnv:Category2, Category3),
      rdfs_assert_subclass(Category3, stcnv:'TypografischKenmerk', G),
      rdfs_assert_label(Category3, Label, G),
      (Comment == '', ! ; rdfs_assert_comment(Category3, Comment, G))
    )
  ),

  % Typographic values.
  forall(
    kmc_1200_table(Category1, Char1, Label, Comment),
    (
      atomic_concat(Category1, 'TypografischKenmerk', Category2),
      rdf_global_id(stcnv:Category2, Category3),
      atomic_concat(tk_, Char1, Char2),
      rdf_global_id(stcnv:Char2, Char3),
      rdf_assert_individual(Char3, Category3, G),
      rdfs_assert_label(Char3, Label, G),
      (Comment == '', ! ; rdfs_assert_comment(Char3, Comment, G))
    )
  ),

  % Typographic value relation.
  rdf_assert_property(stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:typographic_property, 'typographic property', en,
      G),
  rdfs_assert_label(stcnv:typographic_property, 'typografische eigenschap',
      nl, G),
  rdf_assert_string(stcnv:typographic_property, stcnv:kb_name, 'KMC 1200', G),
  rdfs_assert_seeAlso(stcnv:typographic_property,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1200.html', G),
  rdf_assert_language_tagged_string(stcnv:typographic_property,
      stcnv:picarta_name, 'Typografische informatie', nl, G),
  rdfs_assert_domain(stcnv:typographic_property, stcnv:'Publication', G),
  rdfs_assert_range(stcnv:typographic_property, stcnv:'TypografischKenmerk',
      G),

  rdfs_assert_subproperty(stcnv:boekenlijst, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:boekenlijst, 'list of books', en, G),
  rdfs_assert_label(stcnv:boekenlijst, boekenlijst, nl, G),
  rdfs_assert_range(stcnv:boekenlijst,
      stcn:'TypografischKenmerk/Boekenlijsten', G),

  rdfs_assert_subproperty(stcnv:lettertype, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:lettertype, 'font type', en, G),
  rdfs_assert_label(stcnv:lettertype, lettertype, nl, G),
  rdfs_assert_range(stcnv:lettertype, stcnv:'TypografischKenmerk/Lettertype',
      G),

  rdfs_assert_subproperty(stcnv:illustraties, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:illustraties, illustrations, en, G),
  rdfs_assert_label(stcnv:illustraties, illustraties, nl, G),
  rdfs_assert_range(stcnv:illustraties,
      stcnv:'TypografischKenmerk/Illustraties', G),

  rdfs_assert_subproperty(stcnv:diversen, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:diversen, miscellaneous, en, G),
  rdfs_assert_label(stcnv:diversen, diversen, nl, G),
  rdfs_assert_range(stcnv:diversen, stcnv:'TypografischKenmerk/Diversen', G),

  rdfs_assert_subproperty(stcnv:titelpagina, stcnv:typographic_property, G),
  rdfs_assert_label(stcnv:titelpagina, titlepage, en, G),
  rdfs_assert_label(stcnv:titelpagina, titelpagina, nl, G),
  rdfs_assert_range(stcnv:titelpagina,
      stcnv:'TypografischKenmerk/Titelpagina', G).



% DOMAIN KNOWLEDGE %

%! kmc_1200_table(?Category:atom, ?Label:atom, ?Comment:atom) is nondet.

kmc_1200_table('Boekenlijsten', boekenlijsten,
  'Waar mogelijk worden van deze lijsten kopieën gemaakt\c
   met aangeniet een kopie van de tp. (zie ook kmc 7134).').
kmc_1200_table('Diversen', diversen, '').
kmc_1200_table('Illustraties', illustraties, '').
kmc_1200_table('Lettertype', lettertype,
  'NB: Voor Romein, Gotisch, Grieks, Hebreeuws, Arabisch, Armeens en Cyrillish\c
   geldt dat er meer in dit lettertype gezet moet zijn dan een enkel citaat,\c
   dus substantieële tekstgedeelten.').
kmc_1200_table('Titelpagina', titelpagina, '').

%! kmc_1200_table(?Category:atom, ?Char:char, ?Label:atom, ?Comment:atom) is nondet.

kmc_1200_table('Boekenlijsten', d, 'van auteur', '(geen kopie maken)').
kmc_1200_table('Boekenlijsten', e, fondslijst,
  'De uitgever heeft de boeken gedrukt of\c
   houdt het kopijrecht en biedt ze in groten getale te koop (bij voorbeeld:\c
   `Catalogus van eenige Nederduytse boeken die gedrukt of te bekoomen zyn by\c
   Boudewyn vander Aa\').').
kmc_1200_table('Boekenlijsten', f, assortment,
  'Exemplaren van deze boeken zijn in de betreffende\c
   boekwinkel te koop (bij voorbeeld: `Register van Nederduytse reghts-geleerde\c
   boeken, dewelke [...] tot Amsterdam, by Hendrik en Dirk Boom, boekverkoopers,\c
   te bekomen zijn\').').
kmc_1200_table('Boekenlijsten', g, diverse,
  'Hieronder vallen bij voorbeeld veilingcatalogi,\c
   maar ook advertenties. Onder advertenties verstaan we aankondigingen van\c
   een of enkele boeken in een lopende tekst.').
kmc_1200_table('Diversen', '3', 'lijst van intekenaren',
  'Lijsten van intekenaren en oproepen tot intekening komen vooral voor als\c
   onderdeel van voor- of nawerk, maar kunnen ook, als een soort prospectus,\c
   zelfstandig voorkomen. Zelfstandig uitgekomen advertenties voor de\c
   inschrijving op een boek (kan ook een reeks of tijdschrift zijn), zonder\c
   lijst van intekenaars, krijgen een \'3\' en een \'g\' en, indien uit de\c
   titel niet al duidelijk blijkt dat het om een inschrijving gaat, in\c
   kmc 4201 de annotatie Proposal for printing by subscription. Tevens komt\c
   de auteur van het beoogde werk in kmc 3011 en wordt ontsloten op\c
   Documentary information EN het onderwerp van het betreffende boek. Gaat\c
   het niet om een intekenlijst maar om een advertentie-achtig blaadje,\c
   dan 4201 advertisement brochure. Gebruik in het Engels niet het woord\c
   \'prospectus\', dat is meer voor bedrijven of scholen die zichzelf\c
   aanprijzen.\c
   Boekverkopers die als intekenadres op zulke advertenties voorkomen,\c
   worden in beschrijving en thesaurus verder genegeerd, maar er wordt wel\c
   een annotatie toegevoegd (zie kmc 4040).').
kmc_1200_table('Diversen', '4', prijsopgave,
  'Ook bij opmerkingen als \'gratis\' of \'bijdrage voor de armen\'.').
kmc_1200_table('Diversen', '8', boekverkoperslijst,
  'De 8 wordt door het controlescript toegevoegd indien aanwezig in 700X.').
% Some PPNs, e.g., 271591978, have this value which is undocumented.
kmc_1200_table('Diversen', '5', onbekend, '').
% Some PPNs, e.g., 301032831, have this value which is undocumented.
kmc_1200_table('Diversen', '9', onbekend, '').
kmc_1200_table('Illustraties', a, 'illustratie op de titelpagina',
  'Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een frontispice,\c
   een auteursportret voor of na de titel, een illustratie op de bedrukte omslag,\c
   enz., doch niet vignetten en dergelijke als typografisch materiaal op te\c
   vatten boekversieringen.').
kmc_1200_table('Illustraties', b, 'illustraties buiten collatie',
  'Als illustraties gelden ook kaarten.').
kmc_1200_table('Illustraties', c,  'andere illustraties binnen collatie',
  'Dit zijn alle illustraties binnen de collatie, met uitzondering van degene\c
   die code a krijgen. Als een boek geheel of gedeeltelijk uit gegraveerde\c
   bladen bestaat en de collatie luidt: `engraved folia\', dan vallen deze\c
   illustraties binnen de collatie, en wordt dus kenmerk c toegekend.').
kmc_1200_table('Lettertype', i, romein, '').
kmc_1200_table('Lettertype', j, gotisch, '').
kmc_1200_table('Lettertype', k, cursief, '').
kmc_1200_table('Lettertype', l, 'civilité', '').
kmc_1200_table('Lettertype', m, 'Grieks', '').
kmc_1200_table('Lettertype', n, 'Hebreeuws', '').
kmc_1200_table('Lettertype', o, 'Arabisch', '').
kmc_1200_table('Lettertype', p, 'Armeens', '').
kmc_1200_table('Lettertype', q, muzieknoten,
  'Ook als er maar één muzieknoot in de hele tekst staat.').
kmc_1200_table('Lettertype', r, cyrillisch, '').
kmc_1200_table('Lettertype', s, overige, '').
kmc_1200_table('Titelpagina', a, 'illustratie op de titelpagina',
  'Op te vatten in ruime zin, dus ook een gegraveerd titelblad, een\c
   frontispice, een auteursportret voor of na de titel, een illustratie op\c
   de bedrukte omslag, enz., doch niet vignetten en dergelijke als typografisch\c
   materiaal op te vatten boekversieringen.').
kmc_1200_table('Titelpagina', h, drukkersmerk,
  'Vignetten van bij voorbeeld de Amsterdamse Academie (de Bijenkorf) of van\c
   Nil Volentibus Arduum worden niet beschouwd als drukkersmerk, maar als\c
   illustratie op de titelpagina (a).').
% This occus in the STCN documentation but not in the GGC documentation.
kmc_1200_table('Titelpagina', v, 'bedrukte omslag',
  'Een bedrukte omslag kan voorkomen naast een typografische of een\c
   gegraveerde titelpagina, maar ook in plaats daarvan, zie kmc 4000.\c
   Gegevens afkomstig van een bedrukte omslag zoals de prijs of een\c
   boekenlijst, krijgen ook de betreffende code.').
kmc_1200_table('Titelpagina', w, 'gegraveerde titelpagina',
  'Hieronder wordt verstaan een titelblad dat geheel door middel van een\c
   gravure of houtsnede is vervaardigd. Een gegraveerde titelpagina hoeft\c
   niet per definitie een illustratie te bevatten, maar kan louter uit\c
   gegraveerde tekst bestaan; in het eerste geval krijgt hij zowel code a\c
   als code w, in het tweede geval alleen code w. Een titelpagina die bijna\c
   geheel uit een gravure of houtsnede bestaat, maar waarvan de titel gevormd\c
   is door middel van zetsel, is een typografische titelpagina, met illustratie.\c
   (N.B. Het verschil tussen een gegraveerd titelblad en een frontispice is de\c
   aan- resp. afwezigheid van titel of impressum. Zowel gegraveerde titelpagina\c
   als frontispice worden opgenomen in de collatieformule.)').
kmc_1200_table('Titelpagina', x, 'typografische titelpagina', '').
kmc_1200_table('Titelpagina', y, 'geen titelpagina', '').
kmc_1200_table('Titelpagina', z, meerkleurig,
  'Ook als het gehele werk in een kleur (dus niet zwart) is gedrukt.').



% GRAMMAR %

kmc_1200(G, PPN, Char1):-
  kmc_1200_table(Category, Char1, _Label, _Comment1),
  kmc_1200_table(Category, Relation1, _Comment2),
  rdf_global_id(stcnv:Relation1, Relation2),
  atomic_list_concat(['TypografischKenmerk',Category,Char1], '/', Char2),
  rdf_global_id(stcnv:Char2, Char3),
  rdf_assert(PPN, Relation2, Char3, G).

%! kmc_1200(+Graph:atom, +PPN:iri)//
% A KMC 1200 value consist (ideally) of one character,
% but sometimes it consists of two characters (erroneously).

% PPN 14162843X has a very strange value for this KMC.
kmc_1200(_G, PPN) -->
  atom(aeloude), !,
  dcg_until([end_mode(exclusive)], end_of_line, _),
  {debug(kmc_1200, '[PPN ~w] Unrecognized value for KMC 1200.', [PPN])}.
% Because of typos we need to consider 2-character values as well.
kmc_1200(G, PPN) -->
  [C1,C2],
  {
    atom_codes(Atom, [C1,C2]),
    kmc_1200_translate(Atom, Char), !,
    debug(kmc_1200, '[PPN ~w] Typo in value ~w.', [PPN,Atom]),
    kmc_1200(G, PPN, Char)
  }.
kmc_1200(G, PPN) -->
  [C1],
  {
    char_code(Char1, C1),
    (
      kmc_1200_translate(Char1, Char2)
    ->
      debug(kmc_1200, '[PPN ~w] Typo in value ~w -> ~w.', [PPN,Char1,Char2])
    ;
      Char2 = Char1
    ),
    kmc_1200(G, PPN, Char2), !
  }.
% DEB
kmc_1200(_G, PPN) -->
  {debug(kmc_1200, '[PPN ~w] KMC 1200 cannot be parsed.', [PPN])}.



% CATCH TYPOS %

% `1` is assumed to be a typo of `l`. Example: PPN 321339320.
kmc_1200_translate('1', l).
% `b)` is assumend to be a typo of `b`. Example: PPN 261654624.
kmc_1200_translate('b)', b).
% `C` is assumed to be a typo of `c`. Example: PPN 314559655.
kmc_1200_translate('C',  c).
% `w.` is assumed to be a typo of `w`. Example: 204513316.
kmc_1200_translate('w.', w).
% `X` is assumed to be a typo of `x`. Example: 298324997.
kmc_1200_translate('X', x).
% `Y` is assumed to be a typo of `y`. Example: PPN 305305972.
kmc_1200_translate('Y', y).



% PICARTA GRAMMAR %

%! kmc_1200_picarta(+Graph:atom, +PPN:iri)//
% This applies to KMC 1200 values as they occur in content that
% was scraped from Picarta.

kmc_1200_picarta(G, PPN) -->
  [Code],
  {code_type(Code, alnum)},
  ` (`,
  dcg_until([end_mode(inclusive),output_format(atom)], closing_bracket, _),
  `)`,
  {
    char_code(Char1, Code),
    kmc_1200_table(Category, Char1, _Label, _Comment1),
    kmc_1200_table(Category, Relation1, _Comment2),
    rdf_global_id(picarta:Relation1, Relation2),
    atomic_list_concat(['TypografischKenmerk',Category,Char1], '/', Char2),
    rdf_global_id(stcnv:Char2, Char3),
    rdf_assert(PPN, Relation2, Char3, G)
  }.
kmc_1200_picarta(_G, PPN) -->
  {debug(kmc_1200, '[PPN ~w] Cannot parse Picarta value.', [PPN])}.



% STATISTICS %

statistics_kmc_1200(G, [['Typografosch kenmerk (KMC 1200)','Occurrences']|L]):-
  rdf_property_table(stcnv:typographic_property, G, L).

