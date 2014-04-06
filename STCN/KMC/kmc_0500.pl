:- module(
  kmc_0500,
  [
% SCHEMA ASSERTION
    assert_schema_kmc_0500/1, % +Graph:atom
% GRAMMAR
    kmc_0500//2, % +Graph:atom
                 % +PPN:uri
% STATISTICS
    statistics_kmc_0500/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 0500

Encodes the 'status' of a work.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').

:- meta_predicate(kmc_0500_oclc(+,+,+,3)).
:- meta_predicate(kmc_0500_status_value(3,+,-,-)).



% SCHEMA ASSERTION %

assert_schema_kmc_0500(G):-
  % Status values
  rdfs_assert_class(stcnv:'StatusValue', G),
  rdfs_assert_label(stcnv:'StatusValue', 'status value', G),
  rdfs_assert_comment(stcnv:'StatusValue',
    'The GGC assigns meaning to each status code individually.', G),
  rdfs_assert_seeAlso(stcnv:'StatusValue',
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500', G),

  % STCN status values
  rdfs_assert_subclass(stcnv:'StatusValue/STCN', stcnv:'StatusValue', G),
  rdfs_assert_label(stcnv:'StatusValue/STCN', 'STCN status value', G),
  rdfs_assert_comment(stcnv:'StatusValue/STCN',
    'The STCN assigns extra meaning to the three status codes combined.', G),
  rdfs_assert_seeAlso(stcnv:'StatusValue/STCN',
    'http://www.kb.nl/kbhtml/stcnhandleiding/0500.html', G),

  % Monograph
  rdf_assert_individual(stcnv:'Aav', stcnv:'StatusValue/STCN', G),
  rdfs_assert_label(stcnv:'Aav', en, monograph, G),
  rdfs_assert_label(stcnv:'Aav', nl, monografie, G),

  % Multipart work
  rdf_assert_individual(stcnv:'Abv', stcnv:'StatusValue/STCN', G),
  rdfs_assert_label(stcnv:'Abv', en, 'multipart work', G),
  rdfs_assert_label(stcnv:'Abv', nl, 'meerdelig werk', G),

  % Magazine
  rdf_assert_individual(stcnv:'Acv', stcnv:'StatusValue/STCN', G),
  rdfs_assert_label(stcnv:'Acv', en, magazine, G),
  rdfs_assert_label(stcnv:'Acv', nl, tijdschrift, G),

  % First position status values.
  rdfs_assert_subclass(stcnv:'StatusValue/Pos1', stcnv:'StatusValue', G),
  forall(
    'kmc_0500-1'(Char, Label, Comment),
    (
      kmc_0500_status_value('kmc_0500-1', Char, Value, _Pred1),
      rdf_assert_individual(Value, stcnv:'StatusValue/Pos1', G),
      rdfs_assert_label(Value, nl, Label, G),
      rdfs_assert_comment(Value, nl, Comment, G)
    )
  ),

  % Second position status values.
  rdfs_assert_subclass(stcnv:'StatusValue/Pos2', stcnv:'StatusValue', G),
  forall(
    'kmc_0500-2'(Char, Label, Comment),
    (
      kmc_0500_status_value('kmc_0500-2', Char, Value, _Pred2),
      rdf_assert_individual(Value, stcnv:'StatusValue/Pos2', G),
      rdfs_assert_label(Value, nl, Label, G),
      rdfs_assert_comment(Value, nl, Comment, G)
    )
  ),

  % Third position status values.
  rdfs_assert_subclass(stcnv:'StatusValue/Pos3', stcnv:'StatusValue', G),
  rdfs_assert_label(stcnv:'StatusValue/Pos3', 'PAR 103 status value', G),
  rdfs_assert_comment(stcnv:'StatusValue/Pos3',
    'Status values as defined by PAR 103.', en, G),
  forall(
    par_103(Char, Label, Comment),
    (
      kmc_0500_status_value(par_103, Char, Value, _Pred3),
      rdf_assert_individual(Value, stcnv:'StatusValue/Pos3', G),
      rdfs_assert_label(Value, Label, nl, G),
      rdfs_assert_comment(Value, Comment, nl, G)
    )
  ),

  % The status relationship.
  rdf_assert_property(stcnv:status, G),
  rdfs_assert_label(stcnv:status, status, en, G),
  rdfs_assert_label(stcnv:status, status, nl, G),
  rdf_assert_string(stcnv:status, stcnv:kb_name, 'KMC 0500', G),
  rdfs_assert_seeAlso(stcnv:status,
    'http://www.kb.nl/kbhtml/stcnhandleiding/0500.html', G),
  rdfs_assert_domain(stcnv:status, stcnv:'Publication', G),
  rdfs_assert_range(stcnv:status, stcnv:'StatusValue', G),

  % Status 1 property.
  rdfs_assert_subproperty(stcnv:status_pos1, stcnv:status, G),
  rdfs_assert_label(stcnv:status_pos1, 'status positie 1', nl, G),
  rdfs_assert_comment(stcnv:status_pos1, 'Algemene inhoudsaanduiding.', nl,
      G),
  rdfs_assert_seeAlso(stcnv:status_pos1,
      'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500#positie_1_kmc_0500',
      G),

  % Status 2 property.
  rdfs_assert_subproperty(stcnv:status_pos2, stcnv:status, G),
  rdfs_assert_label(stcnv:status_pos2, 'status positie 2', nl, G),
  rdfs_assert_comment(stcnv:status_pos2,
    'Het correct invullen van positie 2 van kmc 0500 is van groot belang.\c
     Wijzig positie 2 alleen als dat noodzakelijk is en breng bij een\c
     belangrijke wijziging (bijv. \'a\' wordt veranderd in \'b\') de\c
     medegebruikers van de titel via Picamail op de hoogte (zie PAR 052).',
     nl, G),
  rdfs_assert_seeAlso(stcnv:status_pos2,
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500#positie_2_kmc_0500', G),

  % Status 3 property.
  rdfs_assert_subproperty(stcnv:status_pos3, stcnv:status, G),
  rdfs_assert_label(stcnv:status_pos3, 'status positie 3', nl, G),
  rdf_assert_string(stcnv:status_pos3, stcnv:kb_name, 'PAR 103', G),
  rdfs_assert_seeAlso(stcnv:status_pos3,
    'http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103', G).



% GRAMMAR %

kmc_0500(G, PPN) -->
  [C1],
  {kmc_0500_oclc(G, PPN, C1, 'kmc_0500-1')},

  [C2],
  {kmc_0500_oclc(G, PPN, C2, 'kmc_0500-2')},

  [C3],
  {kmc_0500_oclc(G, PPN, C3, par_103)},

  % The STCN assigns extra meaning to the three codes combined.
  !, {
    atom_codes(Atom, [C1,C2,C3]),
    kmc_0500_stcn(G, PPN, Atom)
  }.
kmc_0500(_G, PPN) -->
  {debug(kmc_0500, '[PPN ~w] Unrecognized KMC 0500 status value.', [PPN])}.

kmc_0500_stcn(G, PPN, 'Aav'):- !,
  rdf_assert(PPN, stcnv:status, stcnv:'StatusValue/STCN/Aav', G).
kmc_0500_stcn(G, PPN, 'Abv'):- !,
  rdf_assert(PPN, stcnv:status, stcnv:'StatusValue/STCN/Abv', G).
kmc_0500_stcn(G, PPN, 'Acv'):- !,
  rdf_assert(PPN, stcnv:status, stcnv:'StatusValue/STCN/Acv', G).
kmc_0500_stcn(_G, PPN, Value):-
  debug(kmc_0500, '[PPN ~w] Unrecognized compound type: ~w.', [PPN,Value]).



% SUPPORT PREDICATES %

kmc_0500_oclc(G, PPN, Code, Base):-
  char_code(Char, Code),
  % Make sure this is a legal value.
  once(call(Base, Char, _Label, _Comment)), !,
  kmc_0500_status_value(Base, Char, Value, Pred),
  rdf_assert(PPN, Pred, Value, G).
kmc_0500_oclc(_G, PPN, Code, Base):-
  char_code(Char, Code),
  debug(kmc_0500, 'Illegal status value ~w:~w for PPN ~w.', [Base,Char,PPN]).

kmc_0500_status_value(_Mod:Base1, Char, Value, Pred):-
  (  Base1 == 'kmc_0500-1'
  -> Base2 = 'StatusValue/Pos1',
     rdf_global_id(stcnv:status_pos1, Pred)
  ;  Base1 == 'kmc_0500-2'
  -> Base2 = 'StatusValue/Pos2',
     rdf_global_id(stcnv:status_pos2, Pred)
  ;  Base1 == par_103
  -> Base2 = 'StatusValue/Pos3',
     rdf_global_id(stcnv:status_pos3, Pred)),
  atomic_list_concat([Base2,Char], '/', Name),
  rdf_global_id(stcnv:Name, Value).



% DOMAIN KNOWLEDGE %

'kmc_0500-1'('A', 'gedrukte tekst',
  '- Gedrukte tekst.\c
   - Gedrukt niet-bewegend beeld in boekvorm uitgegeven.\c
   - Gedrukt cartografisch materiaal in boekvorm uitgegeven.\c
   - Gedrukte brieven in boekvorm uitgegeven.\c
   - Bladmuziek valt niet onder deze categorie.').
'kmc_0500-1'('B', 'bewegend beeld', 'Bewegend beeld (o.a. streaming video).').
'kmc_0500-1'('D', 'losse brieven',
  'Losse brieven al dan niet in een band of een map.\c
   Dit is een specifieke categorie handschriften.\c
   Het gaat hierbij dus niet om in boekvorm verschenen brieven.').
'kmc_0500-1'('F', handschriften,
  'Handschriften inclusief typoscript (m.u.v. losse brieven).').
'kmc_0500-1'('G', geluid, 'Geluid').
'kmc_0500-1'('I', 'niet-bewegend beeld',
  'Niet-bewegend beeld in losse items al dan niet in een band of\c
   een map (bijvoorbeeld losse foto\'s, losse prenten en dia\'s).\c
   Het gaat hierbij dus niet om in boekvorm verschenen niet-bewegend\c
   beeld.').
'kmc_0500-1'('K', 'cartografisch materiaal',
  'Cartografisch materiaal in losse items al dan niet in een band\c
   of een map (bijvoorbeeld losse plattegronden van steden)\c
   (m.u.v. handschriften).').
'kmc_0500-1'('M', bladmuziek, 'Bladmuziek (m.u.v. handschriften).').
'kmc_0500-1'('O', 'online resources',
  'Online resources (hoofdzakelijk tekst), zoals online e-books en online\c
   e-journals.').
'kmc_0500-1'('R', archiefmateriaal, 'Archiefmateriaal').
'kmc_0500-1'('S', software,
  'Software, d.w.z.\c
   - Algemene computerprogramma\'s\c
   - Computergames
   - Multimedia op computerdrager
   - E-books en e-journals op computerdrager\c
     (tekst; geen online resource)).').
'kmc_0500-1'('T', thesaurus, 'Thesaurus').
'kmc_0500-1'('V', objecten,
  'Objecten. De overige materiaalsoorten gelden als \'primair\'.\c
   Alleen \'objecten\' die niet vallen onder een van de meer specifieke\c
   categorieën (A, B, F e.d.), krijgen een \'V\' op de 1e positie van\c
   kmc 0500 met de betekenis van \'voorwerp/object\'. ').
'kmc_0500-1'('X', 'NBD Biblion',
  'Uitsluitend gebruikt door NBD Biblion om records in te voeren t.b.v.\c
   de aanschafinformaties (a.i\'s) voor openbare bibliotheken.').

'kmc_0500-2'('a', 'eendelig document',
  'Eendelig document al dan niet in een reeks.').
'kmc_0500-2'('b', periodiek,
  '- Periodiek (tijdschrift, jaarboek, krant e.d.)\c
   - Koepeltitel van een reeks.').
'kmc_0500-2'('c', koepeltitel, 'Koepeltitel meerdelige publicatie (mp).').
'kmc_0500-2'('e', 'onzelfstandige tussentitel',
  'Onzelfstandige tussentitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('E', 'zelfstandige tussentitel',
  'Zelfstandige tussentitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('f', 'onzelfstandige deeltitel',
  'Onzelfstandige deeltitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('F', 'zelfstandige deeltitel',
  'Zelfstandige deeltitel binnen meerdelige publicatie (mp).').
'kmc_0500-2'('n', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('N', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('o', 'Online Contents',
  'Abstract gerelateerd aan Online Contents artikel.').
'kmc_0500-2'('r', convoluut,
  'Convoluut (aantal handschriften in één band) of compilatie.').
'kmc_0500-2'('s', onderdeelbeschrijving,
  'Onderdeelbeschrijving, artikelbeschrijving.').

par_103(a, acquisitie, acquisitie).
par_103('B', 'tape-invoer', 'Titels, afkomstig van tape-invoer,\c
  waarvan na matching is gebleken dat ze wellicht al in het GGC aanwezig\c
  waren. Deze titels kunnen worden omgewerkt naar "normale" titels, maar\c
  zullen veelal verwijderd worden.').
par_103(c, 'C.I.P.', 'C.I.P.').
par_103(p,'bibliografische onvolledigheid', 'Bibliografisch onvolledig record\c
  van een openbare bibliotheek in Nederland in het kader van het NBC project').
par_103(r, retrospectief, retrospectief).
par_103(v, 'bibliografisch volledig', 'Bibliografisch volledig, niet muteerbaar.').
par_103(x, 'tape titel', 'Tape titel/bibliografisch. Volledig, muteerbaar.').
par_103(y, 'bibliografisch onvolledig', 'Bibliografisch onvolledig.').




% STATISTICS %

statistics_kmc_0500(G, [['Status (KMC 0500)','Occurrences']|L]):-
  rdf_property_table(stcnv:status, G, L).

