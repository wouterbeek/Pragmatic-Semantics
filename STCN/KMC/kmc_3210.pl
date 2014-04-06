:- module(
  kmc_3210,
  [
    assert_schema_kmc_3210/1, % +Graph:graph
    kmc_3210//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc3210/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 3210 - SORTING TITLE

Facultative field. Cannot be repeated.

KMC 3210 contains the sorting title of bibles and anonymous popular proze.

# § 65

## Bibles

Bibles and anonymous popular proze receive a sorting title in KMC 3210.
The following table gives the sorting titles for popular bible books.

!!kmc_3210!!bible_book!!2!!

The basic structure for bibles is as follows:

~~~{.txt}
'@Bible' + LANGUAGE + ((';' + PART)! + ' ' + BOOK (+ ' ' + SELECTION)!)!
~~~

Examples:

~~~{.txt}
@Bible Polyglot ; O.T. Job
@Bible Dutch ; O.T. Song of Solomon
@Bible Dutch
@Bible Dutch ; O.T. Apocrypha Tobit. Selection
@Bible French ; Metrical Psalms
~~~

Older descriptions sometimes have a sorting code appearing before the =@=.
These codes can be neglected.

Metrical psalms are described distinctly and do not appear in between the
other psalm prints. After the =/= the poet who put the text on rhyme is
mentioned, after =|Rhymed version by|= instead of =Adaptation=,
=|Translated from the French|=, etc. The name is taken up in thesaurus style
in KMC 3011.

Examples:

~~~{.txt}
kmc 4000: De @CL psalmen Davids. / Rhymed version by P. Dathenus
kmc 3011: Petrus@Dathenus!068066961!
~~~

If the poet who put the text on rhyme is unknown, then use =|Rhymed version|=.

Used for prints of the _Talmud_:
=|Talmud Bavli|= or =|Talmud Yerusalmi|=  for the Babylonian and the
Jerusalem Talmud, respectively.

## Anonymous popular works

Anonymous popular works also receive a sorting title in KIMC 3210 (see table).

The following table enumerates the anonymous popular works.

!!kmc_3210!!anonymous_popular!!2!!

The same holds for several other works, i.e., =Almanak=, =Geuzenliedboek= and
=Koran=.

=Almanak= includes all kinds of [almanakken], including [comptoiralmanachs],
provided that they appear yearly. The editors are included in KMC 3011.
In KMC 4000 the editors are mentioned in the order in which they appear on the
title page, preceded by =By= or =|Compiled by|=.

Publications of the Heidelberg catechismus (first question: "Welke is uw
enige troost beide in't leven ende sterven?") and the Augsburg confession
(first article: "Eerstelijk wordt eendrachtig geleerd ende gehouden naar het
besluit des Concilii Nicaeni") receive as sorting title in KMC 3210
=|@Heidelberg Catechism (+ LANGUAGE)|= and
=|@Augsburg Confession (+ LANGUAGE)|= respectively.

Note that additions such as =profeet=, for instance in case of prophet David,
are not included in a 4-2-2-1 search. [?]

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_string)).
:- use_module(standards(lexvo)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



anonymous_popular('9 quaetsten').
anonymous_popular('Alexander (koning)').
anonymous_popular('Alexander van Mets'). % See 'Vrouwenpeerle'
anonymous_popular('Almanak').
anonymous_popular('Amadis').
anonymous_popular('Appollonius van Thyro').
anonymous_popular('Aurelius ende Isabella').
anonymous_popular('Baghijnken van Parijs').
anonymous_popular('Borchgravinne van Vergi').
anonymous_popular('Buevijn van Austoen').
anonymous_popular('Carolus V').
anonymous_popular('Christoffel Wagenaer').
anonymous_popular('Clamydes ende Claermonde').
anonymous_popular('David (profeet)').
anonymous_popular('Destructien van Jerusalem').
anonymous_popular('Destructien van Troyen').
anonymous_popular('Dialogus creaturarum').
anonymous_popular('Elckerlyck').
anonymous_popular('Esopus').
anonymous_popular('Euryalus ende Lucresia').
anonymous_popular('Evangelien van den Spinrocke').
anonymous_popular('Faustus').
anonymous_popular('Fierenbras').
anonymous_popular('Florent ende Lion').
anonymous_popular('Florentina de Getrouwe'). % See 'Vrouwenpeerle'
anonymous_popular('Floris ende Blancefleur').
anonymous_popular('Fortunatus').
anonymous_popular('Frederick van Jenuen').
anonymous_popular('Galien Rhetore').
anonymous_popular('Galmy (ridder)').
anonymous_popular('Genoveva').
anonymous_popular('Gesten of gheschienissen van Romen').
anonymous_popular('Geuzenliedboek').
anonymous_popular('Gilias').
anonymous_popular('Godevaert van Boloen').
anonymous_popular('Griseldis'). % See 'Vrouwenpeerle'
anonymous_popular('Helena van Constantinopel'). % See 'Vrouwenpeerle'
anonymous_popular('Helias, ridder metter Swane').
anonymous_popular('Hertog Adolf in Kourland').
anonymous_popular('Hertog van Brunswijk').
anonymous_popular('Hughe van Bordeus').
anonymous_popular('Jacke').
anonymous_popular('Jan van Beverley').
anonymous_popular('Jan van Parijs').
anonymous_popular('Jeesten. Zie: Gesten').
anonymous_popular('Jonathas ende Rosafiere').
anonymous_popular('Joncker Jan wt den vergiere').
anonymous_popular('Joseph').
anonymous_popular('Julius Caesar').
anonymous_popular('Koran').
anonymous_popular('Lansloet ende Sandrijn').
anonymous_popular('Leonelle en Canamorus').
anonymous_popular('Leven ons Heren').
anonymous_popular('Malegijs').
anonymous_popular('Margarieta van Lymborch').
anonymous_popular('Mariken van Nieumegen').
anonymous_popular('Meliadus').
anonymous_popular('Meluzine').
anonymous_popular('Merlijn').
anonymous_popular('Nieuwe Hollandsche schouwburg, zynde een verzameling van verscheyden pluggen en serieuse danssen').
anonymous_popular('Olyvier van Castillen').
anonymous_popular('Palmerijn van Olyven').
anonymous_popular('Parijs ende Vienna').
anonymous_popular('Partinoples').
anonymous_popular('Pastoor te Kalenberghe').
anonymous_popular('Peeter van Provencen').
anonymous_popular('Ponthus ende Sydonie').
anonymous_popular('Primalion van Grieken').
anonymous_popular('Pyramus en Thisbe').
anonymous_popular('Reynaert die Vos').
anonymous_popular('Ridder metter Swane'). % See 'Helias'
anonymous_popular('Rijckaert zonder Vreese').
anonymous_popular('Robrecht den Duyvel').
anonymous_popular('Russche (broeder)').
anonymous_popular('Sack der consten').
anonymous_popular('Salomon ende Marcolphus').
anonymous_popular('Sandrijn ende Lansloet.'). % See 'Lansloet ende Sandrijn'
anonymous_popular('Scaecspel').
anonymous_popular('Schip vol wonders').
anonymous_popular('Seghelijn van Jerusalem').
anonymous_popular('Seven wijse mannen van Romen').
anonymous_popular('Sibilla').
anonymous_popular('Strijt van Roncevale').
anonymous_popular('Sydrac').
anonymous_popular('Tondalus visioen').
anonymous_popular('Troylus ende Brysede'). % See 'Destructien van Troyen'
anonymous_popular('Turias ende Floreta').
anonymous_popular('Uilenspiegel').
anonymous_popular('Urbaen').
anonymous_popular('Valentijn en Ourson').
anonymous_popular('Van den X esels').
anonymous_popular('Verloren sone').
anonymous_popular('Vier Heemskinderen').
anonymous_popular('Virgilius').
anonymous_popular('Vrouwenpeerle').
anonymous_popular('Wandelende jood').

assert_schema_kmc_3210(_Graph).

bible_book(ot, 'Pentateuch').
bible_book(ot, 'Prophets').
bible_book(ot, 'Prophets, Greater').
bible_book(ot, 'Prophets, Minor').
bible_book(ot, 'Amos').
bible_book(ot, 'Chronicles').
bible_book(ot, 'Chronicles I').
bible_book(ot, 'Chronicles II').
bible_book(ot, 'Daniel').
bible_book(ot, 'Deuteronomy').
bible_book(ot, 'Ecclesiastes').
bible_book(ot, 'Esther').
bible_book(ot, 'Exodus').
bible_book(ot, 'Ezekiel').
bible_book(ot, 'Ezra').
bible_book(ot, 'Genesis').
bible_book(ot, 'Habakkuk').
bible_book(ot, 'Haggai').
bible_book(ot, 'Hosea').
bible_book(ot, 'Isaias').
bible_book(ot, 'Jeremiah').
bible_book(ot, 'Job').
bible_book(ot, 'Joel').
bible_book(ot, 'Jonah').
bible_book(ot, 'Joshua').
bible_book(ot, 'Judges').
bible_book(ot, 'Kings').
bible_book(ot, 'Kings I').
bible_book(ot, 'Kings II').
bible_book(ot, 'Lamentations').
bible_book(ot, 'Leviticus').
bible_book(ot, 'Malachi').
bible_book(ot, 'Micah').
bible_book(ot, 'Nahum').
bible_book(ot, 'Nehemiah').
bible_book(ot, 'Numbers').
bible_book(ot, 'Obadiah').
bible_book(ot, 'Proverbs').
bible_book(ot, 'Psalms').
bible_book(ot, 'Ruth').
bible_book(ot, 'Samuel').
bible_book(ot, 'Samuel I').
bible_book(ot, 'Samuel II').
bible_book(ot, 'Song of Solomon').
bible_book(ot, 'Zechariah').
bible_book(ot, 'Zephaniah').
bible_book(ot, 'Apocrypha').
bible_book(ot, 'Apocrypha. Additions to Daniel').
bible_book(ot, 'Apocrypha. Baruch').
bible_book(ot, 'Apocrypha. Ecclesiasticus').
bible_book(ot, 'Apocrypha. Esdras').
bible_book(ot, 'Apocrypha. Esdras III').
bible_book(ot, 'Apocrypha. Esdras IV').
bible_book(ot, 'Apocrypha. Judith').
bible_book(ot, 'Apocrypha. Maccabees').
bible_book(ot, 'Apocrypha. Maccabees I').
bible_book(ot, 'Apocrypha. Maccabees II').
bible_book(ot, 'Apocrypha. Maccabees III').
bible_book(ot, 'Apocrypha. Maccabees IV').
bible_book(ot, 'Apocrypha. Prayer of Manasses').
bible_book(ot, 'Apocrypha. Tobit').
bible_book(ot, 'Apocrypha. Wisdom of Solomon').
bible_book(nt, 'Epistles').
bible_book(nt, 'Epistles, Catholic').
bible_book(nt, 'Epistles, Pauline').
bible_book(nt, 'Gospels').
bible_book(nt, 'Acts').
bible_book(nt, 'Colossians').
bible_book(nt, 'Corinthians').
bible_book(nt, 'Corinthians I').
bible_book(nt, 'Corinthians II').
bible_book(nt, 'Ephesians').
bible_book(nt, 'Galatians').
bible_book(nt, 'Hebrews').
bible_book(nt, 'James').
bible_book(nt, 'John').
bible_book(nt, 'John, Epistles').
bible_book(nt, 'Jude').
bible_book(nt, 'Luke').
bible_book(nt, 'Mark').
bible_book(nt, 'Matthew').
bible_book(nt, 'Peter').
bible_book(nt, 'Philemon').
bible_book(nt, 'Philippians').
bible_book(nt, 'Revelation').
bible_book(nt, 'Romans').
bible_book(nt, 'Thessalonians').
bible_book(nt, 'Thessalonians I').
bible_book(nt, 'Thessalonia').
bible_book(nt, 'Timothy').
bible_book(nt, 'Timothy I').
bible_book(nt, 'Timothy II').
bible_book(nt, 'Titus').
bible_book(_,  'Metrical Psalms').

kmc_3210(G, PPN) -->
  ampersand,
  {anonymous_popular(Title)},
  atom(Title),
  {
    rdf_assert_string(PPN, stcnv:title, Title, G),
    debug(
      kmc_3210,
      'Recognized title \'~w\' for publication ~w.',
      [Title, PPN]
    )
  }.
kmc_3210(G, PPN) -->
  ampersand,
  atom('Bible'),
  space,
  language(G, PPN).
kmc_3210(_G, PPN) -->
  {debug(kmc_3210, 'Cannot parse title for publication ~w.', [PPN])}.

language(G, PPN) -->
  word(Word),
  {
    % Just checking!
    (
      find_language([language(en)], Word, Language),
      rdf(PPN, stcn:language, Language, G)
    ->
      debug(
        kmc_3210,
        'Checked language ~w (~w) for publication ~w.',
        [Word,Language,PPN]
      )
    ;
      debug(kmc_3210, 'Failed language ~w for publication ~w.', [Word,PPN])
    )
  }.

statistics_kmc3210(G, [[A1,V1]]):-
  A1 = 'Publications with title',
  count_subjects(stcnv:title, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1,V1]).

