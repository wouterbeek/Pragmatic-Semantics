:- module(
  stcn_clean,
  [
    stcn_clean/1 % +Graph:atom
  ]
).

/** <module> STCN clean

Cleaning the STCN database.

This assumes that the STCN_SCRAPE script has been successfully completed
and the STCN graph files are in =|/Data|=.

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09, 2014/03
*/

:- use_module(dcg(dcg_generic)).
:- use_module(kmc(kmc_1200)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(picarta, 'http://picarta.pica.nl/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



stcn_clean(G):-
  % KMC 1200
  forall(
    rdf_string(PPN, picarta:typographic_information, LexicalForm, G),
    (
      dcg_phrase(kmc_1200_picarta(G, PPN), LexicalForm),
      rdf_retractall_string(PPN, picarta:typographic_information, LexicalForm, G)
    )
  ).

