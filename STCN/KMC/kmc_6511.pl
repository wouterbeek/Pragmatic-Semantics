:- module(
  kmc_6511,
  [
    assert_schema_kmc_6511/1, % +Graph:graph
    kmc_6511//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc6511/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 6511 - TOPICS

# Numbers taken from _|redactiebladen|_ file

These numbers were calculated using the following regular expression schema:
_|\n6511!|_.

| *KMC*   | *Occurrences* |
| 6511    | 136.272       |
| 6512    |  97.020       |
| 6513    |  50.100       |
| 6514    |  10.373       |
| 6515    |   2.119       |
| 6516    |     777       |
| 6517    |     543       |
| 6518    |     154       |
| 6519    |       1       |
| All     | 297.359       |
| Parsed  | 297.293       |

Perform an RE search that matches on "\nKMC !PPN", i.e. without the trailing
exclamation mark.

# Parsing problems

PPN 324985231 has no exclamation mark after the topic PPN in KMC 6512.

@author Wouter Beek
@version 2013/03, 2013/06, 2013/09, 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



assert_schema_kmc_6511(G):-
  rdfs_assert_class(stcnv:'Topic', G),
  rdf_assert_property(stcnv:topic, G),
  rdfs_assert_label(stcnv:topic, onderwerp, nl, G),
  rdf_assert_string(stcnv:topic, stcnv:kb_name, 'KMC 6511', G),
  rdfs_assert_seeAlso(stcnv:topic,
      'http://www.kb.nl/kbhtml/stcnhandleiding/6511.html', G),
  rdf_assert_language_tagged_string(stcnv:topic, stcnv:picarta_name,
      'Onderwerpstrefwoord', nl, G).

kmc_6511(G, PPN) -->
  exclamation_mark,
  ppn('Topic', TopicPPN),
  exclamation_mark,
  {
    ppn_to_topic(G, TopicPPN, Topic),
    rdf_assert(PPN, stcnv:topic, Topic, G)
  }.

ppn_to_topic(_G, TopicPPN, Topic):-
  rdf_global_id(stcnv:TopicPPN, Topic),
  rdfs_individual_of(Topic, stcnv:'Topic'), !.
ppn_to_topic(G, TopicPPN, Topic):-
  rdf_global_id(stcnv:TopicPPN, Topic),
  rdf_assert_individual(Topic, stcnv:'Topic', G).

statistics_kmc6511(_G, []).

