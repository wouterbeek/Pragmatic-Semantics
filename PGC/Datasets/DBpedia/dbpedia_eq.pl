:- module(
  dbpedia_eq,
  [
    dbpedia_eq/2, % +Resource:iri
                  % -EntityOrQuantity:iri
    dbpedia_eq/3 % +Resource:iri
                 % -EntityConfidence:between(0.0,1.0)
                 % -QuantityConfidence:between(0.0,1.0)
  ]
).

/** <module> DBpedia Entity/Quantity

In Qualitative Reasoning it is important to distinguish between
 things whose fundamental property changes over time, called *quantities*,
 and things whose fundamental properties do not change over time,
 called *entities*.
It would therefore be neat to have a predicate that tells us
 whether something is an entity or a quantity (assuming it is either).

@author Wouter Beek
@version 2014/02
*/

:- use_module(dbpedia(dbpedia)).
:- use_module(dbpedia(dbpedia_categories)). % Ensures DBpedia categories are loaded.
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(lod(cache_it)).
:- use_module(lod(lod_query)).
:- use_module(math(math_ext)).

:- rdf_meta(dbpedia_eq(r,-)).
:- rdf_meta(dbpedia_eq(r,-,-)).



dbpedia_eq(Resource, EntityOrQuantity):-
  dbpedia_eq(Resource, EConf, QConf),
  (
    EConf > QConf
  ->
    rdf_global_id(qsim:'Entity', EntityOrQuantity)
  ;
    rdf_global_id(qsim:'Quantity', EntityOrQuantity)
  ).


dbpedia_eq(Resource, EConf, QConf):-
  dbpedia_load_categories,
  cache_it1(_, lod_cache, _, Resource),
  findall(
    EConf-QConf,
    (
      rdf(Resource, dcterms:subject, Subject),
      rdf_reachable(Subject, skos:broader, dbpedia:'Category:Objects', 25, EDist),
      rdf_reachable(Subject, skos:broader, dbpedia:'Category:Quantity', 25, QDist),
      QConf is EDist / (EDist + QDist),
      EConf is QDist / (EDist + QDist)
    ),
    Pairs
  ),
  pairs_keys_values(Pairs, EConfs, QConfs),
  average(EConfs, EConf),
  average(QConfs, QConf).

