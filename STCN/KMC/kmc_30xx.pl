:- module(
  kmc_30xx,
  [
    assert_schema_kmc_30xx/1, % +Graph:graph
    kmc_30xx//3, % +Graph:atom
                 % +PPN:uri
                 % +Predicate
    populate_dbpedia/1, % +DBpedia_Graph:atom
    statistics_kmc30xx/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 30XX

Code shared by KMC 3000 and KMC 3011.

This includes PPN-based author lookup on Picarta.

Some author PPNs have lower case x in them, e.g. PPN 06907352x.
Some author PPNs have upper case x in them, e.g. PPN 14895961X.
These are considered to be the same. Mapped to upper case X using option
=|case(upper)|=.

@author Wouter Beek
@version 2013/01-2013/03, 2013/05-2013/06, 2013/09, 2013/12-2014/01, 2014/03
*/

:- use_module(dbpedia(dbpedia_agent)).
:- use_module(dcg(dcg_ascii)). % Meta-DCG.
:- use_module(dcg(dcg_generic)).
:- use_module(generics(thread_ext)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(owl(owl_build)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_stat)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(rdfs(rdfs_read)).
:- use_module(stcn(stcn_generic)).
:- use_module(sparql(sparql_cache)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').

:- rdf_meta(link_to_dbpedia_agent(+,r)).



assert_schema_kmc_30xx(G):-
  % Author.
  rdfs_assert_subclass(stcnv:'Author', foaf:'Agent', G),
  rdfs_assert_label(stcnv:'Author', author, en, G),
  rdfs_assert_label(stcnv:'Author', auteur, nl, G),

  % Has author.
  rdf_assert_property(stcnv:author, G),
  rdfs_assert_label(stcnv:author, 'has author', en, G),
  rdfs_assert_label(stcnv:author, 'heeft auteur', nl, G),

  % Author name.
  rdf_assert_property(stcn:author_name, G),
  rdfs_assert_label(stcnv:author_name, 'has author name', en, G),
  rdfs_assert_label(stcnv:author_name, 'heeft auteursnaam', nl, G).

link_to_dbpedia_agent(G, Agent):-
  rdf_string(Agent, foaf:name, Name, G),

  rdf_datatype(Agent, stcnv:birth, Birth, xsd:gYear, G),
  rdfs_assert_label(stcnv:birth, geboortejaar, nl, G),

  rdf_datatype(Agent, stcnv:death, Death, xsd:gYear, G),
  rdfs_assert_label(stcnv:death, sterftejaar, nl, G),

  dbpedia_find_agent(Name, Birth, Death, DBpediaAgent),
  owl_assert_resource_identity(Agent, DBpediaAgent, G),
  debug(
    dbpedia,
    'Agent ~w linked to DBpedia agent ~w.',
    [Agent,DBpediaAgent]
  ).

link_to_dbpedia_agents(G):-
  aggregate_all(
    set(Agent),
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      rdf_subject(Agent, G)
    ),
    Agents
  ),
  run_on_sublists(Agents, link_to_dbpedia_agents(G), 10).

link_to_dbpedia_agents(G, Agents):-
  maplist(link_to_dbpedia_agent(G), Agents).

kmc_30xx(G, PPN, Pred) -->
  dcg_until([end_mode(exclusive)], exclamation_mark, _Codes),
  ppn('Author', AuthorPPN),
  "!",
  {
    rdf_global_id(stcn:AuthorPPN, Author),
    rdf_assert(PPN, Pred, Author, G)
  }.

populate_dbpedia(DBpediaG):-
  forall(
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      owl_resource_identity(Agent, DBpedia_Agent),
      rdf_global_id(dbpedia:_, DBpedia_Agent)
    ),
    (
      sparql_cache(DBpedia_Agent, _, Propositions),
      forall(
        member([S,P,O], Propositions),
        (
          rdf_assert(S, P, O, DBpediaG),
          flag(populate_dbpedia_triples, Id1, Id1 + 1)
        )
      ),
      flag(populate_dbpedia_agents, Id2, Id2 + 1)
    )
  ).

statistics_kmc30xx(G, [[A1,V1],[A2,V2],[A3,V3],[A4,V4],[A5,V5],[A6,V6]]):-
  A1 = 'Publications with at least one author',
  count_subjects(stcnv:author, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1,V1]),

  A2 = 'Publications with at least one DBpedia author',
  aggregate_all(
    set(DBpediaAuthorPPN),
    (
      rdfs(DBpediaAuthorPPN, stcnv:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, _DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthorPPNs
  ),
  length(DBpediaAuthorPPNs, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2,V2]),

  A3 = 'Number of authors (including pseudonyms)',
  count_individuals(stcnv:'Author', G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3,V3]),

  A4 = 'Number of DBpedia authors (including pseudonyms)',
  aggregate_all(
    set(DBpediaAuthor),
    (
      rdfs(_PPN, stcnv:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthors
  ),
  length(DBpediaAuthors, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4,V4]),

  A5 = 'Publications written under at least one pseudonym',
  aggregate_all(
    set(PPN_Pseudonym),
    (
      rdfs(PPN_Pseudonym, stcnv:author, Author, G),
      rdf_string(Author, stcnv:pseudonym, Pseudonym, G)
    ),
    PPN_Pseudonyms
  ),
  length(PPN_Pseudonyms, V5),
  debug(stcn_statistics, '-- ~w: ~w', [A5,V5]),

  A6 = 'Number of pseudonyms',
  aggregate_all(
    set(Pseudonym),
    rdf_string(_, stcnv:pseudonym, Pseudonym, G),
    NumberOfPseudonyms
  ),
  length(NumberOfPseudonyms, V6),
  debug(stcn_statistics, '-- ~w: ~w', [A6,V6]).

