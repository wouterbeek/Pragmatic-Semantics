:- module(
  dbpedia_agent,
  [
    dbpedia_find_agent/4 % +Name:atom
                         % +Birth:integer
                         % +Death:integer
                         % -DBpediaAgent:iri
  ]
).

/** <module> DBpedia agent

Search for agents (e.g. people) on DBpedia.

@author Wouter Beek
@version 2013/03-2013/05, 2013/08, 2013/12-2014/01
*/

:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf_term(rdf_term)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_ext)).

:- rdf_meta(dbpedia_find_agent(+,+,+,r)).



%! dbpedia_find_agent(
%!   +FullName:atom,
%!   +Birth:integer,
%!   +Death:integer,
%!   -DBpediaAuthor:uri
%! ) is semidet.

dbpedia_find_agent(Name, Birth, Death, DBpediaAuthor):-
  phrase(
    sparql_formulate(
      _,
      _,
      [dbp,foaf],
      select,
      true,
      [writer],
      [
        rdf(var(writer), rdf:type, foaf:'Person'),
        rdf(var(writer), rdfs:label, var(label)),
        filter(regex(var(label), string(Name), [case_insensitive])),
        rdf(var(writer), dbpprop:dateOfBirth, var(birth)),
        filter(regex(var(birth), string(Birth))),
        rdf(var(writer), dbpprop:dateOfDeath, var(death)),
        filter(regex(var(death), string(Death)))
      ],
      10,
      _,
      _
    ),
    Query
  ),
  sparql_query(dbpedia, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(dbpedia, 'Could not find a resource for \'~w\'.', [Name])
  ;
    first(Resources, row(DBpediaAuthor))
  ).

