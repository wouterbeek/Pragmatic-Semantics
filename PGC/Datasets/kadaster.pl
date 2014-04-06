:- module(
  kadaster,
  [
    load_kadaster/0,
    query_kadaster/1, % +Resources:list(iri)
    query_kadaster/2, % +Subject
                      % +Resources:list(iri)
    scrape_kadaster/0
  ]
).

/** <module> Kadaster

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(sparql(sparql_build)).
:- use_remote_module(sparql(sparql_db)).
:- use_remote_module(sparql(sparql_ext)).

:- sparql_register_remote(kadaster, 'brk.kadaster.nl', default, '/sparql').



load_kadaster:-
  absolute_file_name(kadaster, File, [access(read)]),
  rdf_load([format(turtle)], kadaster, File).

query_kadaster(Resources):-
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [s,p,o],
      [rdf(var(s), var(p), var(o))],
      10,
      _,
      _
    ),
    Query
  ),
  sparql_query(kadaster, Query, _VarNames, Resources).

query_kadaster(S, Resources):-
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(S), var(p), var(o))],
      10,
      _,
      _
    ),
    Query
  ),
  sparql_query(kadaster, Query, _VarNames, Resources).

scrape_kadaster:-
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [s,p,o],
      [rdf(var(s), var(p), var(o))],
      inf,
      _,
      _
    ),
    Query
  ),
  sparql_query(kadaster, Query, _VarNames, Resources),
  forall(
    member(row(S,P,O), Resources),
    rdf_assert(S, P, O, kadaster)
  ),
  rdf_save([format(turtle)], kadaster, kadaster).

