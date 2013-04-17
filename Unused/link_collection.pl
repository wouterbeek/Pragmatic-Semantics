:- module(
  link_collection,
  [
    increment_indegree/1, % +Link:resource
    increment_outdegree/1, % +Link:resource
    indegree/2, % ?Link:resource
                % ?Indegree:integer
    link/2, % ?Local:resource
            % ?Remote:resource
    outdegree/2, % ?Link:resource
                 % ?Outdegree:integer
    site/1, % ?Link:resource
    store_new_link/2, % +Local:resource
                      % +Remote:resource
    store_new_uri/1 % +URI:resource
  ]
).

/** <module> Link collection

Link collection gathered by crawler.

@author Wouter Beek
@version 2012/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).


increment_indegree(Link):-
  rdf_increment(Link, rdf:indegree, prasem).

increment_outdegree(Link):-
  rdf_increment(Link, rdf:outdegree, prasem).

indegree(Link, Indegree):-
  nonvar(Link),
  !,
  indegree_(Link, Indegree),
  !.
indegree(Link, Indegree):-
  indegree_(Link, Indegree).

indegree_(Link, Indegree):-
  rdf_datatype(Link, rdf:indegree, integer, Indegree, prasem).

link(Local, Remote):-
  rdf(Local, rdf:link, Remote, prasem).

outdegree(Link, Outdegree):-
  nonvar(Link),
  !,
  outdegree_(Link, Outdegree),
  !.
outdegree(Link, Outdegree):-
  outdegree_(Link, Outdegree).

outdegree_(Link, Outdegree):-
  rdf_datatype(Link, rdf:outdegree, integer, Outdegree, prasem).

site(Link):-
  rdfs_individual_of(Link, rdf:site).

store_new_link(Local, Remote):-
  rdf_assert(Local, rdf:link, Remote, prasem).

store_new_uri(URI):-
  rdf_assert(URI, rdf:type, rdf:site, prasem),
  rdf_assert_datatype(URI, rdf:indegree, integer, 0, prasem),
  rdf_assert_datatype(URI, rdf:outdegree, integer, 0, prasem),
  debug(link_collection, 'New URI: ~w', [URI]).
