:- module(
  sparql_find,
  [
    sparql_find/3 % +Remote:atom
                    % +SearchTerm:or([atom,iri])
                    % -Resource:iri
  ]
).

/** <module> SPARQL find

Find a single resource based on a search term.

@author Wouter Beek
@version 2014/01
*/

:- use_remote_module(generics(typecheck)).
:- use_module(library(debug)).
:- use_remote_module(sparql(sparql_build)).
:- use_remote_module(sparql(sparql_cache)).
:- use_remote_module(sparql(sparql_ext)).



%! sparql_find(
%!   +Remote:atom,
%!   +SearchTerm:or([atom,iri]),
%!   -Resource:iri
%! ) is det.
% Returns the resource that best fits the given search term.
%
% If the search term is itself a concept, then this is returned.
% Otherwise, the remote is searched for a resource that is labeled with
%  the given search term.
%
% @arg Remote
% @arg SearchTerm
% @arg Resource

sparql_find(Remote, Resource, Resource):-
  is_of_type(iri, Resource), !,
  % @tbd This can be done more efficiently by just looking for
  %      the first triple.
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      1,
      _,
      _
    ),
    Query
  ),
  sparql_query(Remote, Query, _VarNames, Results),
  (
    Results == []
  ->
    debug(sparql_find, 'No results for resource ~w.', [Resource])
  ;
    true
  ).
sparql_find(Remote, SearchTerm, Resource):-
  phrase(
    sparql_formulate(
      _,
      _,
      [rdfs],
      select,
      true,
      [resource],
      [
        rdf(var(resource), rdfs:label, var(label)),
        filter(regex(var(label), at_start(SearchTerm), [case_insensitive]))
      ],
      inf,
      _,
      _
    ),
    Query
  ),
  sparql_query(Remote, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(sparql_find, 'Could not find a resource for \'~w\'.', [SearchTerm]),
    fail
  ;
    Resources = [row(Resource)|_]
  ).

