:- module(
  cache_it,
  [
    cache_it1/4, % +Graph:atom
                 % :Goal
                 % +PredicatesFilter:ordset(iri)
                 % +Resource:iri
    cache_it2/5 % +Graph:atom
                % :Goal
                % +Resource:iri
                % -Resources:ordset(iri)
                % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> Cache it

Generic predicate for caching RDF results.

Possible instantiations for `Goal` are SPARQL_cache/4 and LOD_cache/4.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_remote_module(dcg(dcg_collection)).
:- use_remote_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_web(rdf_store_table)).
:- use_remote_module(sparql(sparql_db)).

:- debug(cache_it).



assert_proposition(Graph, [S,P,O]):-
  var(Graph), !,
  rdf_assert(S, P, O).
assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).

default_predicate_filter([
  dcterms:subject,
  rdf:type,
  rdfs:subClassOf,
  rdfs:subPropertyOf,
  skos:broader,
  skos:related,
  owl:sameAs
]).

%! cache_it1(
%!   +Graph:atom,
%!   :Goal,
%!   +PredicatesFilter:ordset(iri),
%!   +Resource:or([bnode,iri,literal])
%! ) is det.

:- meta_predicate(cache_it1(+,3,+,+)).
cache_it1(Graph, Goal, PredicatesFilter, [H|T]):- !,
  absolute_file_name(prasem('lod.log'), File, [access(write)]),
  open(File, write, _, [alias(lod_log)]),
  findall(
    X-[X],
    member(X, [H|T]),
    Pairs
  ),
  (nonvar(PredicatesFilter), ! ; default_predicate_filter(PredicatesFilter)),
  cache_it1(depth_first, Graph, Goal, PredicatesFilter, Pairs).
cache_it1(Graph, Goal, PredicatesFilter, Resource):-
  cache_it1(Graph, Goal, PredicatesFilter, [Resource]).


:- meta_predicate(cache_it1(+,+,3,+,+)).
cache_it1(_, _, _, _, []):- !.
cache_it1(Mode, Graph, Goal, PredicatesFilter, [X-HistX|T1]):-
  length(HistX, Depth),
  format(lod_log, '~:d\t', [Depth]),
  dcg_with_output_to(lod_log, list(rdf_term_name, HistX)),
  nl(lod_log),

  call(Goal, X, _, NeighborProps), !,
  
  % Filter on propositions that are included in results.
  exclude(old_proposition(Graph), NeighborProps, NewProps),

  % Update results: propositions.
  maplist(assert_proposition(Graph), NewProps),

  % Only predicate terms and object terms that occur
  % with predicate terms in the filter are visited later.
  propositions_to_resources(NewProps, PredicatesFilter, Ns1),
  
  % Exclude resources that are already fully asserted.
  exclude(old_resource(Graph), Ns1, Ns2),

  exclude(unwanted_neighbor(Graph), Ns2, Ns3),
  
  % We want to track paths for debugging purposes
  % (i.e. showing the path depth).
  maplist(resource_to_pair(HistX), Ns3, NewPairs),

  % Update resources that have to be visited.
  % Support breadth-first and depth-first modes.
  (
    Mode == breadth_first
  ->
    append(T1, NewPairs, T2)
  ;
    Mode == depth_first
  ->
    append(NewPairs, T1, T2)
  ),
  length(T2, NumberOfT2),
  length(Ns3, NumberOfNewResources),
  length(NewProps, NumberOfNewProps),
  dcg_with_output_to(atom(Name), rdf_term_name(X)),
  message(
    '~:d remain (depth ~:d) (new res ~:d props ~:d) (resource ~w)',
    [NumberOfT2,Depth,NumberOfNewResources,NumberOfNewProps,Name]
  ),

  % Recurse.
  cache_it1(Mode, Graph, Goal, PredicatesFilter, T2).
% The show must go on!
cache_it1(Mode, Graph, Goal, PredicatesFilter, [X-_|T]):-
  message('[FAILED] ~w', [X]),
  cache_it1(Mode, Graph, Goal, PredicatesFilter, T).

resource_to_pair(T, H, H-[H|T]).

propositions_to_resources(L1, PredicatesFilter, L2):-
  propositions_to_resources(L1, PredicatesFilter, [], L2).

propositions_to_resources([], _, Sol, Sol).
propositions_to_resources([[_,P,O]|T], PredicatesFilter, L1, Sol):-
  ord_add_element(L1, P, L2),
  (
    rdf_memberchk(P, PredicatesFilter)
  ->
    ord_add_element(L2, O, L3)
  ;
    L3 = L2
  ),
  propositions_to_resources(T, PredicatesFilter, L3, Sol).

unwanted_neighbor(Graph, Resource):-
  rdf([graph_mode(no_inst)], Resource, _, _, Graph), !.
unwanted_neighbor(_, Resource):-
  \+ rdf_is_bnode(Resource),
  \+ rdf_is_literal(Resource),
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  atomic_list_concat([_,dbpedia|_], '.', Domain).



%! cache_it2(
%!   +Graph:atom,
%!   :Goal,
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.

:- meta_predicate(cache_it2(+,3,+,-,-)).
cache_it2(_, _, [], [], []):- !.
cache_it2(Graph, Goal, [H|T], Resources, Propositions):- !,
  cache_it2(
    breadth_first,
    Graph,
    Goal,
    [H|T],
    [H],
    Resources,
    [],
    Propositions
  ),
  % DEB
  findall(
    [S,P,O,none],
    member([S,P,O], Propositions),
    Quadruples
  ),
  rdf_store_rows(Quadruples).
cache_it2(Graph, Goal, Resource, Resources, Propositions):-
  cache_it2(Graph, Goal, [Resource], Resources, Propositions).


%! cache_it2(
%!   +Mode:oneof([breadth_first,depth_first]),
%!   +Graph:atom,
%!   :Goal,
%!   +QueryTargets:list(or([bnode,iri,literal])),
%!   +ResourceHistory:ordset(or([bnode,iri,literal])),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   +PropositionHistory:ordset(list(or([bnode,iri,literal]))),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.

:- meta_predicate(cache_it2(+,+,3,+,+,-,+,-)).
% Base case.
cache_it2(_, _, _, [], VSol, VSol, PropsSol, PropsSol):- !.
% Recursive case.
cache_it2(Mode, Graph, Goal, [H1|T1], Vs1, VSol, Props1, PropsSol):-
  message('Resource ~w', [H1]),
  call(Goal, H1, Neighbors, NeighborProps), !,

  % Filter on propositions that are included in results.
  exclude(old_proposition(Graph), NeighborProps, NewProps),

  % Filter on resources that have to be visited.
  exclude(old_neighbor(Vs1, NewProps), Neighbors, NewNeighbors),

  % Update results: resources.
  ord_union(Vs1, NewNeighbors, Vs2),
  maplist(length, [NewNeighbors,Vs2], [NumberOfNewNeighbors,NumberOfVs2]),
  message(
    '~:d resources added (~:d in total)',
    [NumberOfNewNeighbors,NumberOfVs2]
  ),

  % Update results: propositions.
  ord_union(Props1, NewProps, Props2),
  maplist(length, [NewProps,Props2], [NumberOfNewProps,NumberOfProps2]),
  message(
    '~:d propositions added (~:d in total)',
    [NumberOfNewProps,NumberOfProps2]
  ),

  % Update resources that have to be visited.
  % Support breadth-first and depth-first modes.
  (
    Mode == breadth_first
  ->
    append(T1, NewNeighbors, T2)
  ;
    Mode == depth_first
  ->
    append(NewNeighbors, T1, T2)
  ),
  length(T2, NumberOfT2),
  message('~:d remaining', [NumberOfT2]),

  % Recurse.
  cache_it2(Mode, Graph, Goal, T2, Vs2, VSol, Props2, PropsSol).
% The show must go on!
cache_it2(Mode, Graph, Goal, [H|T], Vs, VSol, Props, PropsSol):-
  message('[FAILED] ~w', [H]),
  cache_it2(Mode, Graph, Goal, T, Vs, VSol, Props, PropsSol).

message(Format, Args):-
  format(user_output, Format, Args),
  nl(user_output),
  flush_output(user_output).

old_neighbor(Vs1, _, Element):-
  memberchk(Element, Vs1), !.
old_neighbor(_, NewProps, Element):-
  member(
    [_,'http://dbpedia.org/ontology/wikiPageExternalLink',Element],
    NewProps
  ), !.

old_proposition(Graph, [S,P,O]):-
  rdf([graph_mode(no_inst)], S, P, O, Graph), !.
old_proposition(Graph, [S,P,O]):-
  rdf_predicate_property(P, symmetric(true)),
  rdf([graph_mode(no_inst)], O, P, S, Graph), !.

old_resource(Graph, Resource):-
  rdf([graph_mode(no_inst)], Resource, _, _, Graph).

