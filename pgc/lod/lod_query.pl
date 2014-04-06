:- module(
  lod_query,
  [
    lod_cache/2, % +Resource:or([bnode,iri,literal])
                 % +Graph:atom
    lod_cache/3, % +Resource:or([bnode,iri,literal])
                 % -Resources:ordset(or([bnode,iri,literal]))
                 % -Propositions:ordset(list(or([bnode,iri,literal])))
    lod_local_query/6 % +Options:list(nvpair)
                      % +URL:url
                      % ?Graph:atom
                      % +Resource:or([bnode,iri,literal])
                      % -Resources:ordset(or([bnode,iri,literal]))
                      % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> LOD query

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_remote_module(generics(typecheck)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(http(http_download)).
:- use_module(library(aggregate)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_remote_module(lod(flickrwrappr)).
:- use_remote_module(lod(lod_location)).
:- use_remote_module(os(file_mime)).
:- use_remote_module(rdf(rdf_meta)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(sparql(sparql_cache)).



lod_cache(IRI, Graph):-
  lod_cache(IRI, _, Propositions),
  forall(
    member([S,P,O], Propositions),
    rdf_assert(S, P, O, Graph)
  ).


% Flickrwrappr LOD description.
% We extract these later, see module [flickrwrappr].
lod_cache(URL, [], []):-
  is_flickrwrappr_url(URL), !.

% Query a registered SPARQL endpoint.
lod_cache(IRI, Resources, Propositions):-
  sparql_cache(IRI, Resources, Propositions), !.

% Download a LOD description based on the IRI prefix.
lod_cache(IRI, Resources, Propositions):-
  rdf_global_id(Prefix:_, IRI),
  (
    lod_location(Prefix, URL), !
  ;
    URL = IRI
  ),
  lod_local_query([], URL, Prefix, IRI, Resources, Propositions).

% Based on the entire IRI we can download a LOD description.
% Sometimes we need special HTTP request headers in order to receive
% machine-readable content upon dereferencing.
lod_cache(IRI, Resources, Propositions):-
  is_of_type(uri, IRI), !,
  rdf_global_id(Prefix:_, IRI),
  findall(
    request_header(N=V),
    lod_header(Prefix, N, V),
    O1
  ),
  lod_local_query(O1, IRI, _NoGraph, IRI, Resources, Propositions).


%! lod_local_query(
%!   +Options:list(nvpair),
%!   +URL:url,
%!   +Graph:atom,
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% The options are passed to download_to_file/3 -> http_goal -> http_open/3.

lod_local_query(_, _, Graph, Resource, Resources, Propositions):-
  rdf_graph(Graph), !,
  lod_local_query_on_loaded_graph(Resource, Resources, Propositions, Graph).
lod_local_query(O1, URL, Graph, Resource, Resources, Propositions):-
  catch(download_to_file(O1, URL, File), _, fail),
  lod_local_query_on_file(File, Graph, Resource, Resources, Propositions).

% Potential RDF! Let's try to load it in a graph.
lod_local_query_on_file(File, Graph, Resource, Resources, Propositions):-
  file_mime(File, MIME),
  rdf_mime(MIME), !,
  lod_local_query_on_graph(
    File,
    MIME,
    Graph,
    Resource,
    Resources,
    Propositions
  ).
% There is no joy in this: no RDF.
lod_local_query_on_file(File, _, _, [], []):-
  debug(cache_it, 'No RDF in file ~w.', [File]),
  delete_file(File).


% The graph first needs to be loaded.
lod_local_query_on_graph(
  File,
  MIME,
  Graph,
  Resource,
  Resources,
  Propositions
):-
  % If graph is nonvar, it is kept.
  % If graph is var, it is erased.
  rdf_setup_call_cleanup(
    [graph(Graph),mime(MIME)],
    File,
    lod_local_query_on_loaded_graph(Resource, Resources, Propositions)
  ).

lod_local_query_on_loaded_graph(Resource, Resources, Propositions, Graph):-
  aggregate_all(
    set([Resource,P,O]),
    rdf(Resource, P, O, Graph),
    Propositions
  ),
  ord_union(Propositions, Resources).

