:- module(
  flickrwrappr,
  [
    is_flickrwrappr_url/1, % +URL:iri
    flickrwrappr_cache/1, % +Graph:atom
    flickrwrappr_cache/3 % +Graph:atom
                         % -Resources:ordset(or([bnode,iri,literal]))
                         % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> FlickrWrappr

In DBpedia FlickWrappr resources are linked to DBpedia resources in triples
 like [1]. The RDF file retrieved at [2] describes resource [1].

~~~
[1]   http://dbpedia.org/resource/Monkey dbpprop:hasPhotoCollection http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/photos/Monkey
[2]   http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/photos/Monkey
[3]   http://dbpedia.org/resource/Monkey
~~~

Most of the time it is the case that the LOD description at an URL
 describes that URL.
But sometimes the LOD description at an URL describes
 the previous resource.
For example, the LOD description at location [2] describes resource [3].
While caching, location [2] is reached via triple [1],
 and the LOD description at location [2] makes assertions
 about resource [3].

@author Wouter Beek
@see http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/
@version 2014/01
*/

:- use_module(generics(uri_query)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(lod(lod_query)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbpprop, 'http://dbpedia.org/property/').



is_flickrwrappr_url(URL):-
  uri_components(
    URL,
    uri_components(_,'wifo5-03.informatik.uni-mannheim.de',Path,_,_)
  ),
  file_directory_name(Path, Dir),
  Dir == '/flickrwrappr/photos'.


%! flickrwrappr_cache(+Graph:atom) is det.
%! flickrwrappr_cache(
%!   +Graph:atom
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.

flickrwrappr_cache(Graph):-
  flickrwrappr_cache(Graph, _, Propositions),
  maplist(assert_proposition(Graph), Propositions).

flickrwrappr_cache(Graph, Resources, Propositions):-
  aggregate_all(
    set(IRI-URL),
    rdf(IRI, dbpprop:hasPhotoCollection, URL, Graph),
    Pairs
  ),
  maplist(flickrwrappr_cache_url, Pairs, Resourcess, Propositionss),
  maplist(ord_union, [Resourcess,Propositionss], [Resources,Propositions]).

flickrwrappr_cache_url(URL1-IRI, Resources, Propositions):-
  uri_query_add(URL1, format, rdf, URL2),
  lod_local_query([], URL2, _NoGraph, IRI, Resources, Propositions).

assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).

