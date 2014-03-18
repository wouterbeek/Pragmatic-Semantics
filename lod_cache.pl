% Script for developing the LOD cacher.

:- [debug].

:- use_module(lod(lod_query)).
:- use_module(lod(cache_it)).

go:-
  thread_create(
    cache_it1(
      'LOD-test',
      lod_cache,
      _,
      'http://dbpedia.org/resource/Banana'
    ),
    _,
    []
  ).

:- go.

