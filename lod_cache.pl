% Script for developing the LOD cacher.

:- [debug].

:- use_module('LOD'('LOD_query')).
:- use_module('LOD'(cache_it)).

go:-
  thread_create(
    cache_it1(
      'LOD-test',
      'LOD_cache',
      _,
      'http://dbpedia.org/resource/Banana'
    ),
    _,
    []
  ).

:- go.

