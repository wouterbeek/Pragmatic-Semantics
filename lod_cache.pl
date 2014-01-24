% Script for developing the LOD cacher.

:- [debug].

:- use_module('LOD'('LOD_query')).
:- use_module('LOD'(cache_it)).
:- cache_it(
  'LOD-test',
  'LOD_cache',
  'http://dbpedia.org/resource/Monkey',
  _,
  _
).

