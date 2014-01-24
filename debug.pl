% Debug file for the PraSem project.

% Indicate to the support modules that we are running in debug mode.
:- assert(user:debug_project).

:- [run].

:- use_module('LOD'('LOD_query')).
:- use_module('LOD'(cache_it)).
:- cache_it(
  'LOD-test',
  'LOD_cache',
  'http://dbpedia.org/resource/Monkey',
  _,
  _
).

