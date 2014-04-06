% The load file for the Identity on the Web (IOTW) project.

:- multifile(user:project/2).
user:project(
  'IOTW',
  'Identity on the Web. Researching the owl:sameAs relation.'
).

:- set_prolog_stack(global, limit(2*10**9)).

:- initialization(load_iotw).

load_iotw:-
  % Entry point.
  source_file(load_iotw, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % IOTW
  assert(user:file_search_path(iotw, ThisDir)),
  assert(user:file_search_path(iotw_exp, iotw('Experiments'))),
  use_module(iotw(iotw_web)).

