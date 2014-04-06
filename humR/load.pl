% The load file for the humR project.

:- multifile(user:project/2).
user:project(
  humR,
  'Digital Humanities + R: statistics support\c
   for querying historical datasets.'
).

:- use_module(pl(pl_package)).

:- initialization(load_humR).

load_humR:-
  % Entry point.
  source_file(load_humR, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % Package "Real"
  load_pl_package(real),
  
  % humR
  assert(user:file_search_path(humR, ThisDir)),
  use_module(humR(humR_web)).

