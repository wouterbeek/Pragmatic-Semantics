#!/home/wbeek/bin/swipl

% The run file for the Beekeeper project.

:- initialization(run_bk).

run_bk:-
  % Entry point.
  source_file(run_bk, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % PGC
  load_pgc(project),
  
  % Beekeeper
  ensure_loaded(load).

load_pgc(_Project):-
  user:file_search_path(plc, _), !.
load_pgc(Project):-
  compound_name_arguments(Spec, Project, ['PGC']),
  assert(user:file_search_path(plc, Spec)),
  load_or_debug(plc).

load_or_debug(Project):-
  predicate_property(user:debug_mode, visible), !,
  compound_name_arguments(Spec, Project, [debug]),
  ensure_loaded(Spec).
load_or_debug(Project):-
  compound_name_arguments(Spec, Project, [load]),
  ensure_loaded(Spec).

