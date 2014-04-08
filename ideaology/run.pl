% The run file for the IDEAology project.

:- initialization(run_id).

run_id:-
  % Entry point.
  source_file(run_id, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % PGC
  load_pgc(project),
  
  % IDEAology load file.
  ensure_loaded(load).

load_pgc(_Project):-
  user:file_search_path(plc, _Spec), !.
load_pgc(Project):-
  Spec =.. [Project,'PGC'],
  assert(user:file_search_path(plc, Spec)),
  load_or_debug(plc).

load_or_debug(Project):-
  predicate_property(user:debug_project, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).

