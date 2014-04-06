#!/home/wbeek/bin/swipl

% The run file for the CKAN project.

:- initialization(run_ckan).

run_ckan:-
  % Entry point.
  source_file(run_ckan, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % Project-specific command-line options. To be caught up by PGC.
  ensure_loaded(ckan(ckan_clas)),
  
  % PGC
  load_pgc(project),
  
  % CKAN load file.
  ensure_loaded(load).

load_pgc(_Project):-
  user:file_search_path(pgc, _Spec), !.
load_pgc(Project):-
  Spec =.. [Project,'PGC'],
  assert(user:file_search_path(pgc, Spec)),
  load_or_debug(pgc).

load_or_debug(Project):-
  predicate_property(user:debug_mode, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).

