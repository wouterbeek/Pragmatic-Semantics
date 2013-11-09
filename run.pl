% The run file for the PraSem project.

:- initialization(run_prasem).

run_prasem:-
  % Entry point.
  source_file(run_prasem, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  assert(user:file_search_path(prasem, ThisDir)),
  
  % PGC
  load_pgc(project),
  
  % EnergyLabels
  absolute_file_name(
    prasem('EnergyLabels'),
    EL_Dir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    load,
    EL_File,
    [access(read),file_type(prolog),relative_to(EL_Dir)]
  ),
  ensure_loaded(EL_File),
  load_energylabels(EL_Dir).

load_pgc(_Project):-
  user:file_search_path(pgc, _Spec), !.
load_pgc(Project):-
  Spec =.. [Project,'PGC'],
  assert(user:file_search_path(pgc, Spec)),
  load_or_debug(pgc).

load_or_debug(Project):-
  predicate_property(debug_project, visible), !,
  Spec =.. [Project,debug],
  ensure_loaded(Spec).
load_or_debug(Project):-
  Spec =.. [Project,load],
  ensure_loaded(Spec).

