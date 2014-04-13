% The run file for the PraSem project.

:- multifile(user:project/2).
user:project('PraSem', 'Pragmatic Semantics for the Web of Data').

:- initialization(run_prasem).

run_prasem:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),

  ensure_loaded(prolog_repository),
  prolog_repository(local, ProjectDir),

  % Load PLC index.
  directory_file_path(ProjectDir, 'Prolog-Library-Collection', PlcDir),
  absolute_file_name(
    index,
    PlcIndexFile,
    [access(read),file_type(prolog),relative_to(PlcDir)]
  ),
  setup_call_cleanup(
    ensure_loaded(PlcIndexFile),
    index(PlcDir),
    unload_file(PlcIndexFile)
  ),

  % Load PraSem.
  use_module(prasem(prasem_clas)).

