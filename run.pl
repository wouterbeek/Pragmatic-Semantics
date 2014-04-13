% The run file for the PraSem project.

:- multifile(user:project/2).
user:project('PraSem', 'Pragmatic Semantics for the Web of Data').

:- initialization(run_prasem).

run_prasem:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),
  
  ensure_loaded(prolog_repository),
  prolog_repository(local),
  
  % Load PLC index.
  absolute_file_name('.', ThisDir, [access(read),file_type(directory)]),
  directory_file_path(ThisDir, plc, PlcDir),
  load_index(PlcDir),
  
  % Load PraSem.
  use_module(prasem(prasem_clas)).

