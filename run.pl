% The run file for the PraSem project.

:- set_prolog_flag(encoding, utf8).

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

:- use_module(library(filesex)).

:- multifile(user:project/2).
user:project('PraSem', 'Pragmatic Semantics for the Web of Data').

:- initialization(run_prasem).

run_prasem:-
  % Entry point.
  source_file(run_prasem, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  assert(user:file_search_path(prasem, ThisDir)),
  
  % Prolog repository.
  ensure_loaded(prolog_repository),
  prolog_repository(local, ThisDir),
  
  % The PLC is available as a Git submodule.
  directory_file_path(ThisDir, 'Prolog-Library-Collection', PlcDir),
  absolute_file_name(
    index,
    PlcIndexFile,
    [access(read),file_type(prolog),relative_to(PlcDir)]
  ),
  setup_call_cleanup(
    ensure_loaded(PlcIndexFile),
    assert_index(PlcDir),
    unload_file(PlcIndexFile)
  ),
  
  % Process the `project` command-line option.
  ensure_loaded(prasem_clas),
  use_module(pl(pl_clas)).
