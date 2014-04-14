% The run file for the PraSem project.

:- set_prolog_flag(encoding, utf8).

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(option)).

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
  use_module(pl(pl_clas)),
  prasem_process_options.


prasem_process_options:-
  read_options(O1),
  prasem_process_options(O1, []).

prasem_process_options(O1, L):-
  select_option(data(Data), O1, O2), !,
  user:process_option(data(Data)),
  prasem_process_options(O2, L).
prasem_process_options(O1, T):-
  select_option(project(Project), O1, _), !,
  user:process_option(project(Project)),
  
  % After loading a project there may be more options.
  read_options(O3),
  exclude(prasem_data, O3, O4),
  exclude(prasem_project(T), O4, O5),
  prasem_process_options(O5, [Project|T]).
prasem_process_options(O1, _):-
  process_options(O1).

prasem_data(data(_)).

prasem_project(Projects, project(Project)):-
  memberchk(Project, Projects).

