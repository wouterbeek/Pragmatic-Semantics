:- use_module(library(filesex)).

% Run file for PGC.
% This is used to run the PGC independent of any other projects/submodules.

:- initialization(run_pgc).

run_pgc:-
  % Entry point.
  current_prolog_flag(executable, Executable),
  source_file(run_pgc, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  assert(user:file_search_path(plc, ThisDir)),
  
  % Load in debug mode.
  % We do not expect production use of the PGC in isolation.
  ensure_loaded(plc(debug)).

