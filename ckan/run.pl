#!/home/wbeek/bin/swipl

% The run file for the CKAN project.

:- initialization(run_ckan).

run_ckan:-
  % Entry point.
  source_file(run_ckan, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  
  % PGC
  load_pgc(project),
  
  % Project-specific command-line options. To be caught up by PGC.
  ensure_loaded(ckan(ckan_clas)),
  
  % CKAN load file.
  ensure_loaded(load).

