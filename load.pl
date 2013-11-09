% The load file for the PraSem project.

:- multifile(user:project_name/1).
user:project_name('PraSem').

:- initialization(load_prasem).

load_prasem:-
  % Entry point
  source_file(load_prasem, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  
  % PraSem
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(prasem, ThisDirectory)),
  
  % PGC
  load_pgc_(prasem),
  
  % Energy labels
  assert(user:file_search_path(el, prasem('EnergyLabels'))),
  load_or_debug_(el).

