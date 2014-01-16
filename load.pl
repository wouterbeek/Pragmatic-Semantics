% The load file for the PraSem project.

:- use_module(os(run_ext)).

:- multifile(user:project_name/1).
user:project_name('PraSem').

:- initialization(load_prasem).

load_prasem:-
  assert(user:file_search_path(data, project('Data'))),
  
  % PraSem
  use_module(prasem(prasem)),
  
  % Load the various projects.
  %load_project('Beekeeper'),
  %load_project('EnergyLabels'),
  %load_project(humR),
  %load_project('IOTW'),
  %load_project('SacknerArchive'),
  %load_project('SemURI'),
  %load_project('STCN'),
  load_project('WebQR'),

  % Enumerate the external program support
  % for the currently loaded modules.
  list_external_programs.

load_project(Project):-
  absolute_file_name(
    prasem(Project),
    Dir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    load,
    File,
    [access(read),file_type(prolog),relative_to(Dir)]
  ),
  ensure_loaded(File).

