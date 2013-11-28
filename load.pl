% The load file for the PraSem project.

:- multifile(user:project_name/1).
user:project_name('PraSem').

:- initialization(load_prasem).

load_prasem:-
  % PraSem
  use_module(prasem(prasem)),
  
  % Load the various projects.
  load_project('Beekeeper'),
  load_project('EnergyLabels'),
  load_project('IOTW'),
  load_project('WebQR').

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

