% The load file for the PraSem project.

:- multifile(user:project_name/1).
user:project_name('PraSem').

:- initialization(load_prasem).

load_prasem:-
  % PraSem
  use_module(prasem(prasem)),
  
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

