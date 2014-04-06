% The load file for the EnergyLabels project.

:- multifile(user:project/2).
user:project(
  'EnergyLabels',
  'XML to Linked Data conversion of energy labels data.'
).

:- initialization(load_el).

load_el:-
  % Entry point.
  source_file(load_el, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % EnergyLabels
  assert(user:file_search_path(el, ThisDir)),
  assert(user:file_search_path(el_data, data(el))),
  use_module(el(el_app)).

