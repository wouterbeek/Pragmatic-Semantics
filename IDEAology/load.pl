% The load file for the IDEAology project.

:- multifile(user:project/2).
user:project('IDEAology', 'An ontology of ideas.').

:- initialization(load_id).

load_id:-
  % Entry point.
  source_file(load_id, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % IDEAology
  assert(user:file_search_path(id, ThisDir)),
  use_module(id(id)).

