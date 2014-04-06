% The load file for the SWAG project.

:- multifile(user:project/2).
user:project('SWAG', 'The Social Web of the Avant-Garde.').

:- initialization(load_swag).

load_swag:-
  % Entry point.
  source_file(load_swag, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % SWAG
  assert(user:file_search_path(swag, ThisDir)),
  use_module(swag(swag)).

