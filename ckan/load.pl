% The load file for the CKAN project.

:- multifile(user:project/2).
user:project(
  'CKAN',
  'Generates an automated RDF version of CKAN datasets.'
).

:- initialization(load_ckan).

load_ckan:-
  % Entry point.
  source_file(load_ckan, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % CKAN
  assert(user:file_search_path(ckan, ThisDir)).

