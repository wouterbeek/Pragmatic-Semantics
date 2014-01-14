% The load file for the Semantic URIs project.

:- multifile(user:project_name/1).
user:project_name('SemURI').

:- initialization(load_semuri).

load_semuri:-
  % Entry point.
  source_file(load_semuri, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % SemURIs
  assert(user:file_search_path(semuri, ThisDir)),
  
  % DEB
  use_module(rdf_web(rdf_table)),
  use_module(rdf_web(rdf_tabular)),
  
  use_module(semuri(semuri_script)).

