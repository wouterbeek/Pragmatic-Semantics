% The load file for the LOD Observatory project.

:- multifile(user:project/2).
user:project(
  'LOD Observatory',
  'Generates an automated overview of the state of the LOD cloud.'
).

:- initialization(load_lodobs).

load_lodobs:-
  % Entry point.
  source_file(load_lodobs, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % LODObs
  assert(user:file_search_path(lodobs, ThisDir)),
  
  % DEB
  use_remote_module(rdf_web(rdf_tabular)),
  
  use_remote_module(lodobs(lod_observatory)).

