% The load file for the Semantic URIs project.

:- multifile(user:project/2).
user:project('SemURI', 'Automated evaluation of the semantics of URIs.').

:- initialization(load_su).

load_su:-
  % Entry point.
  source_file(load_su, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % SemURIs
  assert(user:file_search_path(su, ThisDir)),
  
  % Debug tools.
  use_remote_module(rdf_web(rdf_store_table)),
  use_remote_module(rdf_web(rdf_tabular)),
  
  use_remote_module(su(su_ap)),
  use_remote_module(su(su_btc)).

