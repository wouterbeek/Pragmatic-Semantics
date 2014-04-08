% The load file for the WebQR project.

:- multifile(user:project/2).
user:project('WebQR', 'Web-based Qualitative Reasoning engine.').

:- initialization(load_webqr).

load_webqr:-
  % Entry point.
  source_file(load_webqr, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  
  % WebQR
  assert(user:file_search_path(webqr, ThisDir)),
  assert(user:file_search_path(qsim, webqr('QSIM'))),
  use_remote_module(dbpedia(dbpedia_eq)),
  use_remote_module(webqr(webqr_uc)),
  use_remote_module(webqr(webqr_web)).

