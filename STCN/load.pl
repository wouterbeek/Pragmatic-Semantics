% The load file for the STCN project.

:- multifile(user:project/2).
user:project('STCN', 'Short Title Catalogue of the Netherlands').

:- initialization(load_stcn).

load_stcn:-
  % Entry point.
  source_file(load_stcn, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % STCN
  assert(user:file_search_path(stcn,    ThisDir        )),
  assert(user:file_search_path(kmc,     stcn('KMC'    ))),
  assert(user:file_search_path(picarta, stcn('Picarta'))),
  assert(user:file_search_path(wnt,     stcn('WNT'    ))),
  
  use_module(stcn(stcn_web)).

