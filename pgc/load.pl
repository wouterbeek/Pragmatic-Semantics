% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.

:- set_prolog_flag(encoding, utf8).

:- initialization(load_pgc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_pgc:-
  use_module(library(apply)),
  use_module(library(prolog_pack)),

  source_file(load_pgc, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(plc, ThisDirectory)),

  % If there is no outer project, then PGC is the project.
  once((
    user:file_search_path(plc, _)
  ;
    assert(user:file_search_path(plc, ThisDirectory))
  )),

  set_project,

  assert(user:prolog_file_type(html, 'text/html')),
  assert(user:prolog_file_type(md,   'text/markdown')),
  assert(user:prolog_file_type(txt,  'text/plain')),

  ensure_loaded(plc(init)).


% If there is no outer project, then PGC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PGC', 'Prolog Generics Collection')).

