%% load_self_only is det.
% Loads all Self1993 code.

load_self_only:-
  source_file(load_self_only, SelfOnlyFile),
  file_directory_name(SelfOnlyFile, SelfDirectory),
  atomic_concat(SelfDirectory, '/../internal_startup.pl', File),
  ensure_loaded(File),
  start_anything,
  use_module(self(self_base)).

:- load_self_only.
