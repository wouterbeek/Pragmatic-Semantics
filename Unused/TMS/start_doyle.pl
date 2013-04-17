start_doyle:-
  source_file(start_doyle, This),
  file_directory_name(This, Directory),
  atomic_concat(Directory, '/../internal_startup.pl', File),
  ensure_loaded(File),
  start_anything,
  ensure_loaded(tms(doyle)),
  doyle:test.

:- start_doyle.
