% PGC initialization file.
% This is called from [load].

:- use_remote_module(generics(logging)).
:- use_remote_module(pl(pl_clas)).
:- use_remote_module(pl(pl_version)).

:- ensure_remote_loaded(plc(pl_debug_option)).

:- initialization(init_pgc).



init_pgc:-
  % Check SWI-Prolog version.
  check_pl_version,

  % Set data subdirectory.
  process_options(_),

  % Start logging.
  start_log.

