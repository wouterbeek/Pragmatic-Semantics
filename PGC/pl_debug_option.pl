% Option: debug.

user:cmd_option(d, debug, boolean,
    'Run in debug mode. This shows debug messages and loads debug tools.').

user:process_cmd_option(debug(true)):-
  assert(user:debug_mode),
  ensure_loaded(pl(pl_debug)).
user:process_cmd_option(debug(false)).

