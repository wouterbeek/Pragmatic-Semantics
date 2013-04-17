:- module(
  qr_deb,
  [
    test_model_complexity/0
  ]
).

/** <module> QR DEB

Debug methods for QR.

@author Wouter Beek
@version 2012/07
*/

:- use_module(debug(cm_deb)).
:- use_module(generic(file_ext)).
:- use_module(generic(script_ext)).
:- use_module(qr(qr_eval)).



test_model_complexity:-
  start_script_mode,

  % Make sure there will be no errors due to doubly opened models.
  send(@app, removeModels),

  create_log_file(ilp, _File, Stream),
  current_log_stream(OldStream),
  set_current_log_stream(Stream),

  absolute_file_name(debug(ilp), HGPFileDirectory),
  test(
    path_walk_tree(HGPFileDirectory,'.*\.hgp$', HGPFiles),
    'Get a list of all HGP files',
    Stream
  ),
  length(HGPFiles, HGPLength),
  write_attribute_value_pair(
    Stream,
    'Amount of files found',
    HGPLength
  ),
  get_time(StartTime),

  maplist(evaluate_current_model(Stream), HGPFiles),

  get_time(EndTime),
  TotalTime is EndTime - StartTime,
  format(Stream, 'Total time is ~w', [TotalTime]),
  set_current_log_stream(OldStream),
  close_log_stream(Stream),
  stop_script_mode.

evaluate_current_model(Stream, HGPFile):-
  test_load_model(Stream, HGPFile),
  test_run_full_simulation(Stream),
  evaluate_current_model(Stream),
  test_close_model(Stream).
