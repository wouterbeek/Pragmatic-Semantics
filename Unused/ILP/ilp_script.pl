:- module(
  ilp_script,
  [
    test_ilp/0
  ]
).

/** <module> ILP Script

Automated scripts for ILP.

@author Wouter Beek
@version 2012/07
*/

:- use_module(generic(deb_ext)).
:- use_module(generic(file_ext)).
:- use_module(generic(script_ext)).
:- use_module(ilp(aleph6)).



test_ilp:-
  % Enter scripting.
  start_script_mode,
  absolute_file_name(data_ilp(.), Directory),
  test(
    path_walk_tree(Directory, '.*.b$', BackgroundFiles),
    'Get a list of all background files files',
    Stream
  ),
  length(BackgroundFiles, NumberOfLearningTasks),
  write_attribute_value_pair(
    Stream,
    'Amount of learning tasks found',
    NumberOfLearningTasks
  ),
  get_time(StartTime),
  maplist(ilp_file, BackgroundFiles),
  get_time(EndTime),
  TotalTime is EndTime - StartTime,
  format(Stream, 'Total time is ~w', [TotalTime]),
  % Exit scripting.
  stop_script_mode.
