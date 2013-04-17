:- module(
  ccm_script,
  [
    test_ccm_generation/0,
    test_ccm_generation/2 % +Stream:stream
                          % +HGPFile:atom
  ]
).

/** <module> CCM DEBUG

Automatic debugging of CCM generation.

@author Wouter Beek
@version May - Jun 2012
*/

:- use_module(conceptual_modeling(cm_deb)).
:- use_module(generic(file_ext)).
:- use_module(generic(script_ext)).



test_ccm_generation:-
  start_script_mode,

  % Make sure there will be no errors due to doubly opened models.
  send(@app, removeModels),

  create_file(debug(ccm), test_ccm_generation, temporary, File),
  open(File, write, Stream, [close_on_abort(true), type(text)]),
  absolute_file_name(debug(hgp), HGPMainDirectory),
  test(
    path_walk_tree(HGPMainDirectory,'.*\\.hgp$', HGPFiles),
    'Get a list of all HGP files',
    Stream
  ),
  length(HGPFiles, HGPLength),
  write_attribute_value_pair(
    Stream,
    'Amount of files found: ',
    HGPLength
  ),
  get_time(StartTime),
  maplist(test_ccm_generation(Stream), HGPFiles),
  get_time(EndTime),
  TotalTime is EndTime - StartTime,
  format(Stream, 'Total time is ~w', [TotalTime]),
  close(Stream),

  stop_script_mode.

%% test_ccm_generation(+Stream:stream, +HGPFile:atom) is det.
% Tests the generation of the CCM for the given HGP model.
%
% @param Stream A stream.
% @param HGPFile The atomic name of an HGP model file.

test_ccm_generation(Stream, HGPFile):-
  write_attribute_value_pair(Stream, 'Current model', HGPFile),
  test_load_model(Stream, HGPFile),
  (
    get(@app?currentModel, learningSpaceNumber, 6)
  ->
    get(@app?currentModel, sortedInputSystems, SortedScenariosChain),
    chain_list(SortedScenariosChain, SortedScenarios),
    forall(
      member(Scenario, SortedScenarios),
      (
        test_run_full_simulation(Stream, Scenario),
        test_generate_ccm_(Stream)
      )
    )
  ;
    send(@app?currentModel, learningSpaceAtLeastNumber, 2)
  ->
    test_run_full_simulation(Stream),
    test_generate_ccm_(Stream)
  ;
    true
  ),
  test_close_model(Stream),
  format(Stream, '\n', []).

test_generate_ccm_(Stream):-
  test(generate, 'Generate the CCM', Stream).
