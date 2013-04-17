:- module(
  gde,
  [
    continue_diagnosis/3, % +Diagnosis:diagnosis,
                          % -Diagnoses:ord_set(ord_set(component))
                          % -ProbePoints:list(point)
    start_diagnosis/3 % +Diagnosis:diagnosis
                      % -Diagnoses:ord_set(ord_set(component))
                      % -ProbePoints:list(point)
  ]
).

/** <module> General Disgnostic Engine (GDE).

This module contains the GDE methods.

@author Wouter Beek
@version Aug 2011 - Feb, Apr - May 2012, Aug 2012
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_build)).
:- use_module(atms(atms_db)).
:- use_module(atms(atms_env)).
:- use_module(atms(atms_export)).
:- use_module(ccm(ccm_export)).
:- use_module(ccm(ccm_api)).
:- use_module(diagnosis(diagnosis)).
:- use_module(gde(gde_exp)).
:- use_module(gde(gde_pred)).
:- use_module(gde(gde_probe)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).



% INITIALIZING A DIAGNOSIS %

%% start_diagnosis(
%%   +Diagnosis:diagnosis,
%%   -Diagnoses:ord_set(ord_set(component)),
%%   -ProbePoints:list(point)
%% ) is det.
% The diagnose predicate that is invocated the first time the diagnosis
% starts.
%
% @param Diagnosis A diagnosis use case.
% @param Diagnoses An ordered set of environments.
% @param ProbePoints A list of points. The order is relevant.

start_diagnosis(Diagnosis, Diagnoses, ProbePoints):-
  % CANDIDATE
  diagnosis_atms(Diagnosis, ATMS),
  empty_environment(ATMS, EmptyEnvironment),
  add_candidate(Diagnosis, EmptyEnvironment),

  % OBSERVATIONS
  % All expectations are observations.
  diagnosis_to_expectations(Diagnosis, Expectations),
  maplist(add_premise(ATMS), Expectations),
  % All input points are observations.
  input_space(InputSpace),
  space_to_points(InputSpace, InputPoints),
  maplist(add_premise(ATMS), InputPoints),
  % All generic points are observations.
  global_space(GlobalSpace),
  space_to_points(GlobalSpace, GlobalPoints),
  maplist(add_premise(ATMS), GlobalPoints),

  % DEBUG
  % Export to GraphViz and to RDF.
  if_then(debug_mode(true), write_ccm_to_dot(ccm_gde)),
  if_then(debug_mode(true), write_ccm_to_rdf),

  % DIAGNOSIS
  % Perform the inner diagnosis.
  continue_diagnosis(Diagnosis, Diagnoses, ProbePoints).



% RESUMING AN EXISTING DIAGNOSIS %

%% continue_diagnosis(
%%   +Diagnosis:diagnosis,
%%   -Diagnoses:ord_set(environment),
%%   -ProbePoints:list(point)
%% ) is det.
% Diagnosis after initialisation.
%
% @param Diagnosis A diagnosis.
% @param Diagnoses An ordered set of environments.
% @param ProbePoints An list of points. The order is relevant.

continue_diagnosis(Diagnosis, Candidates, ProbePoints):-
  % CONFLICTS
  conflict_recognition(Diagnosis, Conflicts),
  (
    debug_mode(true)
  ->
    maplist(environment_to_dui_label, Conflicts, ConflictLabels),
    atomic_list_concat(ConflictLabels, ',  ', ConflictLabel),
    debug(gde, '[CONFLICTS]\t~w', [ConflictLabel])
  ;
    true
  ),

  % CANDIDATES
  candidate_generation(Diagnosis, Conflicts, Candidates),
  maplist(environment_to_dui_label, Candidates, CandidateLabels), %DEB
  debug(gde_conflict, '[CANDIDATES] ~w', [CandidateLabels]),

  % PROBES
  candidates_to_probes(Diagnosis, Candidates, ProbePoints),
  debug(gde_probe, '[PROBE-POINTS] ~w', [ProbePoints]).

candidates_to_probes(Diagnosis, Candidates, ProbePoints):-
  candidates_to_probes_(Diagnosis, Candidates, ProbePointWeights_),
  (
    (
      ProbePointWeights_ == []
    ;
      first(ProbePointWeights_, _TopProbePoint/TopProbePointWeight),
      TopProbePointWeight < 0.1
    )
  ->
    learner(Diagnosis, Learner),
    (
      does_consider_component_cloud(Learner, UnpackableComponentCloud),
      unpackable_component_cloud(UnpackableComponentCloud)
    ->
      diagnosis_atms(Diagnosis, OldATMS),
      find_or_add_environment(
        OldATMS,
        [UnpackableComponentCloud],
        HierarchicalEnvironment
      ),
      environment_hierarchical_unpack(
        Diagnosis,
        HierarchicalEnvironment,
        UnpackedEnvironment,
        RemovedEnvironment
      ),
      setoff(
        NewCandidate,
        (
          member(Candidate, Candidates),
          update_environment(
            Candidate,
            RemovedEnvironment,
            UnpackedEnvironment,
            NewCandidate
          )
        ),
        NewCandidates
      ),
      maplist(environment_to_dui_label, NewCandidates, NewCandidateLabels), %DEB
      debug(gde, '[NEW-CANDIDATES] ~w', [NewCandidateLabels]),
      find_probe_point_weights(Diagnosis, NewCandidates, ProbePointWeights),
      if_then(debug_mode(true), write_ccm_to_dot(ccm_gde))
    ;
      ProbePointWeights = []
    )
  ;
    ProbePointWeights = ProbePointWeights_
  ),
  maplist(extract_element, ProbePointWeights, ProbePoints).

extract_element(Element/_Weight, Element).

% @tbd Remove this?
candidates_to_probes_(Diagnosis, Candidates, []):-
  diagnosis_atms(Diagnosis, ATMS),
  empty_environment(ATMS, EmptyEnvironment),
  Candidates == [EmptyEnvironment],
  !.
candidates_to_probes_(Diagnosis, Candidates, ProbePointWeights):-
  find_probe_point_weights(Diagnosis, Candidates, ProbePointWeights).



% CONFLICT RECOGNITION %

%% conflict_recognition(
%%   +Diagnosis:diagnosis,
%%   -Conflicts:ord_set(environment)
%% ) is det.
% Returns the list of conflicts.
% Does not deliver a result, because that can be easily
% obtained from the given diagnosis, i.e. the 'nogoods' set of its ATMS.
%
% @param Diagnosis A diagnosis.
% @param Conflicts An ordered set of environments.

conflict_recognition(Diagnosis, Conflicts):-
  diagnosis_atms(Diagnosis, ATMS),
  % Retrieve the old conflicts.
  diagnosis_conflicts(Diagnosis, OldConflicts),

  (
    debug_mode(true)
  ->
    diagnosis_component_clouds(Diagnosis, ComponentClouds),
    length(ComponentClouds, ComponentCloudsLength),
    Complexity is 2 ^ ComponentCloudsLength,
    % The empty environment is not considered.
    send(@pce, write_ln, 'Number of possible environments: ', Complexity),
    debug(gde, 'Number of possible environments: ~w', [Complexity])
  ;
    true
  ),

  % Start with the empty environment.
  % This iterates over all relevant environments and always succeeds.
  empty_environment(ATMS, FirstEnvironment),
  conflict_recognition2(Diagnosis, FirstEnvironment),
  !,
  atms_to_nogoods(ATMS, NewConflicts),

  flag(environment_flag, Iterations, 0),
  debug(
    gde,
    'Number of calculated environments: ~w of ~w',
    [Iterations, Complexity]
  ),
  send(@pce, write_ln, 'Generated environments: ', Iterations),

  (
    debug_mode(true)
  ->
    format(
      atom(Explanation),
      'Environment: ALL ; Conflicts: ~w',
      [NewConflicts]
    ),
    print_atms(ATMS, Explanation)
  ;
    true
  ),

  if_then(debug_mode(true), write_ccm_to_dot(ccm_gde)),

  % Do not include the old conflicts in the results.
  ord_subtract(NewConflicts, OldConflicts, Conflicts),
  maplist(store_conflict(Diagnosis), Conflicts).

%% conflict_recognition2(
%%   +Diagnosis:diagnosis,
%%   +Environment:environment
%% ) is det.

conflict_recognition2(Diagnosis, Environment):-
  % Derive any new conclusions (register in the ATMS), based on the (backward)
  % behavior rules for the components.
  predict_behavior(Diagnosis, Environment),

  % First check wheter there is not a new conflict caused by a new
  % observation.
  (
    debug_mode(true),
    diagnosis_atms(Diagnosis, ATMS),
    nogood(ATMS, Environment)
  ->
    environment_to_dui_label(Environment, EnvironmentDUILabel),
    debug(
      gde_conflict,
      '[CONFLICT DETECTED] ~w',
      [EnvironmentDUILabel]
    )
  ;
    true
  ),

  % Retrieve the next relevant environment.
  next_environment_small_to_big(Diagnosis, Environment, NextEnvironment),

  conflict_recognition2(Diagnosis, NextEnvironment).
% Never fail.
conflict_recognition2(_Diagnosis, _Environment).



% CANDIDATE GENERATION %

%% candidate_generation(
%%   +Diagnosis:diagnosis,
%%   +NewConflicts:ord_set(environment),
%%   -MinimalConflicts:ord_set(environment)
%% ) is det.
% Applies set-covering technique to calculate new candidate set.
%
% @param Diagnosis A diagnosis.
% @param NewConflicts An ordered set of components.
% @param MinimalConflicts An ordered set of components.

candidate_generation(Diagnosis, NewConflicts, MinimalCandidates):-
  % Old candidates.
  diagnosis_to_candidates(Diagnosis, OldCandidates),
  %%%%rdf_retractall(Diagnosis, diagnosis:has_candidate, _OldCandidate, ccm),

  % Generate new candidates based on old candidates.
  diagnosis_atms(Diagnosis, ATMS),
  update_candidates(ATMS, NewConflicts, OldCandidates, NewCandidates),
  minimize_environments(NewCandidates, MinimalCandidates).

  % Add new candidates.
  %%%%maplist(add_candidate(Diagnosis), NewCandidates).

%% update_candidates(
%%   +ATMS:atms,
%%   +NewConflicts,
%%   +OldCandidates,
%%   -NewCandidates
%% )

% No new conflicts.
update_candidates(_ATMS, [], Candidates, Candidates):-
  !.
% A new conflict.
update_candidates(
  ATMS,
  [NewConflict | NewConflicts],
  OldCandidates,
  NewCandidates
):-
  update_one_conflict(ATMS, NewConflict, OldCandidates, TempCandidates),
  minimize_environments(TempCandidates, Candidates),
  update_candidates(ATMS, NewConflicts, Candidates, NewCandidates).

%% update_one_conflict(
%%   +ATMS:atms
%%   +NewConflict:list(environment),
%%   +OldCandidates:list(environment),
%%   -NewCandidates:list(environment)
%% )

% Nothing to update.

update_one_conflict(_ATMS, _NewConflict, [], []):-
  !.
update_one_conflict(ATMS, Conflict, [Candidate | Tail], NewCandidates):-
  empty_environment(ATMS, EmptyEnvironment),
  intersection_environments([Conflict, Candidate], EmptyEnvironment),
  !,
  calculate_new_candidates(Conflict, Candidate, NewCandidates1),
  update_one_conflict(ATMS, Conflict, Tail, Candidates),
  ord_union(NewCandidates1, Candidates, NewCandidates).
update_one_conflict(
  ATMS,
  Conflict,
  [Candidate | Tail],
  [Candidate | NewCandidates]
):-
  update_one_conflict(ATMS, Conflict, Tail, NewCandidates).

calculate_new_candidates(Conflict, Candidate, NewCandidates):-
  setoff(
    NewCandidate,
    (
      member_environment(Assumption, Conflict),
      extend_environment(Assumption, Candidate, NewCandidate)
    ),
    NewCandidates
  ).
