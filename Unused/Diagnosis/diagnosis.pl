:- module(
  diagnosis,
  [
% ADDITIONAL KNOWLEDGE REQUESTS
    ask_question/3, % +Questioner:agent
                    % +Question:point_cloud
                    % +Answerer:agent

% COMMUNICATION
    locate_expression/1, % +Expression:expression
    locate_culprit/3, % +SupportExpression:support_expression
                      % -Object:object
                      % -Sentence:atom
    register_method/1, % +RegisterRegistree:atom/atom

% DIAGNOSES %
    add_candidate/2, % +Diagnosis:diagnosis
                     % +Candidate:environment
    add_diagnosis/2, % +Learner:agent
                     % -Diagnosis:diagnosis
    add_dialog/2, % +Diagnosis:diagnosis
                  % +Dialog:dialog
    add_expectation/2, % +Diagnosis:diagnosis
                       % +Expectation:point
    diagnosis/1, % ?Diagnosis:diagnosis
    diagnosis_atms/2, % ?Diagnosis:diagnosis
                      % ?ATMS:atms
    diagnosis_candidate/2, % ?Diagnosis:diagnosis
                           % ?Candidate:environment
    diagnosis_conflict/2, % ?Diagnosis:diagnosis
                          % ?Conflict:environment
    diagnosis_conflicts/2, % +Diagnosis:diagnosis
                           % -Conflicts:ord_set(environment)
    diagnosis_current_probe/2, % ?Diagnosis:diagnosis
                               % ?CurrentProbe:point
    diagnosis_dialog/2, % ?Diagnosis:diagnosis
                        % ?Dialog:diagnosis_dialog
    diagnosis_id/2, % ?Diagnosis:diagnosis
                    % ?ID:integer
    diagnosis_probable_point/2, % ?Diagnosis:diagnosis
                                % ?Point:point
    diagnosis_probe/2, % ?Diagnosis:diagnosis
                       % ?Probe:point
    diagnosis_to_candidates/2, % +Diagnosis:diagnosis
                               % -Candidates:ord_set(environment)
    diagnosis_to_expectations/2, % +Diagnosis:diagnosis
                                 % -Expectations:ord_set(point)
    diagnosis_to_probable_points/2, % +Diagnosis:diagnosis
                                    % -Points:ord_set(point)
    expectation/2, % ?Diagnosis:diagnosis
                   % ?Expectation:point
    remove_candidate/2, % +Diagnosis:diagnosis
                        % +Candidate:environment
    store_conflict/2, % ?Diagnosis:diagnosis
                      % ?Conflict:environment

% MODES
    mode/1, % ?Mode:atom
    add_mode/1, % +Mode:atom
    remove_mode/1, % +Mode:atom

% OBSERVATIONS
    observation/2, % ?ATMS:atms
                   % ?Point:point

% RUN
    ask_probes/3, % +Diagnosis:diagnosis
                  % +Candidates:ord_set(environment)
                  % +Probes:list(point)
    continue_diagnosis/1, % +Answer:uri
    continue_diagnosis/2, % +Diagnosis:diagnosis
                          % +Answer:uri
    start_diagnosis/1, % +Diagnosis:diagnosis
    start_repair/1, % +Diagnosis:diagnosis

% STAGE
    stage/2, % +Diagnosis:diagnosis
             % +ExpectationPoint:point

% STORE
    stage_to_store/1, % +Diagnosis:diagnosis
    store_point/2 % +Diagnosis:diagnosis
                  % +Point:point
  ]
).

/** <module> Diagnosis

The diagnosis API.
This is how interfaces use the diagnosis use case.
Interfaces include scripts (for automation), GUI's (for expert
interaction), and VC's for (learner interaction).

@author Wouter Beek
@version 2012/04-2012/08
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_build)).
:- use_module(atms(atms_env)).
:- use_module(atms(atms_export)).
:- use_module(ccm(ccm_aggr)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_conflict)).
:- use_module(ccm(ccm_export)).
:- use_module(ccm(ccm_verb)).
:- use_module(dui(diagnosis_character)).
:- use_module(dui(diagnosis_dialog)).
:- use_module(gde(gde)).
:- use_module(gde(gde_exp)).
:- use_module(gde(missing_component)).
:- use_module(generic(deb_ext)).
:- use_module(generic(meta_ext)).
:- use_module(generic(os_ext)).
:- use_module(ile(agent)).
:- use_module(ile(use_case)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qr(qr_api)).
:- use_module(qr(qr_quest)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).

%% mode(-Mode:atom) is det.
% Returns the current mode.
% Possible mode values:
%   1. done
%   2. expectations
%   3. missing_component
%   4. probes
%
% @param Mode An atomic mode name.

:- dynamic(mode/1).
:- dynamic(diagnosis_dialog/2).

:- dynamic(registered_method(_Register, _Registree)).



:-
  rdf_assert(diagnosis:diagnosis, rdfs:subClassOf, ile:use_case, ccm),
  rdfs_assert_label(diagnosis:diagnosis, 'The diagnosis use cases.', ccm),

  rdf_assert(diagnosis:relation, rdfs:subPropertyOf, rdfs:'Property', ccm),
  rdfs_assert_label(diagnosis:relation, 'A diagnosis relation.', ccm),

  rdf_assert(diagnosis:has_probe_point, rdfs:subPropertyOf, diagnosis:relation, ccm),
  rdfs_assert_label(diagnosis:has_probe_point, 'A probe point for a diagnosis.', ccm),

  rdf_assert(diagnosis:current_probe_point, rdfs:subPropertyOf, diagnosis:has_probe_point, ccm),
  rdfs_assert_label(diagnosis:current_probe_point, 'The current probe point for a diagnosis.', ccm),

  rdf_assert(diagnosis:has_role, rdfs:subPropertyOf, use_case:has_role, ccm),
  rdfs_assert_label(diagnosis:has_role, 'A diagnosis has a role.', ccm),

  rdf_assert(diagnosis:has_learner, rdfs:subPropertyOf, use_case:has_learner, ccm),
  rdfs_assert_label(diagnosis:has_learner, 'A diagnosis has a learner role.', ccm),

  rdf_assert(diagnosis:has_teacher, rdfs:subPropertyOf, use_case:has_teacher, ccm),
  rdfs_assert_label(diagnosis:has_teacher, 'A diagnosis has a teacher role.', ccm),

  rdf_assert(diagnosis:has_staged, rdfs:subPropertyOf, diagnosis:relation, ccm),
  rdfs_assert_label(diagnosis:has_staged, 'A diagnosis has a staged point.', ccm),

  rdf_assert(ile:has_diagnosis, rdfs:subPropertyOf, ile:has_use_case, ccm),
  rdfs_assert_label(ile:has_diagnosis, 'An ILE has a diagnosis use case.', ccm),

  flag(diagnosis_id, _OldDiagnosisID, 0).



% ASKING & ANSWERING QUESTIONS %

%% ask_question(
%%   +Questioner:agent,
%%   +Question:point_cloud,
%%   +Answerer:agent
%% ) is det.
% Asks a questions and returns the answer that was given by the answerer.
%
% @param Questioner The agent asking the question.
% @param Question A point cloud.
% @param Answerer The agent answering the question.

ask_question(Questioner, Question, Answerer):-
  % Resource and identifier.
  flag(request_id, ProbeRequestID, ProbeRequestID + 1),
  atom_number(AtomicProbeRequestID, ProbeRequestID),
  rdf_global_id(request:AtomicProbeRequestID, Request),
  rdf_assert_datatype(Request, request:has_id, integer, AtomicProbeRequestID, probe_vc), %COW
  rdf_assert(Question, ca:has_request, Request, ccm),

  % Natural language question.
  point(Expression, QuestionSpace, Question),
  space_label(QuestionSpace, QuestionSpaceLabel),
  expression_to_question(QuestionSpaceLabel, Expression, QuestionSentence),
  rdf_assert_literal(Request, request:has_question, QuestionSentence, ccm),
  rdf_assert_literal(
    Request,
    request:has_question,
    QuestionSentence,
    probe_vc
  ), %COW

  rdf_assert_literal(Request, request:has_space, QuestionSpace, ccm),
  rdf_assert_literal(Request, request:has_space, QuestionSpace, probe_vc), %COW

  % Questioner.
  rdf_assert(Request, request:has_questioner, Questioner, ccm),
  rdf_assert(Questioner, agent:asked, Request, ccm),

  % Answerer.
  rdf_assert(Request, request:has_answerer, Answerer, ccm),
  rdf_assert(Answerer, agent:answered, Request, ccm),

  % Probe point cloud
  rdf_assert(Request, request:has_probe_point, Question, ccm),
  rdf_assert(Question, point_cloud:has_probe_request, Request, ccm),

  % Time.
  get_time(TimeStamp),
  rdf_assert_datatype(Request, ca:at_dateTime, dateTime, TimeStamp, ccm),

  % Answers.
  flag(answer_id, _OldAnswerID, 0),

  (
    derivative_quantity_value_expression(Expression)
  ->
    from_quantity_expression(Quantity, Expression),
    quantity_derivative_quantity_space(Quantity, DerivativeQuantitySpace),
    quantity_space_to_quantity_values(DerivativeQuantitySpace, Answers)
  ;
    magnitude_quantity_value_expression(Expression)
  ->
    from_quantity_expression(Quantity, Expression),
    quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace),
    quantity_space_to_quantity_values(MagnitudeQuantitySpace, Answers)
  ;
    inequality_expression(Expression)
  ->
    inequality_expression_definition(InequalityExpressionDefinition),
    % We only need the top of the inequality expression defintion hierarchy.
    !,
    findall(
      Answer,
      (
        rdfs_subclass_of(Answer, InequalityExpressionDefinition),
        rdfs_subclass_of(Answer, expression:magnitude),
        rdfs_subclass_of(Answer, expression:qq)
      ),
      Answers
    )
  ;
    support_expression(Expression)
  ->
    negative_quantity_influence_expression_definition(
      NegativeQuantityInfluenceExpressionDefinition
    ),
    negative_quantity_proportionality_expression_definition(
      NegativeQuantityProportionalityExpressionDefinition
    ),
    positive_quantity_influence_expression_definition(
      PositiveQuantityInfluenceExpressionDefinition
    ),
    positive_quantity_proportionality_expression_definition(
      PositiveQuantityProportionalityExpressionDefinition
    ),
    Answers = [
      NegativeQuantityInfluenceExpressionDefinition,
      NegativeQuantityProportionalityExpressionDefinition,
      PositiveQuantityInfluenceExpressionDefinition,
      PositiveQuantityProportionalityExpressionDefinition
    ]
  ),
  maplist(add_answer(Request), Answers).

%% add_answer(+Request:request, +AnswerResource:uri) is det.
% Adds the given answer for the given request.
%
% @param Request A request.
% @param AnswerResource A resource identifying a possible answer.

add_answer(Request, AnswerResource):-
  % Create answer and identifier.
  rdf_datatype(Request, request:has_id, integer, RequestID, probe_vc), %COW
  flag(answer_id, AnswerID, AnswerID + 1),
  format(atom(AtomicAnswer), 'answer_~w_~w', [RequestID, AnswerID]),
  rdf_global_id(request:AtomicAnswer, Answer),

  % Link request to answer.
  rdf_assert(Request, request:has_answer, Answer, probe_vc), %COW

  % Assert label.
  rdfs_label(AnswerResource, AnswerLabel),!,
  rdfs_assert_label(Answer, AnswerLabel, probe_vc), %COW

  % Assert answer object.
  rdf_assert(Answer, request:has_object, AnswerResource, probe_vc). %COW



% COMMUNICATION

%% locate_expression(+Expression:expression) is det.
% Locates the given expression in the build environment.
%
% @param Expression An expression.

% First we come here. Use the registered method (either =dui= or =vc=).
locate_expression(Expression):-
  registered_method(locate_culprit_method, LocateCulpritsMethod),
  Call =.. [LocateCulpritsMethod, Expression],
  call(Call).

% Then we come here. But this is called from the registered methods.
locate_culprit(Expression, Objects, Sentence):-
  % Convert the support expression to a list of fragment individuals.
  locate_expression(Expression),
  locate_culprit1(Expression, Objects, Sentence).

% This one if for diagnosis results.
locate_culprit1(SupportExpression, _ListWithOneObject, Sentence):-
  %%%%open_in_build([Object]),
  expression_to_sentence_list(SupportExpression, SentenceList),
  append([something, may, be, wrong, with, ':'], SentenceList, SentenceList_),
  sentence_list_to_sentence(SentenceList_, Sentence).
% This one is for repair results.
locate_culprit1(_SupportExpression, _Objects, dummy_sentnece):-
  %%%%open_in_build(Objects),
  true.

%% register_method(+RegisterRegistree:atom/atom) is det.
% Registers the registrable method called =Register= with the
% registering method called =Registree=.
%
% @param Register The atomic name of a method.
% @param Registree The atomic name of a method.

register_method(Register/Registree):-
  retractall(registered_method(Register, _OldRegistree)),
  assert(registered_method(Register, Registree)).



% DIAGNOSES %

add_candidate(Diagnosis, Candidate):-
  rdf_assert(Diagnosis, diagnosis:has_candidate, Candidate, ccm).

%% add_diagnosis(+Learner:agent, -Diagnosis:diagnosis) is det.
% Adds a new diagnosis object.
%
% @param Diagnosis A diagnosis resource.

add_diagnosis(Learner, Diagnosis):-
  % Create the diagnosis.
  flag(diagnosis_id, ID, ID + 1),
  atom_number(AtomID, ID),
  rdf_global_id(diagnosis:AtomID, Diagnosis),
  rdf_assert(Diagnosis, rdf:type, diagnosis:diagnosis, ccm),
  rdf_assert_datatype(Diagnosis, diagnosis:has_id, integer, AtomID, ccm),
  rdf_assert(Diagnosis, diagnosis:has_learner, Learner, ccm),

  % Create the first ATMS.
  add_atms(ATMS),
  rdf_assert(Diagnosis, diagnosis:has_atms, ATMS, ccm),

  % Make sure the CCM is available.
  true.

add_dialog(Diagnosis, Dialog):-
  assert(diagnosis_dialog(Diagnosis, Dialog)).

%% add_expectation(+Diagnosis:diagnosis, +Expectation:point) is det.

add_expectation(Diagnosis, Expectation):-
  rdf_assert(Diagnosis, diagnosis:has_expectation, Expectation, ccm).

%% diagnosis(Diagnosis) is nondet.
% Diagnosis resources.
% These store information about a specific diagnostic interaction sequence.
%
% @param Diagnosis A diagnosis resource.

diagnosis(Diagnosis):-
  rdfs_individual_of(Diagnosis, diagnosis:diagnosis).

diagnosis_atms(Diagnosis, ATMS):-
  rdf(Diagnosis, diagnosis:has_atms, ATMS, ccm).

diagnosis_candidate(Diagnosis, Candidate):-
  rdf(Diagnosis, diagnosis:has_candidate, Candidate, ccm).

% A conflict has exactly one diagnosis.
diagnosis_conflict(Diagnosis, Conflict):-
  nonvar(Conflict),
  !,
  diagnosis_conflict_(Diagnosis, Conflict),
  !.
diagnosis_conflict(Diagnosis, Conflict):-
  diagnosis_conflict_(Diagnosis, Conflict).

diagnosis_conflict_(Diagnosis, Conflict):-
  rdf(Diagnosis, diagnosis:has_conflict, Conflict, ccm).

diagnosis_conflicts(Diagnosis, Conflicts):-
  setoff(
    Conflict,
    diagnosis_conflict(Diagnosis, Conflict),
    Conflicts
  ).

%% diagnosis_current_probe(
%%   ?Diagnosis:diagnosis,
%%   ?CurrentProbe:point
%% ) is nondet.
% Pairs of a diagnosis and its current probe point.
%
% @param Diagnosis A diagnosis.
% @param CurrentProbe A point.

diagnosis_current_probe(Diagnosis, CurrentProbe):-
  var(Diagnosis),
  var(CurrentProbe),
  !,
  diagnosis_current_probe_(Diagnosis, CurrentProbe).
% There is a one-to-one mapping between diagnoses and current probe points.
diagnosis_current_probe(Diagnosis, CurrentProbe):-
  diagnosis_current_probe_(Diagnosis, CurrentProbe),
  !.

diagnosis_current_probe_(Diagnosis, CurrentProbe):-
  rdf(Diagnosis, diagnosis:has_current_probe, CurrentProbe, ccm).

diagnosis_dialog_(Diagnosis, Dialog):-
  rdf_assert(Diagnosis, diagnosis:has_dialog, Dialog, ccm).

%% diagnosis_id(?Diagnosis:diagnosis, ?ID:integer) is nondet.
% Paris of a diagnosis and its identifier.
%
% @param Diagnosis A diagnosis.
% @param ID An integer identifier.

diagnosis_id(Diagnosis, ID):-
  var(Diagnosis),
  var(ID),
  !,
  diagnosis_id_(Diagnosis, ID).
% There is a one-to-one mapping between diagnoses and their identifiers.
diagnosis_id(Diagnosis, ID):-
  diagnosis_id_(Diagnosis, ID),
  !.

diagnosis_id_(Diagnosis, ID):-
  rdf_datatype(Diagnosis, diagnosis:has_id, integer, ID, ccm).

diagnosis_probable_point(Diagnosis, Point):-
  learner(Diagnosis, Learner),
  does_consider_point(Learner, Point),
  diagnosis_atms(Diagnosis, ATMS),
  \+(atms_premise(ATMS, Point)).

% A probe point has exactly one diagnosis.
diagnosis_probe(Diagnosis, Probe):-
  nonvar(Probe),
  !,
  diagnosis_probe_(Diagnosis, Probe),
  !.
diagnosis_probe(Diagnosis, Probe):-
  diagnosis_probe_(Diagnosis, Probe).

diagnosis_probe_(Diagnosis, Probe):-
  rdf(Diagnosis, diagnosis:has_probe, Probe, ccm).

diagnosis_to_candidates(Diagnosis, Candidates):-
  setoff(
    Candidate,
    diagnosis_candidate(Diagnosis, Candidate),
    Candidates
  ).

diagnosis_to_expectations(Diagnosis, Expectations):-
  setoff(
    Expectation,
    expectation(Diagnosis, Expectation),
    Expectations
  ).

diagnosis_to_probable_points(Diagnosis, Points):-
  setoff(
    Point,
    diagnosis_probable_point(Diagnosis, Point),
    Points
  ).

expectation(Diagnosis, Expectation):-
  rdf(Diagnosis, diagnosis:has_expectation, Expectation, ccm).

remove_candidate(Diagnosis, Candidate):-
  rdf_retractall(Diagnosis, diagnosis:has_candidate, Candidate, ccm).

store_conflict(Diagnosis, Conflict):-
  rdf_assert(Diagnosis, diagnosis:has_conflict, Conflict, ccm).




% MODES %

add_mode(Mode):-
  assert(mode(Mode)).

remove_mode(Mode):-
  retractall(mode(Mode)).



% OBSERVATIONS %

%% observation(?ATMS:atms, ?Point:point) is nondet.
% Pairs of ATMSs and observation points.
% An observation is a premised node that is a point.
%
% @param ATMS An ATMS.
% @param Point A point.

observation(ATMS, Point):-
  atms_premise(ATMS, Point),
  point(Point).



% RUN DIAGNOSIS & REPAIR %

%% ask_probes(
%%   +Diagnosis:diagnosis,
%%   +Candidates:ord_set(environment),
%%   +Questions:list(point)
%% ) is det.
% Start asking the given probe questions.
%
% @param Diagnosis A diagnosis resource.
% @param Candidates A list of lists of component clouds.
% @param Questions A list of points.

% No more probes; end diagnosis.
ask_probes(Diagnosis, _Candidates, []):-
  registered_method(communicate_culprits_method, CommunicateCulpritsMethod),
  CommunicateCulpritsCall =.. [CommunicateCulpritsMethod, Diagnosis],
  call(CommunicateCulpritsCall),

  registered_method(communicate_end_method, EndMethod),
  CommunicateEndCall =.. [EndMethod, Diagnosis],
  call(CommunicateEndCall),

  absolute_file_name(debug(ccm), CCMDebugDirectory),
  working_directory(_, CCMDebugDirectory),
  run_script(convert_graphs, hide).
% Perform a new probe.
ask_probes(Diagnosis, Candidates, [Question | Questions]):-
  % For debugging purposes we print the candidates.
  (
    debug_mode(true)
  ->
    % Debug the probe points.
    maplist(point_id, [Question | Questions], QuestionIDs),
    atomic_list_concat(QuestionIDs, ',', QuestionsIDs),
    debug(gde, '[PROBE-POINTS]\t~w', [QuestionsIDs])
  ;
    true
  ),

  % Communicate the early culprits, if any.
  intersection_environments(Candidates, EarlyCulpritEnvironment),
  registered_method(
    communicate_early_culprit_method,
    CommunicateEarlyCulpritMethod
  ),
  CommunicateEarlyCulpritCall =..
    [CommunicateEarlyCulpritMethod, Diagnosis, EarlyCulpritEnvironment],
  call(CommunicateEarlyCulpritCall),

  % Ask question.
  once(teacher(Teacher)),
  learner(Diagnosis, Learner),
  ask_question(Teacher, Question, Learner),

  % Register the question as a probe question for the diagnosis use case.
  rdf_retractall(Diagnosis, diagnosis:has_current_probe, _OldQuestion, ccm),
  rdf_assert(Diagnosis, diagnosis:has_current_probe, Question, ccm),

  registered_method(ask_question_method, AskQuestionMethod),
  AskQuestionCall =.. [AskQuestionMethod, Diagnosis],
  call(AskQuestionCall).

continue_diagnosis(Answer):-
  diagnosis(Diagnosis),!,
  continue_diagnosis(Diagnosis, Answer).

%% continue_diagnosis(+Diagnosis:diagnosis, +Answer:uri) is det.
% Continue with an already started diagnostic interaction.
%
% @param Diagnosis A diagnosis.
% @param Answer A resource.

continue_diagnosis(Diagnosis, Answer):-
  % Construe the answered point based on the probed point cloud and the
  % answer given.
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Space, Probe),
  find_or_add_alternative_expression(Expression, Answer, AnswerExpression),
  point_cloud(Probe, ProbePointCloud),
  find_or_add_point_for_point_cloud(
    Space,
    AnswerExpression,
    ProbePointCloud,
    AnswerPoint
  ),

  % Export the ATMS for debugging.
  (
    debug_mode(true)
  ->
    format(atom(Explanation), 'Added premise ~w', [AnswerPoint]),
    diagnosis_atms(Diagnosis, ATMS),
    print_atms(ATMS, Explanation)
  ;
    true
  ),

  % Assert the given answer for the probed point.
  stage(Diagnosis, AnswerPoint),
  stage_to_store(Diagnosis),

  % Diagnose using the newly added ATMS premise.
  continue_diagnosis(Diagnosis, Candidates, ProbePoints),

  % Carry on asking probe point, if any.
  ask_probes(Diagnosis, Candidates, ProbePoints).

%% start_diagnosis(+Diagnosis:diagnosis) is det.
% Start a diagnostic interaction.
% We assume that there is an existing point cloud with alternative point
% and expression (this should be check by the calling method).
% If there is no discrepancy then diagnosis takes maximal time
% without finding any result, so we prevent this from happening.
% Make sure that there is at least one symptom.
%
% @param Diagnosis A diagnosis resource.

start_diagnosis(Diagnosis):-
  % Perform the aggregation.
  aggregate(Diagnosis),

  % Run the diagnosis.
  start_diagnosis(Diagnosis, Candidates, ProbePoints),

  % Export the CCM to RDF and DOT.
  if_then(debug_mode(true), write_ccm_to_rdf),
  if_then(debug_mode(true), write_ccm_to_dot(ccm_exp)),

  % Ask the first probe questions.
  ask_probes(Diagnosis, Candidates, ProbePoints).

%% start_repair(+Diagnosis:diagnosis) is det.
% Existing point cloud with existing point and expression.
% There is no discrepancy, but there is a whole new point cloud.
% This means we can use simulation-side repair to fit the correct
% missing component.

start_repair(Diagnosis):-
  % Communication.
  registered_method(communicate_no_discrepancy_method, Method),
  call(Method),

  missing_component(Diagnosis).



% STAGE & STORE EXPECTATIONS %

%% stage(+Diagnosis:diagnosis, +Point:point) is det.
% The same or an alternative point has already been staged.
% This means that the given point cannot also be staged.
% We decide to give a warning in this case.
% Adding the given point can be done by first unstaging the
% previouis point and then staging again.
%
% @param Diagnosis A diagnosis resource.
% @param Point A point.

stage(Diagnosis, Point):-
  staged_point(Diagnosis, AlternativePoint),
  (
    Point == AlternativePoint
  ->
    true
  ;
    conflicting_points(Point, AlternativePoint)
  ),
  registered_method(
    communicate_impossible_stage_method,
    CommunicateImpossibleStageMethodMethod
  ),
  Call =..
    [
      CommunicateImpossibleStageMethodMethod,
      Diagnosis,
      Point,
      AlternativePoint
    ],
  call(Call).
% The point is newly staged.
stage(Diagnosis, Point):-
  rdf_assert(Diagnosis, diagnosis:has_staged, Point, ccm),
  registered_method(communicate_stage_method, CommunicateStageMethod),
  Call =.. [CommunicateStageMethod, Diagnosis, Point],
  call(Call).

%% stage_to_store(+Diagnosis:diagnosis) is det.
% Store all staged points as expectations.
%
% @param Diagnosis A diagnosis.

stage_to_store(Diagnosis):-
  % Gather all staged points.
  staged_points(Diagnosis, Staged),
  % Store all staged points.
  maplist(store_point(Diagnosis), Staged),
  % Clear all staged expectation points.
  rdf_retractall(Diagnosis, diagnosis:has_staged, _Point, ccm).

%% staged_point(?Diagnosis:diagnosis, ?Staged:point) is nondet.
% Pairs of a diagnosis and one of its staged expectations.
%
% @param Diagnosis A diagnosis.
% @param Staged A point.

% An staged point belongs to exactly one diagnosis.
staged_point(Diagnosis, Staged):-
  nonvar(Staged),
  !,
  staged_point_(Diagnosis, Staged),
  !.
staged_point(Diagnosis, Staged):-
  staged_point_(Diagnosis, Staged).

staged_point_(Diagnosis, Staged):-
  rdf(Diagnosis, diagnosis:has_staged, Staged, ccm).

%% staged_points(+Diagnosis:diagnosis, -Staged:ord_set(point)) is det.
% Returns all expectation points that have been staged for the
% given diagnosis.
%
% @param Diagnosis A diagnosis.
% @param ExpectationPoints An ordered set of expectation points.

staged_points(Diagnosis, ExpectationPoints):-
  setoff(
    ExpectationPoint,
    staged_point(Diagnosis, ExpectationPoint),
    ExpectationPoints
  ).

%% store_point(+Diagnosis:diagnosis, +Point:point) is det.
% Stores the given expectation point for the given diagnosis.
%
% @param Diagnosis A diagnosis.
% @param Point A point.

store_point(Diagnosis, Point):-
  learner(Diagnosis, Learner),
  add_expectation(Diagnosis, Point),
  add_belief(Learner, Point),
  consider_point(Learner, Point),
  diagnosis_atms(Diagnosis, ATMS),
  add_premise(ATMS, Point).
