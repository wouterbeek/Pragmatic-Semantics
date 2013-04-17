:- module(
  causal_explanation,
  [
% WHY REQUEST HANDLING
    causal_explanation_export/1, % +Request:ord_set(probe_point)
    why_export_follow_up/1, % +FollowUpRequest:atom
    
% EXPLANATIONS
    explanation/1, % ?Explanation:explanation
    explanation_component/2, % ?Explanation:explanation
                             % ?Component:component
    explanation_conclusion/2, % ?Explanation:explanation
                              % ?Conclusion:proposition
    explanation_derivation_rule/2, % ?Explanation:explanation
                                   % ?DerivationRule:proposition
    explanation_point/2, % ?Explanation:explanation
                         % ?Point:point
    explanation_points/2, % +Explanation:explanation
                          % -Points:ord_set(point)
    explanation_premise/2, % ?Explanation:explanation
                          % ?Premise:proposition
    explanation_relevance/2, % ?Explanation:explanation
                             % ?Relevance:float
    explanations/1, % ?Explanations:ord_set(explanation)
    find_explanation_nonta/3, % +Point:point
                        % +Component:component
                        % -Explanation:explanation
    find_or_add_explanation/3, % +Point:point
                               % +Component:component
                               % -Explanation:explanation

% EXPLANATION OFFSETS
    explanation_offset/1, % ?ExplanationOffset:explanation_offset
    explanation_offsets/1, % -ExplanationOffsets:ord_set(explanation_offset)
    explanation_offset_explanation/2, % ?ExplanationOffset:explanation_offset
                                      % ?Explanation:explanation
    explanation_offset_story/2, % ?ExplanationOffset:explanation_offset
                                % ?Story:atom
    explanation_offset_point/2, % ?ExplanationOffset:explanation_offset
                                % ?Point:point
    find_explanation_offset/3, % +Request:point
                               % +Filter:ord_set(component)
                               % -ExplanationOffset:explanation_offset
    find_or_add_explanation_offset/3, % +Request:point
                                      % +Filter:ord_set(component)
                                      % -ExplanationOffset:explanation_offset

% PROPOSITIONS
    find_or_add_proposition/2, % +Point:point
                               % -Proposition:proposition
    find_proposition/2, % +Point:point
                        % -Proposition:proposition
    proposition/1, % ?Proposition:proposition
    propositions/1, % ?Propositions:ord_set(proposition)

% REQUESTS
    find_or_add_request/3, % +Request:ord_set(point)
                           % +Filter:ord_set(component)
                           % -Request:request
    find_request/3, % +Request:ord_set(point)
                    % +Filter:ord_set(component)
                    % -Request:request
    request/1, % ?Request:request
    requests/1, % ?Requests:ord_set(request)
    request_explanation/2, % ?Request:request
                           % ?Explanation:explanation
    request_explanation_offset/2, % ?Request:request
                                  % ?ExplanationOffset:explanation_offset
    request_point/2, % ?Request:request
                     % ?Point:point
    request_to_explanation_offsets/2, % +Request:request
                                   % -ExplanationOffsets:ord_set(explanation_offset)
    request_to_explanations/2, % +Request:request
                            % -Explanations:ord_set(explanation)
    request_to_points/2, % +Request:request
                      % -Points:ord_set(point)
    request_to_story/2 % +Request:request
                       % -Story:atom
  ]
).

/** <module> Causal Explanation

This module contains methods for providing causal explanations based on QR
simulation results.

First we have some methods that work with/on explanation objects.
Oftentimes, we want to explain multiple things at once, we then wrap several
explanation objects into one explanation collection object.

---+ Aggrgation criterion for causal explanation

Components A and B can be aggregated into a new component C if
$\langle A, B \rangle \in R^C$ and $In(B) \subseteq In(A) \cup \{ A \}$
or $Out(A) \subseteq Out(B) \cup \{ B \}$ or $\langle B, A \rangle \in R^C$.

This criterion excludes cases such as the one depicted in the following
figure:

[[causal_explanation_criterion_violation.jpg]]

@author Wouter Beek
@tbd Fundamental rewrite for ILE.
@version Aug2011-Mrt2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_prob)).
:- use_module(ccm(ccm_verb)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_build)).
:- use_module(library(semweb/rdf_db)).
:- use_module(qr(causal_explanation_export)).
:- use_module(qr(qr_api)).
:- use_module(qr(qr_kb)).



% SETTINGS %

why_excluded_component_definition(retrieval_component).
why_excluded_component_definition(scenario_value_component).



% CAUSAL EXPLANATION REQUEST HANDLING %

%% causal_explanation_export(+PoseRequests:list(compound)) is det.
% This is where the requests for causal explanations come in.
%
% @param PoseRequests A list of causal explanation requests. These are of
%        the form =|request(FromState, ToState, InternalQuantityName)|=.

causal_explanation_export(RequestPoints):-
  causal_explanation_export(RequestPoints, []).

%% causal_explanation_export(
%%   +PoseRequests:list(compound),
%%   +ComponentFilter:list(component)
%% ) is det.
% This is where the requests for causal explanations come in.
%
% @param PoseRequests A list of causal explanation requests. These are of
%        the form =|request(FromState, ToState, InternalQuantityName)|=.
% @param ComponentFilter A list of components that are excluded from the
%        generated explanations.

causal_explanation_export(RequestPoints, ComponentFilter):-
  % First write to a memory file.
  new_memory_file(MemoryFile),
  open_memory_file(MemoryFile, write, OutputStream, [encoding(utf8)]),
  % Make sure the CCM is loaded.
  find_or_add_request(RequestPoints, ComponentFilter, Request),
  % Make sure the aliases are included for the followup point and component
  % URIs as well.
  rdf_current_namespaces(why_vc, Aliases),
  rdf_save2(stream(OutputStream), [graph(why_vc), namespaces(Aliases)]),
  % Done writing.
  rdf_retractall(_Subject, _Predicate, _Object, why_vc),
  close(OutputStream),
  
  % Now read from the momory file.
  open_memory_file(MemoryFile, read, InputStream, [encoding(utf8)]),
  read_stream_to_codes(InputStream, Codes),
  close(InputStream),
  free_memory_file(MemoryFile),
  
  atom_codes(Message, Codes),
  debug(causal_explanation, '~w', [Message]),
  %%%%vc(Message),
  if_then(debug_mode(true), print_request(Request)).

%% why_export_follow_up(+SocketCall:atom) is det.
% Poses a causal explanation follow-up request.
%
% @param SocketCall Atomic term of the following form:
%        =|<??? state="State"/>|=.

why_export_follow_up(RequestPoint):-
  causal_explanation_export([RequestPoint]).

why_export_follow_up(RequestPoints, Components):-
  causal_explanation_export(RequestPoints, Components).



% ARGUMENTS %

%% assert_argument(+Explanation, +Argument:component) is det.
% Asserts an argument for the given explanation.
%
% @param Explanation An explanation.
% @param Argument A component.

assert_argument(Explanation, Argument):-
  % Input points / premise expressions.
  component_to_input_points(Argument, InputPoints),
  forall(
    member(InputPoint, InputPoints),
    (
      add_proposition(InputPoint, Proposition),
      rdf_assert(Explanation, explanation:has_premise, Proposition, ccm),
      rdf_assert(Explanation, explanation:has_premise, Proposition, why_vc)
    )
  ),

  % Support points / derivation rule expressions.
  component_to_support_points(Argument, SupportPoints),
  forall(
    member(SupportPoint, SupportPoints),
    (
      add_proposition(SupportPoint, Proposition),
      rdf_assert(Explanation, explanation:has_derivation_rule, Proposition, ccm),
      rdf_assert(Explanation, explanation:has_derivation_rule, Proposition, why_vc)
    )
  ),

  % Output points / conclusion expression.
  component_to_output_points(Argument, OutputPoints),
  forall(
    member(OutputPoint, OutputPoints),
    (
      add_proposition(OutputPoint, Proposition),
      rdf_assert(Explanation, explanation:has_conclusion, Proposition, ccm),
      rdf_assert(Explanation, explanation:has_conclusion, Proposition, why_vc)
    )
  ).



% EXPLANATIONS %

%% add_explanation(
%%   +Point:point,
%%   +Component:component,
%%   -Explanation:explanation
%% ) is det.
% Creates the singular explanation for the given point, using the
% given component.
%
% @param Point A point from which an explanation is created.
% @param Component A component that explains the given point.
% @param Explanation A singular explanation.
% @tbd If the component has a decomposition into subcomponents, then the
%      explanation also has a decomposition into subexplanations.

add_explanation(Point, Component, Explanation):-
  % Create a new singular explanation.
  flag(singular_explanation_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  format(atom(Name), 'se_~w', [AtomicID]),
  rdf_global_id(explanation:Name, Explanation),
  rdf_assert(Explanation, rdf:type, explanation:singular, ccm),
  rdf_assert(Explanation, rdf:type, explanation:singular, why_vc),
  rdf_assert_datatype(Explanation, explanation:has_id, integer, ID, ccm),
  rdf_assert_datatype(Explanation, explanation:has_id, integer, ID, why_vc),

  % Assert what this explanation is about, allowing us to lift it out in the
  % future.
  rdf_assert(Explanation, explanation:about_component, Component, ccm),
  rdf_assert(Explanation, explanation:about_component, Component, why_vc),
  rdf_assert(Explanation, explanation:about_point, Point, ccm),
  rdf_assert(Explanation, explanation:about_point, Point, why_vc),

  % Assert the argument.
  assert_argument(Explanation, Component),

  % Generate and assert the story label.
  pragmatics(Explanation, Story),
  rdfs_assert_label(Explanation, Story, ccm),
  rdfs_assert_label(Explanation, Story, why_vc),

  % Assert explanation relevance.
  component_cloud_component(ComponentCloud, Component),
  component_cloud_to_probability(ComponentCloud, Relevance),
  rdf_assert_datatype(Explanation, explanation:has_relevance, float, Relevance, ccm),
  rdf_assert_datatype(Explanation, explanation:has_relevance, float, Relevance, why_vc).

explanation(Explanation):-
  rdfs_individual_of(Explanation, explanation:singular).

%% explanation_component(
%%   ?Explanation:explanation,
%%   ?Component:component
%% ) is nondet.

explanation_component(Explanation, Component):-
  rdf(Explanation, explanation:about_component, Component, ccm).

explanation_conclusion(Explanation, Conclusion):-
  rdf(Explanation, explanation:has_conclusion, Conclusion, ccm).

explanation_derivation_rule(Explanation, DerivationRule):-
  rdf(Explanation, explanation:has_derivation_rule, DerivationRule, ccm).

%% explanation_point(?Explanation:explanation, ?Point:point) is nondet.

explanation_point(Explanation, Point):-
  rdf(Explanation, explanation:about_point, Point, ccm).

%% explanation_points(
%%   +Explanation:explanation,
%%   -Points:ord_set(point)
%% ) is det.

explanation_points(Explanation, Points):-
  setoff(
    Point,
    explanation_point(Explanation, Point),
    Points
  ).

explanation_premise(Explanation, Premise):-
  rdf(Explanation, explanation:has_premise, Premise, ccm).

%% explanation_relevance(
%%   ?Explanation:explanation,
%%   ?Relevance:float
%% ) is nondet.

explanation_relevance(Explanation, Relevance):-
  rdf_datatype(Explanation, explanation:has_relevance, float, Relevance, ccm).

explanation_relevance_label(Explanation, relevance(Relevance, Story)):-
  explanation_story(Explanation, Story),!,
  explanation_relevance(Explanation, Relevance),!.

explanation_story(Explanation, Story):-
  rdfs_label(Explanation, Story).

explanations(Explanations):-
  setoff(
    Explanation,
    explanation(Explanation),
    Explanations
  ).

%% find_explanation_nonta(
%%   ?Point:point,
%%   ?Component:component,
%%   ?Explanation:explanation
%% ) is nondet.

find_explanation_nonta(Point, Component, Explanation):-
  explanation(Explanation),
  explanation_component(Explanation, Component),
  explanation_point(Explanation, Point).

find_or_add_explanation(Point, Component, Explanation):-
  find_explanation_nonta(Point, Component, Explanation),
  !.
find_or_add_explanation(Point, Component, Explanation):-
  add_explanation(Point, Component, Explanation).



% EXPLANATION OFFSETS %

add_explanation_offset(RequestPoint, ComponentFilter, ExplanationOffset):-
  % Create a new explanation offset.
  flag(explanation_offset_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  format(atom(Name), 'eo_~w', [AtomicID]),
  rdf_global_id(explanation:Name, ExplanationOffset),
  rdf_assert(ExplanationOffset, rdf:type, explanation:explanation_offset, ccm),
  rdf_assert(ExplanationOffset, rdf:type, explanation:explanation_offset, why_vc),
  rdf_assert_datatype(ExplanationOffset, explanation:has_id, integer, ID, ccm),
  rdf_assert_datatype(ExplanationOffset, explanation:has_id, integer, ID, why_vc),

  % Assert what the explanation offset is about.
  rdf_assert(ExplanationOffset, explanation:about_point, RequestPoint, ccm),
  rdf_assert(ExplanationOffset, explanation:about_point, RequestPoint, why_vc),

  % Assert a component explanation for every component that explains the
  % given request point.
  findall(
    Component,
    (
      point_input_component(RequestPoint, Component),
      % Exclude components that are in the filter.
      \+(member(Component, ComponentFilter)),
      % Exclude subsumed components.
      \+(subsumed_component(Component))
    ),
    Components
  ),
  findall(
    Explanation,
    (
      member(Component, Components),
      (
        \+((
          why_excluded_component_definition(ComponentDefinitionPredicate),
          Call =.. [ComponentDefinitionPredicate, Component],
          call(Call)
        )),
        find_or_add_explanation(RequestPoint, Component, Explanation),
        rdf_assert(
          ExplanationOffset,
          explanation:has_explanation,
          Explanation,
          ccm
        ),
        rdf_assert(
          ExplanationOffset,
          explanation:has_explanation,
          Explanation,
          why_vc
        )
      )
    ),
    Explanations
  ),

  % Assert the explanation label.
  maplist(explanation_relevance_label, Explanations, Stories),
  order_to_relevance(Stories, [TopStory | _DropStories]),
  rdfs_assert_label(ExplanationOffset, TopStory, ccm),
  rdfs_assert_label(ExplanationOffset, TopStory, why_vc).

explanation_offset(ExplanationOffset):-
  rdfs_individual_of(ExplanationOffset, explanation:explanation_offset).

%% explanation_offset_explanation(
%%    +ExplanationOffset:explanation,
%%    -Explanation:explanation
%% ) is nondet.
% Returns a singular explanation that is related to =|ExplanationOffset|=.
%
% @param ExplanationOffset An explanation.
% @param Explanation An explanation.

explanation_offset_explanation(ExplanationOffset, Explanation):-
  rdf(ExplanationOffset, explanation:has_explanation, Explanation, ccm).

%% explanation_offset_story(
%%   ?ExplanationOffset:explanation_offset,
%%   ?Story:atom
%% ) is det.
% Returns the label of the given explanation offset.
%
% @param ExplanationOffset An explanation offset.
% @param Story An atomic label.

explanation_offset_story(ExplanationOffset, Story):-
  rdfs_label(ExplanationOffset, Story).

%% explanation_offset_point(
%%   ?ExplanationOffset:explanation_offset,
%%   ?Point:point
%% ) is nondet.
% Explanation offsets that are about a specific point.

explanation_offset_point(ExplanationOffset, Point):-
  rdf(ExplanationOffset, explanation:about_point, Point, ccm).

explanation_offsets(ExplanationOffsets):-
  setoff(
    ExplanationOffset,
    explanation_offset(ExplanationOffset),
    ExplanationOffsets
  ).

order_to_relevance(RelevanceLabels, Labels):-
  predsort_with_duplicates(
    greater_relevance,
    RelevanceLabels,
    Ordered
  ),
  extract_labels(Ordered, Labels).

greater_relevance(
  Order,
  relevance(Relevance1, _Label1),
  relevance(Relevance2, _Label2)
):-
  compare(Order, Relevance2, Relevance1).

extract_labels([], []):-
  !.
extract_labels(
  [relevance(_Relevance, Label) | RelevanceLabels],
  [Label | Labels]
):-
  extract_labels(RelevanceLabels, Labels).

find_explanation_offset(RequestPoint, _ComponentFilter, ExplanationOffset):-
  rdfs_individual_of(ExplanationOffset, explanation:singular),
  explanation_offset_point(ExplanationOffset, RequestPoint).

find_or_add_explanation_offset(RequestPoint, ComponentFilter, ExplanationOffset):-
  find_explanation_offset(RequestPoint, ComponentFilter, ExplanationOffset),
  !.
find_or_add_explanation_offset(RequestPoint, ComponentFilter, ExplanationOffset):-
  add_explanation_offset(RequestPoint, ComponentFilter, ExplanationOffset).



% PROPOSITIONS %

%% add_proposition(+Point:point, -Proposition:proposition) is det.
% Returns the proposition for the given point.
%
% @param Point A point.
% @param Proposition A proposition.

add_proposition(Point, Proposition):-
  % Create the new proposition.
  flag(proposition_id, ID, ID + 1),
  format(atom(PropositionName), 'proposition_~w', [ID]),
  rdf_global_id(explanation:PropositionName, Proposition),
  rdf_assert(Proposition, rdf:type, explanation:proposition, ccm),
  rdf_assert_datatype(Proposition, explanation:has_id, integer, ID, ccm),
  rdf_assert_datatype(Proposition, explanation:has_id, integer, ID, why_vc),

  % What this proposition is about, used for follow-up questions by the VC.
  rdf_assert(Proposition, explanation:about_point, Point, ccm),
  rdf_assert(Proposition, explanation:has_followup, Point, why_vc),

  % Assert the natural language label.
  point_to_label(Point, PointLanguageLabel),
  rdfs_assert_label(Proposition, PointLanguageLabel, ccm),
  rdfs_assert_label(Proposition, PointLanguageLabel, why_vc),

  % Assert the expression that the proposition expresses. Propositions are
  % not the same as expressions, since they may have properties which
  % depend on the argument of which they are part.
  point(Expression, Point),

  % From argument.
  expression_from_argument(Expression, FromArgument),!,
  rdf_assert(Proposition, explanation:has_from_argument, FromArgument, ccm),
  rdf_assert(Proposition, explanation:has_from_argument, FromArgument, why_vc),
  maplist(add_proposition_helper(FromArgument), [has_id, label, type]),

  % Expression definition.
  expression_definition_expression(ExpressionDefinition, Expression),!,
  rdf_assert(Proposition, explanation:has_relation, ExpressionDefinition, ccm),
  rdf_assert(Proposition, explanation:has_relation, ExpressionDefinition, why_vc),

  % To argument.
  expression_to_argument(Expression, ToArgument),
  maplist(add_proposition_helper(ToArgument), [has_id, label, type]),
  rdf_assert(Proposition, explanation:has_to_argument, ToArgument, ccm),
  rdf_assert(Proposition, explanation:has_to_argument, ToArgument, why_vc).

add_proposition_helper(Resource, PredicateName):-
  rdf(Resource, Predicate, Object, ccm),
  rdf_global_id(_Namespace:PredicateName, Predicate),
  !,
  rdf_assert(Resource, Predicate, Object, ccm),
  rdf_assert(Resource, Predicate, Object, why_vc).

find_or_add_proposition(Point, Proposition):-
  find_proposition(Point, Proposition),
  !.
find_or_add_proposition(Point, Proposition):-
  add_proposition(Point, Proposition).

%% find_proposition(?Point:point, ?Proposition:proposition) is nondet.
% Find propositions by points.
%
% @param Point A point.
% @param Proposition A proposition.

find_proposition(Point, Proposition):-
  rdf(Proposition, explanation:about_point, Point, ccm).

proposition(Proposition):-
  rdfs_individual_of(Proposition, explanation:proposition).

propositions(Propositions):-
  setoff(
    Proposition,
    proposition(Proposition),
    Propositions
  ).



% REQUESTS %

add_request(RequestPoints, ComponentFilter, Request):-
  % Create a request.
  flag(request_id, ID, ID + 1),
  format(atom(Name), 'r_~w', [ID]),
  rdf_global_id(explanation:Name, Request),
  rdf_assert(Request, rdf:type, explanation:request, ccm),
  rdf_assert(Request, rdf:type, explanation:request, why_vc),
  rdf_assert_datatype(Request, explanation:has_id, integer, ID, ccm),
  rdf_assert_datatype(Request, explanation:has_id, integer, ID, why_vc),

  % For each point that is part of the request, add an explanation offset.
  findall(
    ExplanationOffset,
    (
      member(RequestPoint, RequestPoints),
      find_or_add_explanation_offset(
        RequestPoint,
        ComponentFilter,
        ExplanationOffset
      ),
      rdf_assert(
        Request,
        explanation:has_explanation_offset,
        ExplanationOffset,
        ccm
      ),
      rdf_assert(
        Request,
        explanation:has_explanation_offset,
        ExplanationOffset,
        why_vc
      )
    ),
    ExplanationOffsets
  ),

  % Create the label.
  maplist(explanation_offset_story, ExplanationOffsets, Stories),!,
  atomic_list_concat(Stories, ' ', Story),
  rdfs_assert_label(Request, Story, ccm),
  rdfs_assert_label(Request, Story, why_vc).

find_or_add_request(RequestPoints, ComponentFilter, Request):-
  find_request(RequestPoints, ComponentFilter, Request),
  !.
find_or_add_request(RequestPoints, ComponentFilter, Request):-
  add_request(RequestPoints, ComponentFilter, Request).

find_request(RequestPoints, _ComponentFilter, Request):-
  rdfs_individual_of(Request, explanation:request),
  % Request points are already sorted (ordered sets), so we need not
  % check for this.
  request_to_points(Request, RequestPoints).

request(Request):-
  rdfs_individual_of(Request, explanation:request).

request_explanation(Request, Explanation):-
  request_explanation_offset(Request, ExplanationOffset),
  explanation_offset_explanation(ExplanationOffset, Explanation).

%% request_explanation_offset(
%%    ?Request:explanation,
%%    ?ExplanationOffset:explanation
%% ) is nondet.
% Request and explanation offset.
%
% @param Request The URI of a request.
% @param ExplanationOffset The URI of a collection of explanations.

request_explanation_offset(Request, ExplanationOffset):-
  rdf(Request, explanation:has_explanation_offset, ExplanationOffset, ccm).

request_point(Request, Point):-
  rdf(Request, expression:has_explanation_offset, ExplanationOffset, ccm),
  explanation_offset_point(ExplanationOffset, Point).

%% request_to_explanation_offsets(
%%    +Request:explanation,
%%    -ExplanationOffsets:ord_set(explanation)
%% ) is det.
% Returns the explanation collections associated with the given request.
% Explanation collections play the role of conjunction for requests.
%
% @param Request The URI of a request.
% @param ExplanationOffsets A list of URIs of collections of explanations.

request_to_explanation_offsets(Request, ExplanationOffsets):-
  setoff(
    ExplanationOffset,
    request_explanation_offset(Request, ExplanationOffset),
    ExplanationOffsets
  ).

%% request_to_explanations(
%%   +Request:explanation,
%%   -Explanations:list(explanation)
%% ) is det.
% Returns the explanations for the given Request.
%
% @param Request The URI of a request.
% @param Explanations A list of URI's of explanations.

request_to_explanations(Request, Explanations):-
  setoff(
    Explanation,
    request_explanation(Request, Explanation),
    Explanations
  ).

request_to_points(Request, RequestPoints):-
  setoff(
    RequestPoint,
    request_point(Request, RequestPoint),
    RequestPoints
  ).

%% request_to_story(+Request:request, -Story:atom) is det.
% Returns the label of the given request.
%
% @param Request A request.
% @param Story An atomic label.

request_to_story(Request, Story):-
  request_to_explanations(Request, Explanations),
  atomic_list_concat(Explanations, '\\n', Story).

requests(Requests):-
  setoff(Request, request(Request), Requests).

space_transition_to_important_points(SpaceTransition, ImportantPoints):-
  setoff(
    ImportantPoint,
    (
      space_transition_to_component(SpaceTransition, Component),
      % Not all transitions are important; only termination
      % and scenario value components.
      (
        scenario_value_component(Component)
      ;
        termination_component(Component)
      ),
      component_output_point(Component, ImportantPoint)
    ),
    ImportantPoints
  ).
