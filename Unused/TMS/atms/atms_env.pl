:- module(
  atms_env,
  [
    compare_environments/3, % +Environment1:environment
                            % +Environment2:environment
                            % -Comparison:atom
    delete_superenvironments/4, % +Environments:set(environment)
                                % +CompareEnvironments:set(environment)
                                % -ResultEnvironments:set(environment)
                                % -RestEnvironments:set(environment)
    environment_hierarchical_unpack/4, % +Diagnosis:diagnosis
                                       % +Environment:environment
                                       % -UnpackedEnvironment:environment
                                       % -RemovedEnvironment:environment
    extend_environment/3,% +Assumption:node,
                         % +Environment:environment,
                         % -NewEnvironment:environment
    intersection_environments/2, % +Environments:ord_set(environment)
                                 % -IntersectionEnvironment:environment
    is_not_minimal_environment/2, % +Environment:environment
                                  % +CompareEnvironments:list(environment)
    member_environment/2, % +Assumption:node
                          % +Environment:environment
    minimize_environments/2, % +Environments:list(environment)
                             % -MinimalEnvironments:list(environment)
    next_environment_small_to_big/3, % +Diagnosis:diagnosis
                                     % +Environment:environment
                                     % -NextEnvironment:environment
    singleton_environment/1, % +Environment:environment
    subenvironment/2, % +SubEnvironment:environment
                      % +SuperEnvironment:environment
    union_environments/3 % +Environment1:environment
                         % +Environment2:environment
                         % -UnionEnvironment:environment
  ]
).

/** <module> ATMS environments

ATMS environment lattice traversal.

Contains all environment methods, used by both the ATMS (environments)
and the GDE (component assumptions).

@author Wouter Beek
@version Dec 2011 - Aug 2012
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_build)).
:- use_module(ccm(ccm_unpack)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(generic(set_theory)).
:- use_module(ile(agent)).



%% compare_environments(
%%   +Environment1:environment,
%%   +Environment2:environment,
%%   -Comparison:atom
%% ) is det.
% Returns the comparison label for the comparison of the two
% given environments.
%
% @param Environment1 An environment.
% @param Environment2 An environment.
% @param Comparison The atomic comparison label. Either 'eq', 's12',
%        or 's21'.

compare_environments(Environment, Environment, eq):-
  !.
compare_environments(Environment1, Environment2, Comparator):-
  environment_cardinality(Environment1, Count1),
  environment_cardinality(Environment2, Count2),
  Count1 < Count2,
  environment_assumptions(Environment1, Assumptions1),
  environment_assumptions(Environment2, Assumptions2),
  compare_environments1(Assumptions1, Assumptions2, Comparator).
compare_environments1(Assumptions, Assumptions, s12):-
  ord_subset(Assumptions, Assumptions),
  !.
compare_environments1(Assumptions, Assumptions, s21):-
  ord_subset(Assumptions, Assumptions).

%% delete_superenvironments(
%%   +Environments:list(environment),
%%   +CompareEnvironments:list(environment),
%%   -ResultEnvironments:list(environment),
%%   -RestEnvironments:list(environment)
%% ) is det.
% Deletes from =Environments= all environments whose assumptions are a
% superset of the assumptions of some environment in =CompareEnvironments=.
%
%   * For every environment =E= in =Environments= do:
%     * If =E= is a superenvironment of some environment in
%       =CompareEnvironments=, then add =E= to =RestEnvironments=.
%     * If =E= is a superenvironment of some environment in
%       =CompareEnvironments=, then =E= is a _|minimal environment|_ and
%       it is added to =ResultEnvironments=.
%
% @param Environments:list(environment) A set of URIs of environments.
% @param CompareSets:list(environment) A set of URIs of environments.
% @param ResultEnvironments A set of URIs of environments.
% @param RestEnvironments A set of URIs of environments.

delete_superenvironments(
  Environments,
  CompareEnvironments,
  ResultEnvironments,
  RestEnvironments
):-
  delete_sets(
    Environments,
    CompareEnvironments,
    subenvironment,
    ResultEnvironments,
    RestEnvironments
  ).

%% environment_hierarchical_unpack(
%%   +Diagnosis:diagnosis,
%%   +SingletonEnvironment:environment,
%%   -UnpackedEnvironment:environment,
%%   -RemovedEnvironment:environment
%% ) is det.
% Returns the unpacked environment.
% Only singleton environments are unpacked.
%
% @param Diagnosis A diagnosis.
% @param Environment An environment.
% @param UnpackedEnvironment An environment.
% @param RemovedEnvironment The environment of nodes that have been
%        removed in the process.

environment_hierarchical_unpack(
  Diagnosis,
  SingletonEnvironment,
  UnpackedEnvironment,
  RemovedEnvironment
):-
  environment_assumptions(SingletonEnvironment, Assumptions),
  maplist(
    component_hierarchical_unpack(Diagnosis),
    Assumptions,
    NewAssumptionss,
    RemovedAssumptionss
  ),
  diagnosis_atms(Diagnosis, ATMS),
  ord_union(NewAssumptionss, NewAssumptions),
  find_or_add_environment(ATMS, NewAssumptions, UnpackedEnvironment),
  ord_union(RemovedAssumptionss, RemovedAssumptions),
  find_or_add_environment(ATMS, RemovedAssumptions, RemovedEnvironment).

%% extend_environment(
%%   +Assumption:node,
%%   +Environment:environment,
%%   -NewEnvironment:environment
%% ) is det.
% Returns the result of updating =Environment= with =Assumption=.
%
% @param Assumption The URI of a node.
% @param Environment The URI of an environment.
% @param NewEnvironment The URI of an environment.

extend_environment(Assumption, Environment, NewEnvironment):-
  environment_assumptions(Environment, Assumptions),
  ord_add_element(Assumptions, Assumption, NewAssumptions),
  node(ATMS, Assumption),
  find_or_add_environment(ATMS, NewAssumptions, NewEnvironment).

intersection_environments(Environments, IntersectionEnvironments):-
  first(Environments, Environment),
  environment(ATMS, Environment),!,
  intersection_environments(ATMS, Environments, IntersectionEnvironments).

%% intersection_environments(
%%   +ATMS:atms,
%%   +Environments:ord_set(environment),
%%   -IntersectionEnvironment:environment
%% ) is det.
% Returns the intersection of the given environments.
%
% @param ATMS An ATMS.
% @param Environments An ordered set of environments.
% @param IntersectionEnvironment An environment.

intersection_environments(_ATMS, [Environment], Environment):-
  !.
intersection_environments(ATMS, Environments, IntersectionEnvironment):-
  maplist(environment_assumptions, Environments, Assumptionss),
  ord_intersection(Assumptionss, IntersectionAssumptions),
  find_or_add_environment(
    ATMS,
    IntersectionAssumptions,
    IntersectionEnvironment
  ).

is_not_minimal_environment(Environment, CompareEnvironments):-
  member(CompareEnvironment, CompareEnvironments),
  subenvironment(CompareEnvironment, Environment).

%% member_environment(+Assumption:node, +Environment:environment) is semidet.
% Succeeds if =Assumption= is an assumption if =Environment=.
%
% @param Assumption The URI of a node.
% @param Environment The URI of an environment.

member_environment(Assumption, Environment):-
  environment_assumptions(Environment, Assumptions),
  member(Assumption, Assumptions).

%% minimize_environments(
%%   +Environments:list(environment),
%%   -MinimalEnvironments:list(environment)
%% ) is det.

minimize_environments(Environments, MinimalEnvironments):-
  predsort_with_duplicates(
    shorter_environments,
    Environments,
    EnvironmentsSortedByLength
  ),
  minimize_environments_(EnvironmentsSortedByLength, MinimalEnvironments).

minimize_environments_([], []).
minimize_environments_([H | T], Solution):-
  is_not_minimal_environment(H, T),
  !,
  minimize_environments_(T, Solution).
minimize_environments_([H | T1], [H | T2]):-
  minimize_environments_(T1, T2).

%% next_environment_small_to_big(
%%   +Diagnosis:diagnosis,
%%   +Environment:environment,
%%   -NextEnvironment:environment
%% ) is det.
% Returns the next environment, based on the given environment.
% Generating the next environment is done by generating the next
% assumptions set and translating that to an existing or a new
% environment.
%
% @param Diagnosis A diagnosis.
% @param OldEnvironment An environment.
% @param NextEnvironment An environment.

next_environment_small_to_big(Diagnosis, Environment, NextEnvironment):-
  environment_assumptions(Environment, Assumptions),
  next_environment(Diagnosis, Assumptions, NewAssumptions),
  diagnosis_atms(Diagnosis, ATMS),
  find_or_add_environment(ATMS, NewAssumptions, NextEnvironment_),
  (
    % This is not a new environment, since it is a subenvironment of
    % a nogood environment.
    nogood(ATMS, NogoodEnvironment),
    subenvironment(NextEnvironment_, NogoodEnvironment)
  ->
    next_environment_small_to_big(
      Diagnosis,
      NextEnvironment_,
      NextEnvironment
    )
  ;
    % This is a new environment for which we will calculate behavior / make
    % predictions.
    flag(environment_flag, ID, ID + 1),
    NextEnvironment = NextEnvironment_
  ).

%% next_environment(
%%   +Diagnosis:diagnosis,
%%   +Assumptions:ord_set(component),
%%   -NewAssumptions:ord_set(component)
%% ) is det.
% Returns the next relevant environment or fails if there is no such
% environment.
% The environment is relevant if it is a subset of the relevant
% components. Relevance is a property of the diagnosis.
% Environments are ordered sets of assumptions. We use ordered sets
% for computational efficiency.
%
% @param Diagnosis A diagnosis.
% @param Assumptions An ordered set of components.
% @param NewAssumptions An ordered set of components.

% The old environment is all scoped components. Then there is no next
% environment, so fail.

next_environment(Diagnosis, Assumptions, NewAssumptions):-
  % Since all three next_environment/2 predicates use this, we fetch the
  % scoped components.
  diagnosis_component_clouds(Diagnosis, ConsideredComponents),
  diagnosis_atms(Diagnosis, ATMS),
  next_environment_(ATMS, Assumptions, ConsideredComponents, NewAssumptions).

%% next_environment_(
%%   +ATMS:atms,
%%   +Assumptions:ord_set(component),
%%   +ConsideredComponents:ord_set(component),
%%   -NewAssumptions:ord_set(component)
%% ) is det.
% This method recognizes three cases:
%   1. The old environment is the last environment.
%   2. The old environment is the first environment.
%   3. The old environment is some environment in between
%      the first and the last environment.
%
% @param ATMS An ATMS.
% @param Assumptions An ordered set of components.
% @param ConsideredComponents An ordered set of components.
% @param NewAssumptions An ordered set of components.

% FIRST: The old environment is the empty environment. We begin with the
% environment that is the singleton set of the first relevant component.
next_environment_(
  _ATMS,
  [],
  [FirstComponent | _ConsideredComponents],
  [FirstComponent]
):-
  !.
% LAST: The last environment is the set of considered components.
next_environment_(
  _ATMS,
  ConsideredComponents,
  ConsideredComponents,
  _NewAssumptions
):-
  !,
  fail.
% OTHER: The old environment is neither the first nor the last one.
% This is the most common case.
next_environment_(ATMS, Assumptions, ConsideredComponents, NewAssumptions):-
  reverse(Assumptions, RevertedAssumptions),
  length(ConsideredComponents, ConsideredComponentsLength),
  length(Assumptions, AssumptionsLength),
  next_environment(
    RevertedAssumptions,
    Assumptions,
    AssumptionsLength,
    ConsideredComponents,
    ConsideredComponentsLength,
    NewAssumptions1
  ),
  (
    atms_to_nogoods(ATMS, Conflicts),
    forall(
      member(Conflict, Conflicts),
      \+(ord_subset(Conflict, NewAssumptions1))
    ),
    NewAssumptions = NewAssumptions1
  ;
    next_environment_(
      ATMS,
      NewAssumptions1,
      ConsideredComponents,
      NewAssumptions
    )
  ),
  !.

%% next_environment(
%%   +Environment:ord_set(component),
%%   +Environment:ord_set(component),
%%   +EnvironmentLength:integer,
%%   +ConsideredComponents:ord_set(component),
%%   +ConsideredComponentsLength:integer,
%%   -NextEnvironment:ord_set(component)
%% ) is det.
% ...

% FIRST: The old environment is the empty environment. We begin with the
% environment that is the singleton set of the first relevant component.
next_environment(
  [],
  _Environment,
  EnvironmentLength,
  ConsideredComponents,
  _ConsideredComponentsLength,
  NextEnvironment
):-
  NextEnvironmentLength is EnvironmentLength + 1,
  length(NextEnvironment, NextEnvironmentLength),
  append(NextEnvironment, _ComponentsRest, ConsideredComponents),
  !.
next_environment(
  [Component | Components],
  Environment,
  EnvironmentLength,
  ConsideredComponents,
  ConsideredComponentsLength,
  NextEnvironment
):-
  % Retrieve the inverted index of =Component= in =Environment=.
  % The index starts at 0.
  nth1(EnvironmentIndex, Environment, Component),
  EnvironmentInvertedIndex is EnvironmentLength - EnvironmentIndex,

  % Retrieve the inverted index of =Component= in =ConsideredComponents=.
  % The index starts at 0.
  nth1(ConsideredComponentIndex, ConsideredComponents, Component),
  ConsideredComponentInvertedIndex is
    ConsideredComponentsLength - ConsideredComponentIndex,

  EnvironmentInvertedIndex =:= ConsideredComponentInvertedIndex,
  !,
  next_environment(
    Components,
    Environment,
    EnvironmentLength,
    ConsideredComponents,
    ConsideredComponentsLength,
    NextEnvironment
  ).
next_environment(
  [Component | _Components],
  Environment,
  _EnvironmentLength,
  ConsideredComponents,
  _ConsideredComponentsLength,
  NextEnvironment
):-
  % The index of the component in the environment.
  nth0(EnvironmentIndex, Environment, Component),

  % The index of the component in the considered components, off by one.
  % The new index of the new component in the considered components.
  % This new index has never been used before.
  % By the use of nth1 in contrast to nth0.
  nth1(ConsideredComponentsIndex, ConsideredComponents, Component),

  % We give the EnvironmentIndex, because the components that occur in
  % the environment before this index should be preserved in the new
  % environment.
  next_environment_replace(
    Environment,
    EnvironmentIndex,
    ConsideredComponents,
    ConsideredComponentsIndex,
    NextEnvironment
  ).

%% next_environment_replace(
%%   +Environment:ord_set(component),
%%   +IndexOfTobereplacedElementInEnvironment:integer,
%%   +ScopedComponents:ord_set(component),
%%   +IndexOfReplacingElementInComponents:integer,
%%   -NextEnvironment:ord_set(component)
%% ) is det.
% ...

next_environment_replace(
  [_OldComponent | OldComponents],
  0,
  ConsideredComponents,
  ConsideredComponentsIndex,
  [NewComponent | NewComponents]
):-
  % The new component is drawn from the considered components.
  % This K has not been visited before, see the use of nth1 in
  % =|next_environment/6|=.
  nth0(ConsideredComponentsIndex, ConsideredComponents, NewComponent),

  % Create the first tail for the new component.
  next_environment_replace_tail(
    NewComponent,
    OldComponents,
    ConsideredComponents,
    NewComponents
  ),
  !.
% Preserve old components, i.e. those occurring before index I in the
% old environment.
next_environment_replace(
  [OldComponent | OldComponents],
  EnvironmentIndex,
  ConsideredComponents,
  ConsideredComponentsIndex,
  [OldComponent | NewComponents]
):-
  NewEnvironmentIndex is EnvironmentIndex - 1,
  next_environment_replace(
    OldComponents,
    NewEnvironmentIndex,
    ConsideredComponents,
    ConsideredComponentsIndex,
    NewComponents
  ).

%% next_environment_replace_tail(
%%   +NewComponent:component,
%%   +OldComponents:ord_set(component),
%%   +ConsideredComponents:ord_set(component),
%%   -NewComponents:ord_set(component)
%% ) is det
% The first =OldComponentsLength= components from
% =ConsideredComponents= are NewComponents.

next_environment_replace_tail(
  NewComponent,
  OldComponents,
  ConsideredComponents,
  NewComponents
):-
  % The new component cannot be followed by any lower components.
  % Therefore, we can focus on the considered components that occur
  % after this new component.
  element_cut(
    ConsideredComponents,
    NewComponent,
    _BeforeConsideredComponents,
    AfterConsideredComponents
  ),
  % We do not change the length of the environment.
  length(OldComponents, OldComponentsLength),
  length(NewComponents, OldComponentsLength),
  % The next environment consists of the N considered components
  % that directly follow the new component.
  % This is true because the considered components are an ordered set.
  append(NewComponents, _Rest, AfterConsideredComponents).

shorter_environments(Order, Environment1, Environment2):-
  environment_assumptions(Environment1, Assumptions1),
  environment_assumptions(Environment2, Assumptions2),
  shorter(Order, Assumptions1, Assumptions2).

%% singleton_environment(Environment:environment) is semidet.
% Succeeds if the given environment is a singleton environment.
% A singleton environment is an environment with exactly one assumption.
%
% @param Environment An environment.

singleton_environment(Environment):-
  environment_cardinality(Environment, 1).

%% subenvironment(
%%   +Environment1:environment,
%%   -Environment2:environment
%% ) is semidet.
% Succeeds if =Environment1= is a _subenvironment_ of =Environment2=.
%
% An environment =E= is a *subenvironment* of an environment =F= iff
% the assumptions of =E= are a subset of the assumptions of =F=.
%
% This method is more efficient than Prolog's =subset/2= method, since we
% store the number of assumptions for each environment. This means that we
% have two cases:
%   1. The environments are the same. The assumption counts do not allow
%      to infer this, but the identity of arguments (i.e. URIs) does.
%   2. The environments are not the same, but the former's assumptiosn are
%      a subset of the latter's assumptions. This case can be recognized by
%      only looking at the counts of both environments.
%
% @param Environment1 An environment.
% @param Environment2 An environment.

subenvironment(Environment1, Environment2):-
  environment_assumptions(Environment1, Assumptions1),
  environment_assumptions(Environment2, Assumptions2),
  ord_subset(Assumptions1, Assumptions2).

%% union_environments(
%%   +Environment1:environment,
%%   +Environment2:environment,
%%   -UnionEnvironment:environment
%% ) is det.
% Returns the union environment of =Environment1= and =Environment2=.
%
% @param Environment1 The URI of an environment.
% @param Environment2 The URI of an environment.
% @param UnionEnvironment The URI of an environment.

union_environments(Environment1, Environment2, UnionEnvironment):-
  % By inspecting the number of assumptions for both environments, we
  % can assess which is the cheaper argument order.
  environment_cardinality(Environment1, Count1),
  environment_cardinality(Environment2, Count2),
  (
    Count1 > Count2
  ->
    union_environments1(Environment2, Environment1, UnionEnvironment)
  ;
    union_environments1(Environment1, Environment2, UnionEnvironment)
  ).

union_environments1(Environment1, Environment2, NewEnvironment):-
  environment_assumptions(Environment1, Assumptions1),
  union_environments2(Assumptions1, Environment2, NewEnvironment).

union_environments2([], NewEnvironment, NewEnvironment).
union_environments2(
  [Assumption | Assumptions],
  Environment2,
  NewEnvironment
):-
  extend_environment(Assumption, Environment2, NewEnvironment2),
  node(ATMS, Assumption),
  \+(nogood(ATMS, NewEnvironment2)),
  union_environments2(Assumptions, NewEnvironment2, NewEnvironment).
