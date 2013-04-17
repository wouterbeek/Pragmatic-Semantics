:- module(
  agent,
  [
    add_agent/2, % +Name:atom
                 % -Agent:agent
    add_learner/2, % +Name:atom
                   % -Learner:agent
    add_teacher/2, % +Name:atom
                   % -Teacher:agent
    learner/1, % ?Learner:agent
    learner/2, % ?UseCase:use_case
               % ?Learner:agent
    teacher/1, % ?Teacher:agent
    teacher/2, % ?UseCase:use_case
               % ?Teacher:agent

% BELIEF
    add_belief/2, % +Agent:agent
                  % -Point:point
    agent_to_beliefs/2, % +Agent:agent
                        % -Beliefs:ord_set(point)
    believe_point/2, % +Agent:agent
                     % -Point:point

% CONSIDER
    consider_component_cloud/2, % +Agent:agent
                                % +ComponentCloud:component_cloud
    consider_point/2, % +Agent:agent
                      % +Point:point
    consider_point_cloud/2, % +Agent:agent
                            % +PointCloud:point_cloud
    deconsider_component_cloud/2, % +Agent:agent
                                  % +ComponentCloud:component_cloud
    deconsider_point_cloud/2, % +Agent:agent
                              % +PointCloud:point_cloud
    does_consider/2, % ?Agent:agent
                     % ?Node:node
    does_consider_component_cloud/2, % ?Agent:agent
                                     % ?ConsideredComponentCloud:component_cloud
    does_consider_point/2, % ?Agent:agent
                           % ?ConsideredPoint:point
    does_consider_point_cloud/2, % ?Agent:agent
                                 % ?ConsideredPointCloud:point_cloud
    does_consider_point_clouds/2, % +Agent:agent
                                  % -ConsideredPointClouds:ord_set(point_cloud)

% GDE PROPERTIES
    diagnosis_component_cloud/2, % +Diagnosis:diagnosis
                                 % -DiagnosisComponentCloud:component_cloud
    diagnosis_component_clouds/2 % +Diagnosis:diagnosis
                                 % -DiagnosisComponentClouds:ord_set(component_cloud)
  ]
).

/** <module> Agents

Agents are rudimentary representations of rational subjects involved in
a use case.

---+ Believing and considering

For each agent we represent two things:
    1. What an agent considers, i.e., what is in his or her current horizon.
    2. What an agent beliefs, i.e., which propositions s/he holds to be true.

---++ Belief

Beliefs is asserted by the ATMS. This agent module adds metadata that TMS's
typically do not keep track of, e.g., the time at which an agent expressed
a certain belief, or link between a believed datum and an explanation of
why that datum was believed.

Beliefs are *|premised nodes|* in the TMS and *observations* in GDE.

*|Conditional beliefs|* are beliefs that have a non-empty environment label in
the ATMS.

Since this agent module works on top of the CCM representation,
the contents of a beliefs are points. Points are temporally designated
expressions. Agents can therefore believe that something only holds some
of the time, allowing beliefs of dynamic systems and processes to be
represented.

---++ Consideration

Both component clouds and point clouds can be considered. Belief is temporal
and therefore about behavior / system dynamics. Considerations are atemporal
and are therefore about the invariant structure of the CCM representation.

---+ Agent types

There are currently two agent subtypes: learner and teachers.

---++ Learner agent

A learner agent has the ability to ask informative question to a teacher
agent.

What the learner beliefs are called *observations* in diagnosis.

What the learner considers is the *scope* of that agent with respect to
the current CCM.

---++ Teacher agent

A teacher agent has the ability to ask normative questions to a learner agent.

The teacher believes all points in CCM BASE / the model that is based on the
simulation results. The teacher thus represents the simulation engine's
outcomes.

The teacher considers the entire CCM. The teacher has the ability to
change the scope of the learner (i.e., bring other aspects of the modeled
material to the learner's attention).

@author Wouter Beek
@version 2011/08-2012/05, 2012/08-2012/09
*/

:- use_module(atms(atms_api)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_conflict)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).



init_agent:-
  rdf_assert(agent:agent, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(agent:agent, 'Agents.', ccm),

  rdf_assert(agent:learner, rdfs:subClassOf, agent:agent, ccm),
  rdfs_assert_label(agent:learner, 'Learner agents.', ccm),

  rdf_assert(agent:teacher, rdfs:subClassOf, agent:agent, ccm),
  rdfs_assert_label(agent:teacher, 'Teacher agents.', ccm),

  rdf_assert(agent:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(agent:relation, 'An agent relation.', ccm),

  rdf_assert(agent:communicated, rdfs:subPropertyOf, agent:relation, ccm),
  rdfs_assert_label(agent:communicated, 'An agent communicated.', ccm),

  rdf_assert(agent:asked, rdfs:subPropertyOf, agent:communicated, ccm),
  rdfs_assert_label(agent:asked, 'An agent asked a question.', ccm),

  rdf_assert(agent:answered, rdfs:subPropertyOf, agent:communicated, ccm),
  rdfs_assert_label(agent:answered, 'An agent answered a question.', ccm),

  rdf_assert(agent:believes, rdfs:subPropertyOf, agent:relation, ccm),
  rdfs_assert_label(agent:believes, 'An agent believes a certain subject matter.', ccm),

  rdf_assert(agent:considers, rdfs:subPropertyOf, agent:relation, ccm),
  rdfs_assert_label(agent:considers, 'An agent considers a certain subject matter.', ccm),

  rdf_assert(agent:considers_component_cloud, rdfs:subPropertyOf, agent:considers, ccm),
  rdfs_assert_label(agent:considers_component_cloud, 'An agent considers a component cloud.', ccm),

  rdf_assert(agent:considers_point_cloud, rdfs:subPropertyOf, agent:considers, ccm),
  rdfs_assert_label(agent:considers_point_cloud, 'An agent considers a point cloud.', ccm).



% AGENTS %

%% add_agent(Name, Agent) is det.
% Returns a new agent with the given name.
%
% @param Name The atomic name of an agent.
% @param Agent An agent.

add_agent(Name, Agent):-
  init_agent,
  flag(agent_id, AgentID, AgentID + 1),
  atom_number(AtomicAgentID, AgentID),
  rdf_global_id(agent:AtomicAgentID, Agent),
  rdf_assert(Agent, rdf:type, agent:agent),
  rdfs_assert_label(Agent, Name, ccm).

%% add_learner(Name, Teacher) is det.
% Adds a teacher agent with the given name.
%
% @param Name The atomic name of a teacher.
% @param Learner An agent.

add_learner(Name, Learner):-
  add_agent(Name, Learner),
  rdf_assert(Learner, rdf:type, agent:learner, ccm).

%% add_teacher(Name, Teacher) is det.
% Adds a teacher agent with the given name.
%
% @param Name The atomic name of a teacher.
% @param Teacher An agent.

add_teacher(Name, Teacher):-
  add_agent(Name, Teacher),
  rdf_assert(Teacher, rdf:type, agent:teacher, ccm).

%% learner(?Learner:agent) is nondet.
% Learner agents with or without a use case.
%
% @param Learner An agent.
% @see learner/2 for learners related to a use case.

learner(Learner):-
  rdfs_individual_of(Learner, agent:learner).

%% learner(?UseCase:use_case, ?Learner:agent) is nondet.
% Pairs of learners an use cases to which they are related.
%
% @param UseCase A use case.
% @param Learner An agent.

learner(UseCase, Learner):-
  rdfs(UseCase, use_case:has_learner, Learner, ccm).

%% teacher(?Teacher:agent) is nondet.
% Teacher agents with or without a use case.
%
% @param Teacher An agent.
% @see teacher/2 for teachers related to a use case.

teacher(Teacher):-
  rdfs_individual_of(Teacher, agent:teacher).

%% teacher(?UseCase:use_case, ?Teacher:agent) is nondet.
% Pairs of teachers an use cases to which they are related.
%
% @param UseCase A use case.
% @param Teacher An agent.

teacher(UseCase, Teacher):-
  rdfs(UseCase, use_case:has_teacher, Teacher, ccm).



% BELIEF %

%% add_belief(+Agent:agent, +Point:point) is semidet
% Succeeds if the given agent believes the given point.
%
% @param Agent An agent.
% @param Point A point.

add_belief(Agent, Point):-
  rdf_assert(Agent, agent:believes, Point, ccm).

%% agent_to_beliefs(+Agent:agent, -Beliefs:ord_set(point)) is det.
% Returns all the beliefs of the given agent.
%
% @param Agent An agent.
% @param Beliefs An ordered set of points.

agent_to_beliefs(Agent, Beliefs):-
  setoff(
    Belief,
    believe_point(Agent, Belief),
    Beliefs
  ).

%% believe_point(?Agent:agent, ?Point:point) is nondet.
% Pairs of agents and points they believe.
%
% @param Agent An agent.
% @param Point A point.

believe_point(Agent, Point):-
  rdf(Agent, agent:believes, Point, ccm).



% CONSIDERS %

%% consider_component_cloud(
%%   +Agent:agent,
%%   +ComponentCloud:component_cloud
%% ) is det.
% Asserts that the given agent considers the given component cloud.
%
% @param Agent An agent.
% @param ComponentCloud A component cloud.

consider_component_cloud(Agent, ComponentCloud):-
  rdf_assert(Agent, agent:considers_component_cloud, ComponentCloud, ccm).

%% consider_point(+Agent:agent, +Point:point) is det.
% Asserts that the given agent consider the given point.
%
% @param Agent An agent.
% @param Point A point.

consider_point(Agent, Point):-
  point_cloud(Point, PointCloud),
  consider_point_cloud(Agent, PointCloud).

%% consider_point_cloud(+Agent:agent, +PointCloud:point_cloud) is det.
% Asserts that the given agent consider the given point cloud.
%
% @param Agent An agent.
% @param PointCloud A point cloud.

consider_point_cloud(Agent, PointCloud):-
  rdf_assert(Agent, agent:considers_point_cloud, PointCloud, ccm).

%% deconsider_component_cloud(
%%   +Agent:agent,
%%   +ComponentCloud:component_cloud
%% ) is det.
% Retracts the consideration of the given component cloud by the given agent.
%
% @param Agent An agent.
% @param ComponentCloud A component cloud.

deconsider_component_cloud(Agent, ComponentCloud):-
  rdf_retractall(Agent, agent:considers_component_cloud, ComponentCloud, ccm).

%% deconsider_point_cloud(+Agent:agent, +PointCloud:point_cloud) is det.
% Retracts the consideration of the given point cloud by the given agent.
%
% @param Agent An agent.
% @param PointCloud A point cloud.

deconsider_point_cloud(Agent, PointCloud):-
  rdf_retractall(Agent, agent:considers_point_cloud, PointCloud, ccm).

%% does_consider(?Agent:agent, ?Node:node) is nondet.
% Pairs of an agent and nodes it consideres.
% A node is either a component cloud or a point.
%
% @param Agent An agent.
% @param Node A node.

does_consider(Agent, Node):-
  does_consider_component_cloud(Agent, Node).
does_consider(Agent, Node):-
  does_consider_point(Agent, Node).

%% does_consider_component_cloud(
%%   ?Agent:agent,
%%   ?ComponentCloud:component_cloud
%% ) is nondet.
% Pairs of agents and component clouds they consider.
%
% @param Agent An agent.
% @param ComponentCloud A component cloud.

% A pair of agent and component cloud is semidet.
does_consider_component_cloud(Agent, ComponentCloud):-
  nonvar(Agent),
  nonvar(ComponentCloud),
  !,
  does_consider_component_cloud_(Agent, ComponentCloud),
  !.
does_consider_component_cloud(Agent, ComponentCloud):-
  does_consider_component_cloud_(Agent, ComponentCloud).

does_consider_component_cloud_(Agent, ComponentCloud):-
  rdf(Agent, agent:considers_component_cloud, ComponentCloud, ccm).

%% does_consider_point(?Agent:agent, ?Point:point) is nondet.
% Pairs of agents and points they consider.
%
% @param Agent An agent.
% @param Point A point.

does_consider_point(Agent, Point):-
  nonvar(Point),
  !,
  point_cloud(Point, PointCloud),
  does_consider_point_cloud(Agent, PointCloud).
does_consider_point(Agent, Point):-
  does_consider_point_cloud(Agent, PointCloud),
  point_cloud(Point, PointCloud).

% Pairs of agents and points are semidet.
does_consider_point_cloud(Agent, PointCloud):-
  nonvar(Agent),
  nonvar(PointCloud),
  !,
  does_consider_point_cloud_(Agent, PointCloud),
  !.
does_consider_point_cloud(Agent, PointCloud):-
  does_consider_point_cloud_(Agent, PointCloud).
does_consider_point_cloud(Agent, PointCloud):-
  does_consider_point_cloud_(Agent, PointCloud).

does_consider_point_clouds(Agent, ConsideredPointClouds):-
  setoff(
    ConsideredPointCloud,
    does_consider_point_cloud(Agent, ConsideredPointCloud),
    ConsideredPointClouds
  ).

does_consider_point_cloud_(Agent, PointCloud):-
  rdf(Agent, agent:considers_point_cloud, PointCloud, ccm).



% GDE PROPERTIES %

diagnosis_component_cloud(Diagnosis, ComponentCloud):-
  nonvar(ComponentCloud),
  !,
  active_component_cloud(ComponentCloud),
  learner(Diagnosis, Learner),
  does_consider_component_cloud(Learner, ComponentCloud).
diagnosis_component_cloud(Diagnosis, ComponentCloud):-
  learner(Diagnosis, Learner),
  does_consider_component_cloud(Learner, ComponentCloud),
  active_component_cloud(ComponentCloud).

diagnosis_component_clouds(Diagnosis, ComponentClouds):-
  setoff(
    ComponentCloud,
    diagnosis_component_cloud(Diagnosis, ComponentCloud),
    ComponentClouds
  ).
