:- module(
  atms_api,
  [
% ATMSs
    atms/1, % ?ATMS:atms
    atms/2, % ?Label:atom
            % ?ATMS:atms
    atms_assumption/2, % ?ATMS:atms
                       % ?Assumption:node
    atms_contradiction/2, % ?ATMS:atms
                          % ?Contradiction:node
    atms_contradictory/2, % ?ATMS:atms
                          % ?Contradictory:node
    atms_id/2, % ?ATMS:atms
               % ?ID:integer
    atms_label/2, % ?ATMS:atms
                  % ?ATMSLabel:atom
    atms_premise/2, % ?ATMS:atms
                    % ?Premise:node
    atms_to_assumptions/2, % +ATMS:atms
                           % -Assumptions:ord_set(node)
    atms_to_dot_name/2, % +ATMS:atms
                        % -ATMSDOTName:atom
    atms_to_environments/2, % +ATMS:atms
                            % -Environments:ord_set(environment)
    atms_to_justifications/2, % +ATMS:atms
                              % -Justifications:ord_set(justification)
    atms_to_nodes/2, % +ATMS:atms
                     % -Nodes:ord_set(node)
    atms_to_nogoods/2, % +ATMS:atms
                       % -Nogoods:ord_set(environment)
    atms_to_premises/2, % +ATMS:atms
                        % -Premises:ord_set(node)

% ENVIRONMENTS
    empty_environment/1, % ?EmptyEnvironment:environment
    empty_environment/2, % ?ATMS:atms
                         % ?EmptyEnvironment:environment
    environment/1, % ?Environment:environment
    environment/2, % ?ATMS:atms
                   % ?Environment:environment
    environment/3, % +ATMS:atms
                   % +Assumptions:ord_set(node)
                   % -Environment:environment
    environment_assumptions/2, % ?Environment:environment
                               % ?Assumptions:ord_set(node)
    environment_cardinality/2, % ?Environment:environment
                               % ?Cardinality:integer
    environment_id/2, % ?Environment:environment
                      % ?EnvironmentID:number
    environment_label/2, % ?Environment:environment
                         % ?EnvronmentLabel:atom
    environment_node/2, % ?Environment:environment
                        % ?Node:node
    environment_to_dot_name/2, % +Environment:environment
                               % -EnvironmentDOTName:atom
    environment_to_dui_label/2, % +Environment:environment
                                % -EnvironmentDUILabel:atom
    environment_to_nodes/2, % +Environment:environment
                            % -Nodes:ord_set(node)
    environments/1, % -Environments:ord_set(environment)
    zero_environment/1, % ?ZeroEnvironment:environment

% JUSTIFICATIONS
    justification/1, % ?Justification:justification
    justification/2, % ?ATMS:atms
                     % ?Justification:justification
    justification/4, % +Informant:object
                     % +Consequence:node
                     % +Antecedents:ord_set(node)
                     % -Justification:justification
    justification_antecedent/2, % ?Justification:justification
                                % ?Antecedent:node
    justification_consequence/2, % ?Justification:justification
                                 % ?Consequence:node
    justification_id/2, % +Justification:justification
                        % -JustificationBareID:number
    justification_informant/2, % ?Justification:justification
                               % ?Informant:atom
    justification_label/2, % ?Justification:justification
                           % ?JustificationLabel:atom
    justification_to_antecedents/2, % +Justification:justification
                                    % -Antecedents:ord_set(node)
    justification_to_dot_name/2, % +Justification:justification
                                 % -JustificationDOTName:atom
    justifications/1, % -Justifications:ord_set(justification)

% NODES
    is_assumption/1, % +Node:node
    is_contradiction/1, % +Node:node
    is_consistent_with/2, % +Node:node
                          % +Environment:environment
    is_in_antecedent/1, % +Nodes:list(uri)
    is_in_node/1, % +Node:node
    is_in_node/2, % +Node:node
                  % +Environment:environment
    is_out_node/1, % +Node:node
    is_out_node/2, % +Node:node
                   % +Environment:environment
    is_premise/1, % ?Node:node
    node/1, % ?Node:node
    node/2, % ?ATMS:atms
            % ?Node:node
    node/3, % +ATMS:atms
            % +Datum:node_or_atom
            % -Node:node
    node_consequence/2, % ?Node:node
                        % ?Consequences:justification
    node_datum/2, % ?Node:node
                  % ?Datum:object
    node_environment/2, % ?Node:node
                        % ?Environment:environment
    node_id/2, % ?Node:node
               % ?NodeID:atom
    node_justification/2, % ?Node:node
                          % ?Justification:justification
    node_label/2, % ?Node:node
                  % ?Label:atom
    node_to_ccm_label/2, % +Node:node
                         % -NodeCCMLabel:atom
    node_to_consequences/2, % +Node:node
                            % -Consequences:ord_set(justification)
    node_to_dot_name/2, % +Node:node
                        % -NodeDOTName:atom
    node_to_justifications/2, % +Node:node
                              % -Justifications:ord_set(justification)
    node_to_label/2, % +Node:node
                     % -Label:ord_set(environment)
    nodes/1, % -Nodes:ord_set(node)
    nogood/2 % ?ATMS:atms
             % ?Nogood:node
  ]
).

/** <module> ATMS API

The API for ATMSs.

@author Wouter Beek
@version 2012/04, 2014/03
*/

:- use_remote_module(atms(atms_db)).
:- use_remote_module(atms(atms_env)).
:- use_remote_module(atms(atms_hierarchy)). % XML namespace.
:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_term(rdf_boolean)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdfs(rdfs_read)).



% ATMS %

%% atms(?ATMS:atms) is nondet.
% Automated Truth Maintenance Systems.
%
% @arg ATMS An ATMS.

atms(ATMS):-
  rdfs_individual_of(ATMS, atms:atms).

%% atms(?ATMS:atms, ?Label:atom) is nondet.
% Returns the =|ATMS|= with the given =|Title|= or fails if no such ATMS
% exists.
%
% @arg Label An atomic ATMS title.
% @arg ATMS An ATMS.

atms(Label, ATMS):-
  atms(ATMS),
  atms_label(ATMS, Label).

% An assumption has exactly one ATMS.
atms_assumption(ATMS, Assumption):-
  nonvar(Assumption), !,
  atms_assumption_(ATMS, Assumption), !.
atms_assumption(ATMS, Assumption):-
  atms_assumption_(ATMS, Assumption).

atms_assumption_(ATMS, Assumption):-
  rdf(ATMS, atms:has_assumption, Assumption, ccm).

%% atms_contradiction(?ATMS:atms, ?ContradictionNode:node) is nondet.
% Returns the contradiction node of the given ATMS.
% The contradiction node is a contradiction, but there can be many more
% contradictions in an ATMS than the contradiction node.
% An ATMS has exactly one contradiction node.
%
% @arg ATMS An ATMS.
% @arg ContradictionNode The contradiction node for a certain ATMS.

atms_contradiction(ATMS, Contradiction):-
  var(ATMS),
  var(Contradiction), !,
  atms_contradiction_node_(ATMS, Contradiction).
% There is a one-to-one mapping between ATMSs and contradiction nodes.
atms_contradiction(ATMS, Contradiction):-
  atms_contradiction_node_(ATMS, Contradiction), !.

atms_contradiction_node_(ATMS, Contradiction):-
  rdf(ATMS, atms:has_contradiction_node, Contradiction, ccm).

%% atms_contradictory(?ATMS:atms, ?Contradiction:node) is nondet.
% Pairs of an ATMS and one of its contradiction nodes.
%
% @arg ATMS An ATMS.
% @arg Contradiction A node that is contradictory.

atms_contradictory(ATMS, Contradiction):-
  rdf(ATMS, atms:has_contradiction, Contradiction, ccm).

atms_id(ATMS, ID):-
  nonvar(ATMS), !,
  atms_id_(ATMS, ID), !.
atms_id(ATMS, ID):-
  atms_id_(ATMS, ID).

atms_id_(ATMS, ID):-
  rdf_datatype(ATMS, atms:has_id, ID, xsd:integer, ccm).

%% atms_label(?ATMS:atms, ?Label:atom) is nondet.
% Pairs of ATMSs and their labels.
%
% @arg ATMS An ATMS.
% @arg Label An atomic label.

atms_label(ATMS, Label):-
  rdfs_label(ATMS, Label).

%% atms_premise(?ATMS:atms, ?Premise:node) is nondet.
% Pairs of an ATMS and one of its premises.
% A premise is a node with the empty node as label.
%
% @arg ATMS An ATMS.
% @arg Premise A node with empty assumptions.

atms_premise(ATMS, Premise):-
  empty_environment(ATMS, EmptyEnvironment),
  environment_node(EmptyEnvironment, Premise).

atms_to_assumptions(ATMS, Assumptions):-
  aggregate_all(
    set(Assumption),
    atms_assumption(ATMS, Assumption),
    Assumptions
  ).

%% atms_to_dot_name(+ATMS:atms, -ATMSDOTName:atom) is det.
% Returns the identifier of the ATMS.
%
% @arg ATMS An ATMS.
% @arg ATMSDOTName The atomic identifier of the given =ATMS=.

atms_to_dot_name(ATMS, ATMSDOTName):-
  atms_id(ATMS, ATMSID),
  atom_concat('a_', ATMSID, ATMSDOTName).

%% atms_to_environments(
%%   +ATMS:atms,
%%   -Environments:ord_set(environment)
%% ) is det.
% Returns the environments of the given ATMS.
%
% @arg ATMS An ATMS.
% @arg Environments An ordered set of environments.

atms_to_environments(ATMS, Environments):-
  aggregate_all(
    set(Environment),
    environment(ATMS, Environment),
    Environments
  ).

%% atms_to_justifications(
%%   +ATMS:atms,
%%   -Justifications:ord_set(justification)
%% ) is det.
% Returns the justifications of the given ATMS.
%
% @arg ATMS An ATMS.
% @arg Justifications An ordered set of justifications.

atms_to_justifications(ATMS, Justifications):-
  aggregate_all(
    set(Justification),
    justification(ATMS, Justification),
    Justifications
  ).

%% atms_to_nodes(+ATMS:atms, -Nodes:ord_set(node)) is det.
% Returns the nodes of the given ATMS.
%
% @arg ATMS An ATMS.
% @arg Nodes An ordered set of nodes.

atms_to_nodes(ATMS, Nodes):-
  aggregate_all(
    set(Node),
    node(ATMS, Node),
    Nodes
  ).

%% atms_to_nogoods(+ATMS:atms, -Nogoods:ord_set(environment)) is det.
% Returns the nogood environments for the given ATMS.
%
% @arg ATMS An ATMS.
% @arg Nogoods An ordered set of environments.

atms_to_nogoods(ATMS, Nogoods):-
  aggregate_all(
    set(Nogood),
    nogood(ATMS, Nogood),
    Nogoods
  ).

%% atms_to_premises(+ATMS:atms, -Premises:ord_set(node)) is det.
% Returns the premised nodes in the ATMS.
% A premise is a node with the empty node as label.
%
% @arg ATMS An ATMS.
% @arg Premises An ordered set of nodes.

atms_to_premises(ATMS, Premises):-
  aggregate_all(
    set(Premise),
    atms_premise(ATMS, Premise),
    Premises
  ).



% ENVIRONMENTS %

empty_environment(EmptyEnvironment):-
  empty_environment(_ATMS, EmptyEnvironment).

%% empty_environment(?ATMS:atms, ?EmptyEnvironment:environment) is nondet.
% Pairs of an ATMS and its empty environment.
%
% @arg ATMS An ATMS.
% @arg EmptyEnvironment An empty environment.

empty_environment(ATMS, EmptyEnvironment):-
  var(ATMS),
  var(EmptyEnvironment), !,
  atms_empty_environment_(ATMS, EmptyEnvironment).
% An ATMS has exactly one empty environment.
empty_environment(ATMS, EmptyEnvironment):-
  atms_empty_environment_(ATMS, EmptyEnvironment), !.

atms_empty_environment_(ATMS, EmptyEnvironment):-
  rdf(ATMS, atms:has_empty_environment, EmptyEnvironment, ccm).

%% environment(?Environment:environment) is nondet.
% Environments, i.e. component assumptions.
%
% @arg Environment An environment.

environment(Environment):-
  rdfs_individual_of(Environment, environment:environment).

% An environment has exactly one ATMS.
environment(ATMS, Environment):-
  nonvar(Environment),
  !,
  environment_(ATMS, Environment),
  !.
environment(ATMS, Environment):-
  environment_(ATMS, Environment).

environment_(ATMS, Environment):-
  rdfs(ATMS, atms:has_environment, Environment, ccm).

%% environment(
%%   +ATMS:atms,
%%   +Assumptions:ord_set(node),
%%   -Environment:environment
%% ) is semidet.
% Finds an environment based on the given ATMS and the given assumption nodes.
%
% @arg ATMS An ATMS
% @arg Assumptions An ordered set of assumption nodes.
% @arg Environment An environment.

environment(ATMS, [], EmptyEnvironment):-
  empty_environment(ATMS, EmptyEnvironment),
  !.
environment(ATMS, Assumptions, Environment):-
  length(Assumptions, Length),
  has_label(Length, Environment, Assumptions),
  % Uniqueness.
  !,
  environment(ATMS, Environment).

%% environment_assumptions(
%%   ?Environment:environment,
%%   ?Assumptions:ord_set(component)
%% ) is nondet.
% Pairs of environments and their assumptions.
%
% @arg Environment An environment
% @arg Assumptions An ordered set of assumption nodes.

environment_assumptions(Environment, Assumptions):-
  var(Environment),
  var(Assumptions), !,
  environment_assumptions_(Environment, Assumptions).
% There is a one-to-one mapping between environments and assumptions.
environment_assumptions(Environment, Assumptions):-
  environment_assumptions_(Environment, Assumptions), !.

environment_assumptions_(Environment, Assumptions):-
  has_label(_Length, Environment, Assumptions).

environment_cardinality(Environment, Cardinality):-
  environment_assumptions(Environment, Assumptions),
  length(Assumptions, Cardinality).

%% environment_id(?Environment:environment, ?EnvironmentID:number) is nondet.
% Pairs of environments and their identifiers.
%
% @arg Environment An environment.
% @arg EnvironmentID The numeric identifier of an environment.

environment_id(Environment, EnvironmentID):-
  nonvar(Environment), !,
  environment_id_(Environment, EnvironmentID), !.
environment_id(Environment, EnvironmentID):-
  environment_id_(Environment, EnvironmentID).

environment_id_(Environment, EnvironmentID):-
  rdf_datatype(Environment, environment:has_id, EnvironmentID, xsd:integer,
      ccm).

%% environment_label(
%%   ?Environment:environment,
%%   ?EnvironmentLabel:atom
%% ) is nondet.
% Pairs of environments and their labels.
%
% @arg Environment An environment.
% @arg Environment The atomic name of an environment.

environment_label(Environment, EnvironmentLabel):-
  nonvar(Environment),
  !,
  environment_label_(Environment, EnvironmentLabel),
  !.
environment_label(Environment, EnvironmentLabel):-
  environment_label_(Environment, EnvironmentLabel).

environment_label_(Environment, EnvironmentLabel):-
  rdfs_label(Environment, EnvironmentLabel).

environment_node(Environment, Node):-
  rdf(Environment, environment:has_node, Node, ccm).

%% environment_to_dot_name(
%%   +Environment:environment,
%%   -EnvironmentDOTName:atom
%% ) is det.
% Returns the name for the given environment, for use in GraphViz
% DOT exports.
%
% @arg Environment An environment.
% @arg EnvironmentDOTName The atomic GraphViz name of an environment.

environment_to_dot_name(Environment, EnvironmentDOTName):-
  environment_id(Environment, EnvironmentID),
  !,
  atomic_concat('env_', EnvironmentID, EnvironmentDOTName).

%% environment_to_dui_label(+Environment:environment, -DUILabel:atom) is det.
% Returns the DUI label of the given environment.
%
% @arg Environment An environment.
% @arg DUILabel The atomic label of an environment, for use in
%        a diagnosis UI context.

environment_to_dui_label(Environment, DUILabel):-
  environment_id(Environment, EnvironmentID),
  environment_assumptions(Environment, Assumptions),
  maplist(component_cloud_to_dot_name, Assumptions, AssumptionDOTLabels),
  atomic_list_concat(AssumptionDOTLabels, ',', AssumptionsDOTLabel),
  format(
    atom(DUILabel),
    'env_~w = {~w}',
    [EnvironmentID, AssumptionsDOTLabel]
  ).

environment_to_nodes(Environment, Nodes):-
  aggregate_all(
    set(Node),
    environment_node(Environment, Node),
    Nodes
  ).

%% environments(-Environments:ord_set(environment)) is det.
% Returns all environments.
%
% @arg Environments An ordered set of environments.

environments(Environments):-
  aggregate_all(
    set(Environment),
    environment(Environment),
    Environments
  ).

%% zero_environment(ZeroEnvironment:environment) is nondet.
% Zero environments, i.e., environments that support no nodes.
%
% @arg ZeroEnvironment An environment.

zero_environment(ZeroEnvironment):-
  \+(environment_node(ZeroEnvironment, _SupportedNode)).



% JUSTIFICATIONS %

justification(Justification):-
  rdfs_individual_of(Justification, justification:justification).

%% justification(?ATMS:atms, ?Justification:justification) is nondet.
% Pairs of an ATMS and one of its justifications.
%
% @arg ATMS An ATMS.
% @arg Justification A justification.

justification(ATMS, Justification):-
  rdf(ATMS, atms:has_justification, Justification, ccm).

justification(Informant, Consequence, Antecedents, Justification):-
  node_justification(Consequence, Justification),
  justification_to_antecedents(Justification, Antecedents),
  justification_informant(Justification, Informant).

justification_antecedent(Justification, Antecedent):-
  rdf(Justification, justification:has_antecedent, Antecedent, ccm).

justification_consequence(Justification, Consequence):-
  rdf(Justification, justification:has_consequence, Consequence, ccm).

justification_id(Justification, JustificationID):-
  nonvar(Justification), !,
  justification_id_(Justification, JustificationID), !.
justification_id(Justification, JustificationID):-
  justification_id_(Justification, JustificationID).

justification_id_(Justification, JustificationID):-
  rdf_datatype(Justification, justification:has_id, JustificationID,
      xsd:integer, ccm).

justification_informant(Justification, Informant):-
  rdf_string(Justification, justification:has_informant, Informant, ccm).

justification_label(Justification, LanguageLabel):-
  rdfs_label(Justification, LanguageLabel).

justification_to_antecedents(Justification, Antecedents):-
  aggregate_all(
    set(Antecedent),
    justification_antecedent(Justification, Antecedent),
    Antecedents
  ).

justification_to_dot_name(Justification, JustificationDOTName):-
  justification_id(Justification, JustificationID),
  atomic_concat('j_', JustificationID, JustificationDOTName).

justifications(Justifications):-
  aggregate_all(
    set(Justification),
    justification(Justification),
    Justifications
  ).



% NODES %

is_assumption(Node):-
  rdf_true(Node, node:is_assumption, ccm).

is_contradiction(Node):-
  rdf_true(Node, node:is_contradiction, ccm).

is_consistent_with(Node, Environment):-
  node(ATMS, Node),
  node_to_label(Node, Label),
  member(LabelEnvironment, Label),
  union_environments(LabelEnvironment, Environment, Union),
  \+(nogood(ATMS, Union)).

is_in_antecedent([]).
is_in_antecedent([Node | Nodes]):-
  node(ATMS, Node),
  empty_environment(ATMS, EmptyEnvironment),
  is_weave(ATMS, EmptyEnvironment, [Node | Nodes]).

%% is_in_node(+Node:node) is semidet.
% Succeeds if the given node is in.
%
% @arg Node A node.

is_in_node(Node):-
  \+(is_out_node(Node)).

%% is_in_node(+Node:node, +Environment:environment) is semidet.
% Succeeds if the given node is in.
%
% @arg Node A node.
% @arg Environment An environment.

is_in_node(Node, Environment):-
  node_to_label(Node, Label),
  member(LabelEnvironment, Label),
  subenvironment(LabelEnvironment, Environment),
  % This cut is crucial, otherwise member/2 will backtrack
  % after is_in_node/2 has succeeded (which is unwanted).
  !.

%% is_out_node(+Node:node) is semidet.
% Succeeds if the given node is out.
%
% @arg Node A node.

is_out_node(Node):-
  node_to_label(Node, []).

%% is_out_node(+Node:node, +Environment:environment) is semidet.
% Succeeds if the given node is out.
%
% @arg Node A node.
% @arg Environment An environment.

is_out_node(Node, Environment):-
  \+(is_in_node(Node, Environment)).

%% is_premise(?Node:node) is semidet.
% Succeeds if the given node is a premise.
%
% @arg Node A node.

is_premise(Node):-
  node(ATMS, Node),
  node_environment(Node, EmptyEnvironment),
  empty_environment(ATMS, EmptyEnvironment).

is_weave(_ATMS, _Environment, []).
is_weave(ATMS, Environment, [Node | Nodes]):-
  node_to_label(Node, NodeEnvironments),
  forall(
    member(NodeEnvironment, NodeEnvironments),
    (
      union_environments(Environment, NodeEnvironment, Environment1),
      \+(nogood(ATMS, Environment1)),
      is_weave(ATMS, Environment1, Nodes)
    )
  ).

node(Node):-
  rdfs_individual_of(Node, node:nnode).

%% node(?ATMS:atms, ?Node:node) is nondet.
% Pairs of ATMS's and one of their nodes.
%
% @arg ATMS An ATMS.
% @arg Node A node.

% A node has exactly one ATMS.
node(ATMS, Node):-
  nonvar(Node),
  !,
  node_(ATMS, Node),
  !.
node(ATMS, Node):-
  node_(ATMS, Node).

node_(ATMS, Node):-
  rdf(ATMS, atms:has_node, Node, ccm).

%% node(?ATMS:atms, ?Datum:node_or_atom, ?Node:node) is semidet.
% Returns the node in ATMS with contents datum.
% Fails if there is no such node.
%
% @arg ATMS An ATMS.
% @arg Datum Either a node or an atom.
% @arg Node The URI of a node.

node(ATMS, Datum, Node):-
  nonvar(Node), !,
  rdf(Node, node:has_datum, Datum, ccm),
  node(ATMS, Node), !.
node(ATMS, Datum, Node):-
  nonvar(ATMS),
  nonvar(Datum), !,
  rdf(Node, node:has_datum, Datum, ccm),
  node(ATMS, Node), !.
node(ATMS, Datum, Node):-
  node(ATMS, Node),
  rdf(Node, node:has_datum, Datum, ccm).

%% node_consequence(?Node:node, ?Consequence:justification) is nondet.
% Pairs of a node and one of the justiifcations for which it is an antecedent.
%
% @arg Node A node.
% @arg Consequence A justification.

node_consequence(Node, Consequence):-
  rdf(Node, node:has_consequence, Consequence, ccm).

% A node has exactly one datum.
node_datum(Node, Datum):-
  nonvar(Node), !,
  node_datum_(Node, Datum), !.
node_datum(Node, Datum):-
  node_datum_(Node, Datum).

node_datum_(Node, Datum):-
  rdf(Node, node:has_datum, Datum, ccm).

node_environment(Node, Environment):-
  rdf(Node, node:has_environment, Environment, ccm).

node_id(Node, NodeID):-
  node_datum(Node, Datum),
  node_id_(Datum, NodeID).

node_id_(Datum, DatumID):-
  point(Datum), !,
  point_id(Datum, DatumID).
node_id_(Datum, DatumID):-
  component_cloud(Datum), !,
  component_cloud_id(Datum, DatumID).
node_id_(Datum, Datum).

node_justification(Node, Justification):-
  rdf(Node, node:has_justification, Justification, ccm).

node_label(Node, Label):-
  node_datum(Node, Datum),
  node_label_(Datum, Label).

node_label_(Point, PointLabel):-
  point(Point), !,
  point_id(Point, PointID),
  point(Expression, Space, Point),
  space_to_ccm_label(Space, SpaceCCMLabel),
  expression_to_ccm_label(Expression, ExpressionCCMLabel),
  format(
    atom(PointLabel),
    'P~w=(S~w,E~w)',
    [PointID, SpaceCCMLabel, ExpressionCCMLabel]
  ).
node_label_(ComponentCloud, ComponentCloudLabel):-
  component_cloud(ComponentCloud), !,
  component_cloud_id(ComponentCloud, ComponentCloudID),
  component_cloud_abbreviation(ComponentCloud, ComponentCloudAbbreviation),
  format(
    atom(ComponentCloudLabel),
    '~w:~w',
    [ComponentCloudID, ComponentCloudAbbreviation]
  ).
node_label_(Atom, Atom):-
  atom(Atom).

node_to_ccm_label(Node, Label):-
  node_datum(Node, Datum),
  node_to_ccm_label_(Datum, Label).

node_to_ccm_label_(Datum, Label):-
  point(Datum), !,
  point_to_ccm_label(Datum, Label).
node_to_ccm_label_(Datum, Label):-
  component_cloud(Datum), !,
  component_cloud_to_ccm_label(Datum, Label).
node_to_ccm_label_(Datum, Datum).

%% node_to_consequences(
%%   +Node:node,
%%   -Consequences:ord_set(justification)
%% ) is det.
% Returns all justifications for with the given node is an antecedent.
%
% @arg Node A node.
% @arg Consequences An ordered set of justifications.

node_to_consequences(Node, Consequences):-
  aggregate_all(
    set(Consequence),
    node_consequence(Node, Consequence),
    Consequences
  ).

node_to_dot_name(Node, NodeDOTName):-
  node_datum(Node, Datum),
  node_to_dot_name_(Datum, NodeDOTName).

node_to_dot_name_(Datum, DatumDOTName):-
  point(Datum), !,
  point_to_dot_name(Datum, DatumDOTName).
node_to_dot_name_(Datum, DatumDOTName):-
  component_cloud(Datum), !,
  component_cloud_to_dot_name(Datum, DatumDOTName).
% This is used for falsum nodes.
node_to_dot_name_(Datum, Datum).

node_to_justifications(Node, Justifications):-
  aggregate_all(
    set(Justification),
    node_justification(Node, Justification),
    Justifications
  ).

node_to_label(Node, Label):-
  aggregate_all(
    set(Environment),
    node_environment(Node, Environment),
    Label
  ).

nodes(Nodes):-
  aggregate_all(
    set(Node),
    node(Node),
    Nodes
  ).

nogood(ATMS, Nogood):-
  nonvar(Nogood), !,
  nogood_(ATMS, Nogood), !.
nogood(ATMS, Nogood):-
  nogood_(ATMS, Nogood).

nogood_(ATMS, Nogood):-
  rdf(ATMS, atms:has_nogood, Nogood, ccm).

