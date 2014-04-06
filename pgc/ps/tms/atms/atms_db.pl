:- module(
  atms_db,
  [
% ATMSS
    atms_add_nogood/2, % +ATMS:atms
                       % +Nogood:environment
    atms_remove_node/2, % +ATMS:atms
                        % +Node:node
    atms_replace_node/3, % +ATMS:atms
                         % +RemoveNode:node
                         % +AddNodes:list(node)

% ENVIRONMENTS
    add_environment/3, % +ATMS:atms
                       % +Assumptions:ord_set(node)
                       % -Environment:environment
    environment_add_node/2, % +Environment:environment
                            % +Node:node
    environment_change_nogood/2, % +Environment:environment
                                 % +NogoodStatus:boolean
    environment_remove_node/2, % +Environment:environment
                               % +Node:node
    environment_replace_nodes/3, % +Environment:environment
                                 % +RemoveNodes:list(node)
                                 % +AddNodes:list(node)
    has_label/3, % ?Length:integer
                 % ?Environment:environment
                 % ?Assumptions:ord_set(node)

% JUSTIFICATIONS
    add_antecedent/2, % +Justification:justification
                      % +Antecedent:node
    add_consequence/2, % +Justification:justification
                       % +Consequence:node
    add_justification/4, % +Informant:atom
                         % +Consequence:node
                         % +Antecedents:ord_set(node)
                         % -Justification:justification
    remove_antecedent/2, % +Justification:justification
                         % +Antecedent:node
    remove_consequence/2, % +Justification:justification
                          % +Consequence:node
    replace_antecedent/3, % +Justification:justification
                          % +RemoveAntecedent:node
                          % +Antecedents:list(node)
    replace_consequence/3, % +Justification:justification
                           % +RemoveConsequences:node
                           % +AddConsequences:list(node)

% NODES
    add_node/3, % +ATMS:atms
                % +Datum:node_or_atom
                % -Node:node
    add_node/5, % +ATMS:atms,
                % +Datum:node_or_atom
                % +IsAssumption:boolean
                % +IsContradiction:boolean
                % -Node:node
    assume_node/1, % +Node:node
    make_contradiction/1, % +Node:node
    node_change_label/2, % +Node:node
                         % +Label:ord_set(environment)
    nogood_nodes/3, % +ATMS:atms
                    % +Informant:atom
                    % +Nodes:ord_set(node)
    premise_node/1, % +Node:node
    remove_node/1 % +Node:node
  ]
).

/** <module> The ATMS database.

This module implements the new ATMS which Wouter based on Forbus and
De Kleer 1993 book 'Building Problem Solvers'.

@author Wouter Beek
@version 2011/11-2012/01, 2012/08, 2014/03
*/

:- use_remote_module(atms(atms_api)).
:- use_remote_module(atms(atms_build)).
:- use_remote_module(atms(atms_hierarchy)). % XML namespace.
:- use_remote_module(atms(atms_env)).
:- use_remote_module(atms(atms_update)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_term(rdf_boolean)).

:- dynamic(has_label/3).



% ATMSS %

%% atms_add_environment(
%%   +ATMS:atms,
%%   +Environment:environment,
%%   +Category:integer
%% ) is det.
% Add an environment with category assumptions to the ATMS.
%
% @arg ATMS An ATMS.
% @arg Environment An environment.
% @arg Category An integer.

atms_add_environment(ATMS, Environment, Category):-
  format(atom(Relation), 'has_environment_~w', [Category]),
  rdf_global_id(atms:Relation, Predicate),
  (
    rdf(_Subject, Predicate, _Object, ccm)
  ->
    true
  ;
    rdf_assert(Predicate, rdfs:subPropertyOf, atms:has_environment, ccm),
    rdfs_assert_label(Predicate, Relation, ccm)
  ),
  rdf_assert(ATMS, Predicate, Environment, ccm).

%% atms_add_nogood(+ATMS:atms, +Nogood:environment) is det.
% Add =Nogood= to =ATMS=.
%
% @arg ATMS An ATMS.
% @arg Nogood The URI of an environment.

atms_add_nogood(ATMS, Nogood):-
  rdf_assert(ATMS, atms:has_nogood, Nogood, ccm).

atms_remove_node(ATMS, Node):-
  rdf_retractall(ATMS, atms:has_assumption, Node, ccm),
  rdf_retractall(ATMS, atms:has_contradiction_node, Node, ccm),
  rdf_retractall(ATMS, atms:has_node, Node, ccm),
  rdf_retractall(ATMS, atms:has_nogood, Node, ccm),
  
  findall(
    AffectedEnvironment,
    environment_node(AffectedEnvironment, Node),
    AffectedEnvironments
  ),
  forall(
    member(AffectedEnvironment, AffectedEnvironments),
    environment_remove_node(AffectedEnvironment, Node)
  ),
  
  findall(
    JustificationHasAntecedent,
    justification_antecedent(JustificationHasAntecedent, Node),
    JustificationHasAntecedents
  ),
  forall(
    member(JustificationHasAntecedent, JustificationHasAntecedents),
    remove_antecedent(JustificationHasAntecedent, Node)
  ),

  % Replace the consequences of justifications.
  findall(
    JustificationHasConsequence,
    justification_consequence(JustificationHasConsequence, Node),
    JustificationHasConsequences
  ),
  forall(
    member(JustificationHasConsequence, JustificationHasConsequences),
    remove_consequence(JustificationHasConsequence, Node)
  ),
  
  % Remove the node resource.
  remove_node(Node).

atms_replace_node(ATMS, RemoveNode, AddNodes):-
  % Replaced the nodes of environments.
  findall(
    AffectedEnvironment,
    environment_node(AffectedEnvironment, RemoveNode),
    AffectedEnvironments
  ),
  forall(
    member(AffectedEnvironment, AffectedEnvironments),
    environment_replace_nodes(AffectedEnvironment, [RemoveNode], AddNodes)
  ),

  % Replace the antecedents of justifications.
  findall(
    JustificationHasAntecedent,
    justification_antecedent(JustificationHasAntecedent, RemoveNode),
    JustificationHasAntecedents
  ),
  forall(
    member(JustificationHasAntecedent, JustificationHasAntecedents),
    replace_antecedent(JustificationHasAntecedent, RemoveNode, AddNodes)
  ),

  % Replace the consequences of justifications.
  findall(
    JustificationHasConsequence,
    justification_consequence(JustificationHasConsequence, RemoveNode),
    JustificationHasConsequences
  ),
  forall(
    member(JustificationHasConsequence, JustificationHasConsequences),
    replace_consequence(JustificationHasConsequence, RemoveNode, AddNodes)
  ),

  % Remove the node from the ATMS.
  atms_remove_node(ATMS, RemoveNode).



% ENVIRONMENTS %

%% add_environment(
%%   +ATMS:atms,
%%   +Assumptions:ord_set(node),
%%   -Environment:environment
%% ) is det.
% Creates a new environment.
%
% @arg ATMS An ATMS.
% @arg Assumptions An ordered set of nodes.
% @arg Environment An environment.

add_environment(ATMS, Assumptions, Environment):-
  % Create the resource.
  atms_id(ATMS, ATMSID),
  format(atom(EnvironmentsFlag), 'atms_environment_~w', [ATMSID]),
  flag(EnvironmentsFlag, ID, ID + 1),

  % DEBUG
  X is ID mod 100, %DEB
  (X == 0 -> send(@pce, write_ln, ID) ; true), %DEB

  % ID
  atom_number(AtomicID, ID),
  rdf_global_id(environment:AtomicID, Environment),

  % Assert the instance/definition relationship.
  rdf_assert_individual(Environment, environment:environment, ccm),

  % Assert the identifier.
  rdf_assert_datatype(Environment, environment:has_id, ID, xsd:integer, ccm),

  % Assert the label.
  length(Assumptions, Length),
  assert(has_label(Length, Environment, Assumptions)),

  % Assert the natural language label.
  rdfs_assert_label(Environment, AtomicID, ccm),

  % Add the environment under its cardinality category.
  length(Assumptions, Length),
  atom_number(Category, Length),
  atms_add_environment(ATMS, Environment, Category),

  set_environment_contradictory(ATMS, Environment).

environment_add_node(Environment, Node):-
  rdf_assert(Environment, environment:has_node, Node, ccm).

environment_change_nogood(Environment, NogoodStatus):-
  rdf_assert_datatype(Environment, environment:is_nogood, NogoodStatus,
      xsd:boolean, ccm).

environment_remove_node(Environment, Node):-
  rdf_retractall(Environment, environment:has_node, Node, ccm).

environment_replace_nodes(Environment, RemoveNodes, AddNodes):-
  % Change the environment assertion.
  environment_assumptions(Environment, Nodes),
  ord_subtract(Nodes, RemoveNodes, Nodes_),
  ord_union(Nodes_, AddNodes, NewNodes),
  length(NewNodes, NewLength),
  retract(has_label(_OldLength, Environment, RemoveNodes)),
  assert(has_label(NewLength, Environment, NewNodes)),

  % Change the environment resource.
  maplist(environment_remove_node(Environment), RemoveNodes),
  maplist(environment_add_node(Environment), AddNodes).

set_environment_contradictory(ATMS, Environment):-
  nogood(ATMS, Environment), !.
set_environment_contradictory(ATMS, Environment):-
  nogood(ATMS, Nogood),
  subenvironment(Nogood, Environment), !,
  environment_change_nogood(Environment, true).
% Never fail.
set_environment_contradictory(_ATMS, _Environment).



% JUSTIFICATIONS %

add_antecedent(Justification, Antecedent):-
  rdf_assert(Justification, justification:has_antecedent, Antecedent, ccm),
  rdf_assert(Antecedent, node:has_consequence, Justification, ccm).

add_consequence(Justification, Consequence):-
  rdf_assert(Justification, justification:has_consequence, Consequence, ccm),
  rdf_assert(Consequence, node:has_justification, Justification, ccm).

%% add_justification(
%%   +Informant:atom,
%%   +Consequence:node,
%%   +Antecedents:ord_set(node),
%%   -Justification:justification
%% ) is det.

add_justification(Informant, Consequence, Antecedents, Justification):-
  node(ATMS, Consequence),
  atms_id(ATMS, ATMSID),
  format(atom(JustificationsFlag), 'atms_justification_~w', [ATMSID]),
  flag(JustificationsFlag, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(justification:AtomicID, Justification),
  rdf_assert_individual(Justification, justification:justification, ccm),
  rdf_assert_datatype(Justification, justification:has_id, ID, xsd:integer, ccm),
  rdfs_assert_label(Justification, AtomicID, ccm),
  rdf_assert(ATMS, atms:has_justification, Justification, ccm),
  rdf_assert_string(Justification, justification:has_informant, Informant, ccm),
  maplist(add_antecedent(Justification), Antecedents),
  add_consequence(Justification, Consequence),
  % Propagate changes from the root to the rest of the network.
  propagate(Justification).

remove_antecedent(Justification, Antecedent):-
  rdf_retractall(Justification, justification:has_antecedent, Antecedent, ccm),
  rdf_retractall(Antecedent, node:has_consequence, Justification, ccm).

remove_consequence(Justification, Consequence):-
  rdf_retractall(Justification, justification:has_consequence, Consequence, ccm),
  rdf_retractall(Consequence, node:has_justification, Justification, ccm).

replace_antecedent(Justification, RemoveAntecedent, AddAntecedents):-
  remove_antecedent(Justification, RemoveAntecedent),
  maplist(add_antecedent(Justification), AddAntecedents).

replace_consequence(Justification, RemoveConsequence, AddConsequences):-
  remove_consequence(Justification, RemoveConsequence),
  maplist(add_consequence(Justification), AddConsequences).



% NODES %

%% add_node(+ATMS:atms, +Datum:node_or_atom, -Node:node) is det.
% Creates a node with contents =Datum= for the =ATMS=.
% The node is neither an assumption nor a contradiction.
%
% @arg ATMS An ATMS.
% @arg Datum Either a node or the atom =falsum=, indicating
%        a contradiction node.
% @arg Node The URI of a node.
% @see add_node/5 for creating nodes that are either assumptions or
%      contradictions.

add_node(ATMS, Datum, Node):-
  add_node(ATMS, Datum, fail, fail, Node).

%% add_node(
%%   +ATMS:atms,
%%   +Datum:node_or_atom,
%%   +IsAssumption:boolean,
%%   +IsContradiction:boolean,
%%   -Node:node
%% ) is det.
% Creates a node with contents =Datum= for the =ATMS=.
% The node is either an assumption, a contradiction, or neither.
%
% @arg ATMS An ATMS.
% @arg Datum Either a node or the atom =falsum=, indicating
%        a contradiction node.
% @arg IsAssumption Either =true= or =false=, indicating whether the
%        node is an assumption or not.
% @arg IsContradiction Either =true= or =false=, indicating whether the
%        node is a contradiction or not.
% @arg Node The URI of a node.

add_node(ATMS, Datum, IsAssumption, IsContradiction, Node):-
  atms_id(ATMS, ATMSID),
  format(atom(NodesFlag), 'atms_node_~w', [ATMSID]),
  flag(NodesFlag, ID, ID + 1),
  (
    node(Datum)
  ->
    node_to_label(Datum, NodeName),
    Node = Datum
  ;
    Datum == falsum
  ->
    atom_number(AtomicID, ID),
    atomic_concat(falsum, AtomicID, NodeName),
    rdf_global_id(node:NodeName, Node),
    rdf_assert_individual(Node, node:falsum, ccm)
  ;
    atom(Datum)
  ->
    rdf_global_id(node:Datum, Node),
    rdf_assert_individual(Node, node:nnode, ccm),
    NodeName = Datum
  ;
    % This should never be the case.
    debug(atms_db, 'Datum ~w is not recognised as a node.', [Datum]),
    fail
  ),

  rdf_assert_datatype(Node, node:has_id, ID, xsd:integer, ccm),
  rdf_assert(Node, node:has_datum, Datum, ccm),
  rdf_assert_datatype(Node, node:is_assumption, IsAssumption, xsd:boolean, ccm),
  rdf_assert_datatype(Node, node:is_contradiction, IsContradiction,
      xsd:boolean, ccm),
  rdf_assert(ATMS, atms:has_node, Node, ccm),

  % If the node is contradictory.
  (
    IsContradiction
  ->
    rdf_assert(ATMS, atms:has_contradiction, Node, ccm)
  ;
    true
  ),

  % If the node is an assumption.
  (
    IsAssumption
  ->
    rdf_assert(ATMS, atms:has_assumption, Node, ccm)
  ;
    true
  ).

assume_node(Node):-
  is_assumption(Node), !.
assume_node(Node):-
  node(ATMS, Node),
  rdf_retractall(Node, node:is_assumption, _, ccm),
  rdf_assert_true(Node, node:is_assumption, ccm),
  find_or_add_environment(ATMS, [Node], Environment),
  update(ATMS, [Environment], Node).

make_contradiction(Node):-
  is_contradiction(Node), !.
make_contradiction(Node):-
  node(ATMS, Node),
  rdf_retractall(Node, node:is_contradiction, _, ccm),
  rdf_assert_true(Node, node:is_contradiction, ccm),
  rdf_assert(ATMS, atms:has_contradiction, Node, ccm),
  node_to_label(Node, Label),
  forall(
    member(Environment, Label),
    create_nogood(ATMS, Environment)
  ).

node_change_label(Node, Label):-
  node_to_label(Node, Label1),
  Label1 == Label, !.
node_change_label(Node, Label):-
  rdf_retractall(Node, node:has_environment, _, ccm),
  forall(
    member(Environment, Label),
    rdf_assert(Node, node:has_environment, Environment, ccm)
  ).

nogood_nodes(ATMS, Informant, [Node|Nodes]):-
  % It is not assured that all nodes have already been asserted
  % as... nodes. This sometimes happens because a node (i.e. something
  % of type 'node') need not have been added as a node (i.e. ATMS part)
  % yet.
  % We sort the nodes, since we assume that the antecedents are sorted
  % when searching for an existing justification.
  aggregate_all(
    set(Node1),
    (
      member(Datum, [Node | Nodes]),
      find_or_add_node(ATMS, Datum, Node1)
    ),
    Nodes1
  ),
  atms_contradiction(ATMS, ContradictionNode),
  find_or_add_justification(Informant, ContradictionNode, Nodes1, _J).

premise_node(Node):-
  find_or_add_justification('Premise', Node, [], _Justification).

remove_node(RemoveNode):-
  rdf_retractall(RemoveNode, node:has_datum, _, ccm),
  rdf_retractall(RemoveNode, node:has_environment, _, ccm),
  rdf_retractall(RemoveNode, node:has_id, _, ccm),
  rdf_retractall(RemoveNode, node:is_assumption, _, ccm),
  rdf_retractall(RemoveNode, node:is_contradiction, _, ccm).

