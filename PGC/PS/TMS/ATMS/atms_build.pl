:- module(
  atms_build,
  [
% ATMSS
    add_atms/1, % -ATMS:atms
    reset_atms/2, % +OldATMS:atms
                  % -NewATMS:atms

% ENVIRONMENTS
    find_or_add_environment/3, % +ATMS:atms
                               % +Assumptions:ord_set(node)
                               % -Environment:environment
    update_environment/4, % +OldEnvironment:environment
                          % +RemoveEnvironment:environment
                          % +AddEnvironment:environment
                          % -NewEnvironment:environment

% JUSTIFICATIONS
    find_or_add_justification/4, % +Informant:atom
                                 % +Consequence:node
                                 % +Antecedents:ord_set(node)
                                 % -Justification:justifcation

% NODES
    add_assumption/2, % +ATMS:atms
                      % +Node:node
    add_premise/2, % +ATMS:atms
                   % +Node:node
    find_or_add_node/3 % +ATMS:atms
                       % +Datum:node_or_atom
                       % -Node:node
  ]
).

/** <module> ATMS BUILD

ATMS build methods.

@author Wouter Beek
@version Aug 2012
*/

:- use_remote_module(atms(atms_api)).
:- use_remote_module(atms(atms_db)).
:- use_remote_module(ile(agent)).
:- use_remote_module(rdf(rdf_build)).



% ATMSS %

%% add_atms(-ATMS:atms) is det.
% Creates and returns a new ATMS.
%
% @arg ATMS An ATMS.

add_atms(ATMS):-
  flag(atms_id, Id, Id + 1),
  atom_number(AtomID, Id),
  rdf_global_id(atms:AtomID, ATMS),
  rdf_assert(ATMS, rdf:type, atms:atms, ccm),
  format(atom(Label), 'ATMS ~w', [Id]),
  rdfs_assert_label(ATMS, Label, ccm),
  rdf_assert_datatype(ATMS, atms:has_id, Id, xsd:integer, ccm),

  % Empty environment.
  add_environment(ATMS, [], EmptyEnvironment),
  rdf_assert(ATMS, atms:has_empty_environment, EmptyEnvironment, ccm),

  % Contradiction node.
  add_node(ATMS, falsum, false, true, ContradictionNode),
  rdf_assert(ATMS, atms:has_contradiction_node, ContradictionNode, ccm).

reset_atms(OldATMS, NewATMS):-
  % Retrieve the old ATMS's information that will be carried over.
  atms_to_assumptions(OldATMS, Assumptions),
  atms_to_nodes(OldATMS, Nodes),
  atms_to_premises(OldATMS, Premises),

  % New ATMS.
  add_atms(NewATMS),

  % Populate the new ATMS with the carried over information.
  forall(
    member(Node, Nodes),
    (
      does_consider(Learner, Node)
    ->
      find_or_add_node(NewATMS, Node, _NewNode)
    ;
      true
    )
  ),
  forall(
    member(Assumption, Assumptions),
    (
      does_consider_component_cloud(Learner, Assumption)
    ->
      add_assumption(NewATMS, Assumption)
    ;
      true
    )
  ),
  forall(
    member(Premise, Premises),
    (
      does_consider_point(Learner, Premise)
    ->
      add_premise(NewATMS, Premise)
    ;
      true
    )
  ).



% ENVIRONMENTS %

find_or_add_environment(ATMS, Assumptions, Environment):-
  environment(ATMS, Assumptions, Environment),
  !.
find_or_add_environment(ATMS, Assumptions, Environment):-
  add_environment(ATMS, Assumptions, Environment).

update_environment(
  OldEnvironment,
  RemovedEnvironment,
  AddEnvironment,
  NewEnvironment
):-
  environment_assumptions(OldEnvironment, OldAssumptions),
  environment_assumptions(RemovedEnvironment, RemovedAssumptions),
  environment_assumptions(AddEnvironment, AddAssumptions),
  ord_subtract(OldAssumptions, RemovedAssumptions, NewAssumptions_),
  (
    OldAssumptions == NewAssumptions_
  ->
    NewAssumptions = NewAssumptions_
  ;
    ord_union(NewAssumptions_, AddAssumptions, NewAssumptions)
  ),
  environment(ATMS, OldEnvironment),
  find_or_add_environment(ATMS, NewAssumptions, NewEnvironment).



% JUSTIFICATIONS %

find_or_add_justification(
  Informant,
  Consequence,
  Antecedents,
  Justification
):-
  justification(Informant, Consequence, Antecedents, Justification),
  !.
find_or_add_justification(
  Informant,
  Consequence,
  Antecedents,
  Justification
):-
  add_justification(Informant, Consequence, Antecedents, Justification).



% NODES %

add_assumption(ATMS, Node):-
  find_or_add_node(ATMS, Node, NNode),
  assume_node(NNode).

add_premise(ATMS, Node):-
  find_or_add_node(ATMS, Node, NNode),
  premise_node(NNode).

%% find_or_add_node(+ATMS:atms, +Datum:node_or_atom, -Node:node) is det.
% Finds or creates a node with contents =Datum= for the default ATMS.
% If the node is created, then it is neither an assumption nor a
% contradiction.
%
% @arg ATMS The URI of the ATMS in which the node is found, or to
%        which the node is added.
% @arg Datum Either a node or the atom =falsum=, indicating
%        a contradiction node, or an atom (used for debugging purposes).
% @arg Node The URI of a node.

find_or_add_node(ATMS, Datum, Node):-
  node(ATMS, Datum, Node),
  !.
find_or_add_node(ATMS, Datum, Node):-
  add_node(ATMS, Datum, Node).
