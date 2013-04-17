:- module(
  atms_update,
  [
    create_nogood/2, % +ATMS:atms
                     % +Environment:environment
    propagate/1, % + Justification:justification
    update/3 % +ATMS:atms
             % +Label:list(environment)
             % +Node:node
  ]
).

/** <module> The incremental update algorithm for an ATMS.

The label of node =N= consists of the environments whose assumptions are
the union of the assumptions of environments from the labels of nodes
from every justification of =N=.

Suppose node =N= has justifications =J_k=.
These justifications have antecedent nodes =N_{i,k}= (for the i-th
assumption node in the k-th justification on =N=).
Then =label(N) = \Union { {e_1, \ldots, e_k } \vert e_j \in L_{i,j} }=

From this union the nogood and subsumed environments are removed.
A nogood is an environment that has been marked as such. The ATMS keeps a
list of nogoods.
A subsumed environment in =label(N)= is an environment whose assumptions
are a strict superset of the assumptions in any of the other members of
=label(N)=.

@author Wouter Beek
@version Nov - Dec 2011
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_db)).
:- use_module(atms(atms_env)).
:- use_module(generic(meta_ext)).



% (12.1) PROPAGATE %

%% propagate(+Justification:justification) is det.
% Propagates changes through the network, starting at Justification.
%
% @param Justification The URI of a justification. This is typically a newly
%        added justification. There can be repercussions for the rest of the
%        network, which are being calculated.

propagate(Justification):-
  debug(atms_update, '[PROPAGATE] justification=~w.', [Justification]),
  justification(ATMS, Justification),
  empty_environment(ATMS, EmptyEnvironment),
  propagate(ATMS, Justification, nil, [EmptyEnvironment]).

%% propagate(
%%   +ATMS:atms,
%%   +Justification:justification,
%%   +Antecedent:node,
%%   +AddedEnvironments:set(environment)
%% ) is det.
% Propagates the change caused by adding =AddedEnvironments= to the label of
% =Antecedent=.
%
% The propagation algorithm assumes that all labels are correct before
% introducing =Justification=. This means that only the incremental changes
% caused by introducing the new justification need be propagated.
%
% @param ATMS An ATMS.
% @param Justification The URI of a justification.
% @param Antecedent The URI of a node.
%        The node whose label is undated with =AddedEnvironments=.
% @param AddedEnvironments A set of URIs of environments.
%        The environments that have been newly added to the label of
%        =Antecedent=.

propagate(ATMS, Justification, Antecedent, AddedEnvironments):-
  debug(
    atms_update,
    '[PROPAGATE] justification=~w\tantecedent=~w\tadded_environments=~w',
    [Justification, Antecedent, AddedEnvironments]
  ),
  justification_to_antecedents(Justification, Antecedents),

  % (12.1.1) Compute the incremental label update.
  weave(ATMS, Antecedent, AddedEnvironments, Antecedents, NewLabel),
  debug(
    atms_update,
    '[WEAVE]\n\tantecedent=~w\n\tAddedEnvironments=~w\n\tantecedents=~w\n\tnew_label=~w',
    [Antecedent, AddedEnvironments, Antecedents, NewLabel]
  ),

  justification_consequence(Justification, Consequence),
  update(ATMS, NewLabel, Consequence).

% (12.1.1) If =NewLabel= is empty, return.
% (12.1.2) Update label and recur.
% (12.2) UPDATE %

%% update(+ATMS:atms, +Label:list(environment), +N:node) is det.
% Adds the new potential label environments L to node N.
%
% @param ATMS An ATMS.
% @param Label A list of new potential environments.
% @param N A node.

% (12.2.1) Detect nogoods.
%          If =Node= is a contradiction, then create a nogood for every
%          environment in =Node's= label and return.

update(_ATMS, [], _Consequence):-
  !.
update(_ATMS, Label, Node):-
  is_contradiction(Node),
  !,
  debug(
    atms_update,
    '[UPDATE-CONTRADICTION] label=~w\tnode=~w',
    [Label, Node]
  ),
  node(ATMS, Node),
  forall(
    member(LabelEnvironment, Label),
    create_nogood(ATMS, LabelEnvironment)
  ).

% (12.2.2) Update =Node='s label while ensuring minimality.

update(ATMS, PotentialLabelEnvironments, Node):-
  debug(
    atms_update,
    '[UPDATE-1] label=~w\tnode=~w',
    [PotentialLabelEnvironments, Node]
  ),
  node_to_label(Node, NodeLabel),

  % (12.2.2.a) Delete every environment from =PotentialLabelEnvironments=
  %            which is a superset of some label environment of =Node=.
  delete_superenvironments(
    PotentialLabelEnvironments,
    NodeLabel,
    PotentialLabelEnvironmentsWithoutSubsumedEnvironments,
    _EnvironmentsRemovedFromPotentialLabelEnvironments
  ),

  % =PotentialLabelEnvironmentsWithoutSubsumedEnvironments= are the
  % environments that are newly added to =Node='s label.
  forall(
    member(
      PotentialLabelEnvironmentWithoutSubsumedEnvironments,
      PotentialLabelEnvironmentsWithoutSubsumedEnvironments
    ),
    (
      debug(
        atms_update,
        '[UPDATE-2] add node=~w to environment=~w',
        [Node, PotentialLabelEnvironmentWithoutSubsumedEnvironments]
      ),
      environment_add_node(
        PotentialLabelEnvironmentWithoutSubsumedEnvironments,
        Node
      )
    )
  ),

  % (12.2.2.b) Delete every environment from the label of =Node= which is a
  %            superset of some element of =PotentialLabelEnvironments=.
  delete_superenvironments(
    NodeLabel,
    PotentialLabelEnvironmentsWithoutSubsumedEnvironments,
    NodeLabelWithoutSubsumedEnvironments,
    RemoveEnvironments
  ),

  % =RemovedEnvironments= are the environments that are newly removed
  % from =Node='s label.
  forall(
    member(RemoveEnvironment, RemoveEnvironments),
    (
      debug(
        atms_update,
        '[COFNLICT-3] remove node=~w from environment=~w',
        [Node, RemoveEnvironment]
      ),
      environment_remove_node(RemoveEnvironment, Node)
    )
  ),

  % (12.2.2.c) Add every remaining environment of
  %            =PotentialLabelEnvironments= to the label of =Node=.
  append(
    [
      PotentialLabelEnvironmentsWithoutSubsumedEnvironments,
      NodeLabelWithoutSubsumedEnvironments
    ],
    NewLabel
  ),
  list_to_ord_set(NewLabel, NewLabel1),
  debug(
    atms_update,
    '[CONFLICT-4] node=~w gets new label=~w',
    [Node, NewLabel1]
  ),
  node_change_label(Node, NewLabel1),

  % (12.2.3) For every justification J in which =Node= is mentioned as an
  %          antecedent, call update_justification/3 handling the rest.
  node_to_consequences(Node, Consequences),
  forall(
    member(Justification, Consequences),
    % The first argument here used to be =PotentialLabelEnvironments=,
    % i.e. the non-updated label (see Forbus and De Kleer 1993).
    % It may also be =PotentialLabelEnvironmentsWithoutSubsumedEnvironments=
    % the literature is unclear on this. Chose this one now.
    % (3a) Propagate the incremental change to =Node='s label to its
    %      consequences.
    propagate(
      ATMS,
      Justification,
      Node,
      PotentialLabelEnvironmentsWithoutSubsumedEnvironments
    )
  ).


% (12.3) WEAVE %

%% weave(
%%   +ATMS:atms,
%%   +Node:node,
%%   +NewEnvironments:set(environment),
%%   +Antecedents:set(node),
%%   -NewLabel:set(environment)
%% ) is det.
% Computes the incremental update produced when adding =NewEnvironments=
% to =Node= for a single justification with =Antecedents=, resulting
% in the incremental label update =NewLabel=.
%
% We only compute the incremental changes produced by adding
% =NewEnvironments= to the label of =Node= for a justification with
% =Antecedents=.
%
% @param ATMS An ATMS.
% @param Node The URI of a node.
% @param NewEnvironments A list of environments.
% @param Antecedents A list of nodes that are the antecedents of a justification.
% @param NewLabel A list of URIs of environments.

% (12.3.1) Iterate over =Antecedents=. Repeat the following
%          steps for each =Antecedent= that is not =Node=, then return.

weave(_ATMS, _Node, NewEnvironments, [], NewEnvironments):-
  !.
weave(ATMS, Node, NewEnvironments, [Node | Antecedents], NewLabel):-
  weave(ATMS, Node, NewEnvironments, Antecedents, NewLabel).
weave(ATMS, Node, NewEnvironments, [Antecedent | Antecedents], NewLabel):-
  % (12.3.2) Incrementally construct the incremental label.
  %          Let =TempLabel= be the set of all environments formed by
  %          computing the union of an environment of =NewEnvironments=
  %          and an environment of =Antecedent='s label.
  node_to_label(Antecedent, AntecedentLabel),
  i_prime(NewEnvironments, AntecedentLabel, TempLabel),

  % (12.3.3) Ensure that =TempLabel= is minimal and contains no known
  %          inconsistencies.
  %          Remove from =TempLabel= all duplicates and nogoods, as well
  %          as any environment subsumed by any other.
  setoff(
    TempEnvironment,
    (
      member(TempEnvironment, TempLabel),
      \+(nogood(ATMS, TempEnvironment))
    ),
    NewEnvironments1
  ),

  % (12.3.3) Set =NewEnvironments= to =NewEnvironments1=.
  weave(ATMS, Node, NewEnvironments1, Antecedents, NewLabel).

%% i_prime(+Environments1, Environments2, PairwiseUnionEnvironments) is det.
% Returns the pairwise union environments of =Environments1= and
% =Environments2=.
%
% @param Environments1 A list of URIs of environments.
% @param Environments1 A list of URIs of environments.
% @param PairwiseUnionEnvironments A list of URIs of environments.

i_prime(NewEnvironments, Label, UnionEnvironments):-
  findall(
    UnionEnvironment,
    (
      member(NewEnvironment, NewEnvironments),
      member(LabelEnvironment, Label),
      union_environments(NewEnvironment, LabelEnvironment, UnionEnvironment)
    ),
    UnionEnvironments
  ).


% (12.4) NOGOOD %

%% create_nogood(+ATMS:atms, +Environment:environment) is det.
% Make =Environment= a nogood.
% Propagate the nogood status to all superenvironments that are currenly
% registered with =ATMS=.
%
% @param ATMS An ATMS.
% @param Environment An environment.

create_nogood(ATMS, Environment):-
  % (12.4.1) Mark =Environment= as nogood.
  environment_change_nogood(Environment, true),

  % (12.4.2) Remove =Environment= and any superset from every node label.
  remove_environment_from_labels(Environment),

  atms_add_nogood(ATMS, Environment),

  atms_to_environments(ATMS, Environments),
  create_nogood1(ATMS, Environments, Environment).

create_nogood1(_ATMS, [], _Environment).

create_nogood1(ATMS, [Environment1 | Environments], Environment):-
  \+(nogood(ATMS, Environment1)),
  subenvironment(Environment, Environment1),
  environment_change_nogood(Environment1, true),
  remove_environment_from_labels(Environment1),
  !,
  create_nogood1(ATMS, Environments, Environment).

create_nogood1(ATMS, [_Environment1 | Environments], Environment):-
  create_nogood1(ATMS, Environments, Environment).


%% remove_environment_from_labels(+Environment:environment) is det.
% Removes Environment from the labels in ATMS that contain it.
% We know which nodes in ATMS have Environment in their label,
% since these nodes are all stored in Environment's node field.
%
% @param Environment An environment.

remove_environment_from_labels(Environment):-
  environment_to_nodes(Environment, Nodes),
  forall(
    member(Node, Nodes),
    rdf_retractall(Node, node:has_environment, Environment, ccm)
  ).
