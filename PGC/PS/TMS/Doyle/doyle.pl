:- module(
  doyle,
  [
% TMS INITIALIZATION
    doyle_init/1, % +TMS:atom
    doyle_reset/1, % +TMS:atom

% TMS CONSTRUCTION
    doyle_add_argument/5, % +TMS:atom
                          % +Premises:list(term)
                          % +Rule:atom
                          % +Conclusion:term
                          % -Justification:atom
    doyle_add_justification/6, % +TMS:atom
                               % +Ins:list(node)
                               % +Outs:list(node)
                               % +Label:atom
                               % +Consequence:iri
                               % -Justification:iri
    doyle_add_node/3, % +TMS:tms
                      % +Statement:atom
                      % -Node:iri

% TMS ANALYSIS
    doyle_is_in_node/1, % +Node
    doyle_is_out_node/1 % +Node
  ]
).

/** <module> Doyle

The TMS as described in Jon Doyle, 1979, _|A Truth Maintenance System|_.

# Idea

Rational thought is the process of finding reasons for attitudes.

The only _real_ component of thought is the current set of reasons -
the attitudes such as beliefs and desires arise from the set of seasons,
and have no independent existence.

To study rational thought, we whould study justified belief or reasoned
argument, and ignore questions of truth. Truth enters into the study of
extra-psychological rationality.

# Definitions

## Ancestors

For a node, the transitive closure of its *|supporting nodes|*.

The ancestors are related to the nodes that may affect the support status
of the node in any way.

## Conditional-proof (CP) justification

A justification that _subtracts_ the dependencies of some nodes
(the *hypotheses* of the hypothetical argument) from the dependencies
of others (the *conclusion* of the hypothetical argument).

## Foundations

For a node, the transitive closure of its *antecedents*.

The foundations of a node are the nodes involved in the
*|well-founded argument|* for belief in the node.

The foundation is realted to the notion of *|well-foundedness|*.

## Justification-set

For a node, the set of its *justifications*.

## Node

A representation of a *belief*.

A node is believed iff one of its justifications
(in its *|justification-set|*) is *valid*.

## Reason

For a belief _b_, a pair _|<In,Out>|_ of sets of beliefs.

A *|valid reason|* is one whose nodes in the first set are all believed
and whose nodes in the second set are all disbelieved.

## Repercussions

Of a node, the transitive closure of its *|affected-consequences|*.

## Support-list (SL) justification

A justification that _sums_ the dependencies of the referenced nodes.

An SL-justification is *valid* iff all nodes in _in_-list are _in_
and all node in _out_-list are _out_.

Special types of SL-justifications:
    1. *Premise*: empty _in_- and _out_-list. Always *valid*.
    2. *Normal deduction*: non-empty _in_-list and empty _out_-list.
    3. *Assumption*: non-empty out-list.

## Supporting-justification

A justification that is singled out and on whom the validity / invalidity
of the consequence node is based.

## Supporting-nodes

Of a node, the nodes the TMS uses to determine its *|support status|*.

For an _in_ node, the nodes in the _in_- and _out_-lists of its
*|well-supported justification|*.

For an _out_-node, the TMS picks one node from each of its justitifications:
    * For *|SL-justitifications|*, the TMS picks either an _in_-node from the
      _out_-list, or an _out_-node from the _in_-list.
    * For *|CP-justitications|*, the TMS picks either an _in_-node from the
      _out_hypotheses or an _out_-node from the _in_hypotheses.

## Support status

Of a node, whether it is _in_ or _out_.

## Well-founded justification

For every node representing a current belief, a justification that is
somehow lifted out.

Well-founded justifications form a non-circular argument for their node.

Only SL-justifications can be well-founded justifications.

@author Wouter Beek
@version 2012/06, 2013/05, 2013/09, 2013/12-2014/01, 2014/03
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(set_theory)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(pl(pl_control)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_reification)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(tms(tms)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(doyle, 'http://www.wouterbeek.com/doyle.owl#').
:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- dynamic(cp_consequence/2).

:- rdf_meta(doyle_add_argument(+,+,+,r,r)).
:- rdf_meta(doyle_add_justification(+,+,+,+,r,r)).
:- rdf_meta(doyle_add_node(+,+,r)).



doyle_add_argument(TMS, Premises, Rule, Conclusion, J):-
  maplist(doyle_add_node(TMS), [Conclusion|Premises], [Consequence|InNs]),
  doyle_add_justification(TMS, InNs, [], Rule, Consequence, J).

%! doyle_add_justification(
%!   +TMS:atom,
%!   +Ins:list(node),
%!   +Outs:list(node),
%!   +Label:atom,
%!   +Consequence:iri,
%!   -Justification:iri
%! ) is det.
% Step 1: Adding a new justification.

doyle_add_justification(TMS, InNs, OutNs, Label, Consequence, J):-
  % Type checks.
  rdf_graph(TMS),
  maplist(is_node, InNs),
  maplist(is_node, OutNs),
  atom(Label),
  is_node(Consequence),
  var(J),

  % Create the justification.
  tms_create_justification_iri(InNs, OutNs, Label, Consequence, J),
  (
    tms_justification(TMS, J)
  ->
    true
  ;
    % @tbd For now we only support SL-justifications.
    rdf_assert_individual(J, doyle:'SL-Justification', TMS),
    rdfs_assert_label(J, Label, TMS),

    % Add the new justification to the node's justification-set.
    add_justification(TMS, Consequence, J),

    % Add the node to the set of consequences of each of the nodes mentioned
    % in the justification.
    forall(
      member(In, InNs),
      (
        add_consequence(TMS, In, Consequence),
        rdf_assert(J, tms:has_in, In, TMS)
      )
    ),
    forall(
      member(Out, OutNs),
      (
        add_consequence(TMS, Out, Consequence),
        rdf_assert(J, tms:has_out, Out, TMS)
      )
    ),

    % If the justification is a CP-justification, add the node to the
    % CP-consequenct-list of the consequence of the CP-Justification,
    % for use in step 6.
    % @tbd Add CP-justification support.
    if_then(
      is_cp_justification(J),
      assert(cp_consequence(TMS, J))
    ),

    % If the node is _out_, check the justification for validity.
    (
      doyle_is_in_node(Consequence)
    ->
      true
    ;
      doyle_is_out_node(Consequence)
    ->
      (
        is_valid(J)
      ->
        % If valid, proceed to step 2.
        update_belief(TMS, J, Consequence)
      ;
        % If invalid, add to the supporting-nodes either an _out_node
        % from the _in_list, or an _in_ node from the _out_list.
        % @tbd Are we supposed to retract over this?
        (
          member(In, InNs),
          doyle_is_out_node(In),
          add_supporting_node(TMS, Consequence, In)
        ;
          member(Out, OutNs),
          doyle_is_in_node(Out),
          add_supporting_node(TMS, Consequence, Out)
        ), !
      )
    )
  ).

%! update_belief(+TMS:atom, +Justification:iri, +Node:iri) is det.
% Step 2: Updating beliefs required.

update_belief(TMS, J, N):-
  % Check the affected-consequences of the node.
  affected_consequences(N, AffectedConsequences),
  (
    AffectedConsequences == []
  ->
    % If there are none, change the support-status to _in_.
    set_support_status(TMS, N, in),

    % Make the supporting-nodes the sum of the _in_list and _out_list.
    set_supporting_nodes(TMS, valid, N, J)
    % Then stop.
  ;
    % Otherwise, make a list containing the node and its repercussions,
    % record the support-status of each of these nodes.
    % We must collect all the repercussions of the node to avoid constructing
    % circular arguments which use reprecussions of the node in its
    % supposedly well-founded supporting argument.
    repercussions(N, Repercussions),
    % Proceed to Step 3.
    marking_nodes(TMS, [N|Repercussions])
  ).

%! marking_nodes(+TMS:atom, +Nodes:list(iri)) is det.
% Step 3: Marking the nodes.

marking_nodes(TMS, Ns):-
  maplist(is_node, Ns),

  % Mark each node in the list with a support-status of _nil_.
  forall(
    member(N, Ns),
    set_support_status(TMS, N, nil)
  ),
  % Proceed to Step 4.
  evaluating_justifications(TMS, Ns).

%! evaluating_justifications(+TMS:atom, +Nodes:list(node)) is det.
% Step 4: Evaluating the nodes' justifications.

evaluating_justifications(TMS, Ns):-
  maplist(evaluating_justification_set(TMS), Ns).

%! evaluating_justification_set(+TMS:atom, +Node:iri) is det.
% Step 4a: Evaluating the justification-set.

% If the node is either _in_ or _out_, do nothing.
evaluating_justification_set(_TMS, Node):-
  doyle_is_in_node(Node), !.
evaluating_justification_set(_TMS, Node):-
  doyle_is_out_node(Node), !.
evaluating_justification_set(TMS, Node):-
  rdf_graph(TMS),

  % Otherwise, keep picking justifications from the justification-set.
  % First the SL-justifications and then the CP-justifications, checking
  % them for well-founded validity or invalidity (to be defined shortly)
  % until either a valid one is found or the justification-set is exhausted.
  % @tbd The TMS tries justifications in chronological order, oldest first.
  member_of_justification_set(Node, J),
  (
    is_valid(J)
  ->
    % If a valid justification is found, then install it as the
    % supporting-justification.
    % @tbd Convert CP-justifications to SL-justifications.
    set_supporting_justification(TMS, Node, J),

    % Install the supporting-nodes as in Step 2.
    set_supporting_nodes(TMS, valid, Node, J),

    % Mark the node _in_.
    set_support_status(TMS, Node, in),

    % Recursively perform Step 4a for all consequences of the node which have
    % a support-status of _nil_.
    forall(
      (
        rdf(Node, doyle:has_consequence, Consequence, TMS),
        support_status(Consequence, nil)
      ),
      evaluating_justification_set(TMS, Consequence)
    )
  ;
    % If only well-founded invalid justifications are found,
    % mark the node _out_.
    set_support_status(TMS, Node, out),

    % Install its supporting-nodes as in Step 1.
    set_supporting_nodes(TMS, invalid, Node, J),

    % Recursively perform Step 4a for all _nil_-marked consequences
    % of the node.
    forall(
      (
        rdf(Node, doyle:has_consequence, Consequence, TMS),
        support_status(Node, nil)
      ),
      evaluating_justification_set(TMS, Consequence)
    )
  ).



% SUPPORT PREDICATES %

%! add_consequence(+TMS:atom, +Node:iri, +Consequence:iri) is det.
% Adds a consequence node to another node.

add_consequence(TMS, Node, Consequence):-
  maplist(nonvar, [TMS, Node, Consequence]),
  rdf_assert(Node, doyle:has_consequence, Consequence, TMS).

%! add_justification(
%!   +TMS:atom,
%!   +Node:iri,
%!   +Justification:iri
%! ) is det.
% Add a justification to a node's justification set.

add_justification(TMS, Node, J):-
  % Type checks.
  is_node(Node),
  is_justification(J),

  rdf_assert(J, tms:has_consequent, Node, TMS).

%! doyle_add_node(+TMS:atom, +Label:atom, -Node:iri) is det.
% Adds a node.

doyle_add_node(TMS, rdf(S,P,O), N):- !,
  % Create an atomic label.
  dcg_with_output_to(atom(Label), rdf_triple_name(S, P, O)),
  
  % Use the atomic label to determine the node URL.
  tms_create_node_iri(Label, N),
  
  % Create the TMS node as a reified statement.
  rdf_assert_statement(S, P, O, TMS, N),
  
  % @tbd Should we unify `tms:Node` and `rdf:Statement`?
  rdf_assert_individual(N, tms:'Node', TMS),
  
  % Assert the RDFS label.
  rdfs_assert_label(N, Label, TMS),
  
  % Set the default TMS node status.
  set_support_status(TMS, N, out).
doyle_add_node(TMS, Label, N):-
  tms_create_node_iri(Label, N),
  (
    tms_node(TMS, N)
  ->
    true
  ;
    rdf_assert_individual(N, tms:'Node', TMS),
    rdfs_assert_label(N, Label, TMS),
    % The initial support status.
    set_support_status(TMS, N, out)
  ).

%! add_supporting_node(+TMS:atom, +Node:iri, +SupportingNode:iri) is det.

add_supporting_node(TMS, Node, SupportingNode):-
  % Type checks.
  rdf_graph(TMS),
  maplist(is_node, [Node,SupportingNode]),

  rdf_assert(Node, doyle:has_supporting_node, SupportingNode, TMS).

%! affected_consequences(+Node:iri, -AffectedConsequences:ordset(iri)) is det.
% For a node, those consequences of the node which contain the node in
% their set of supporting nodes.
%
% *|Affected consequences|*
% For a node, those consequences of the node which contain the node in
% their set of supporting nodes.

affected_consequences(N, AffectedConsequences):-
  is_node(N),
  aggregate_all(
    set(AffectedConsequence),
    (
      rdf(N, doyle:has_consequence, AffectedConsequence),
      rdf(AffectedConsequence, doyle:has_supporting_node, N)
    ),
    AffectedConsequences
  ).

%! antecedents(+Node:iri, -Antecedents:ordset(iri)) is det.
% An antecedent is a supporting node of a believed node.
%
% *|Antecedents|*
% For an _in_ node, its _|supporting nodes|_.
% For an _out_ node, the empty set.

antecedents(N, Antecedents):-
  doyle_is_in_node(N), !,
  supporting_nodes(N, Antecedents).
antecedents(N, []):-
  doyle_is_out_node(N).

%! assumption_justification(+TMS:atom, ?Assumption:iri) is nondet.
% *|Assumption justification|*
% A _|justification|_ with non-empty _out_-list.

assumption_justification(TMS, J):-
  rdf_graph(TMS),
  rdf(J, tms:has_out, _Out, TMS).

%! assumption_node(+TMS:atom, ?Assumption:iri) is nondet.
% *|Assumption node|*
% A node with a _|non-monotonic justification|_ as _|well-founded support|_,
% i.e. the nodes that explain support status _in_.

assumption_node(TMS, N):-
  rdf_graph(TMS),
  rdfs_individual_of(N, tms:'Node'),
  rdf(N, doyle:supporting_justification, J, TMS),
  assumption_justification(TMS, J).

%! believed_consequences(
%!   +Node:iri,
%!   -BelievedConsequences:ordset(node)
%! ) is det.
% Returns the believed consequences of the given node.
%
% *|Believed consequences|*
% Of a node, its _in_ consequences that have it in their *antecedents*.

believed_consequences(N, BelievedConsequences):-
  is_node(N),
  aggregate_all(
    set(BelievedConsequence),
    (
      rdf(BelievedConsequence, doyle:has_supporting_node, N),
      rdf(N, doyle:has_consequence, BelievedConsequence)
    ),
    BelievedConsequences
  ).

%! believed_repercussions(+Node:iri, -Repercussions:ordset(node)) is det.
% Returns the believed repercussions of the given node.
%
% *|Believed repercussions|*
% For a node, the transitive closure of its believed consequences.

believed_repercussions(N, Repercussions):-
  transitive_closure(believed_consequences, N, Repercussions).

%! consequences(+Node:iri, -Consequences:ordset(node)) is det.
% A consequence of a node is a node which mentions the prior node
% in one of its justifications.
%
% *Consequences*
% Of a node, the nodes for which it occurs in one of their justifications.

consequences(Node, Consequences):-
  is_node(Node),
  aggregate_all(
    set(Consequence),
    rdf(Node, doyle:has_consequence, Consequence),
    Consequences
  ).

%! cp_justification(?CP_Justification) is nondet.

cp_justification(CP_Justification):-
  rdfs_individual_of(CP_Justification, doyle:'CP-Justification').

%! foundations(+Node:iri, -Foundations:ordset(node)) is det.
% Returns the foundations of the given node.
% The foundations are the transitive closure of the antecedents.

foundations(Node, Foundations):-
  transitive_closure(antecedents, Node, Foundations).

%! has_support_status(
%!   +Node:iri,
%!   +SupportStatus:oneof([in,nil,out])
%! ) is semidet.

has_support_status(Node, SupportStatus):-
  support_status(Node, SupportStatus).

doyle_init(TMS):-
  tms:tms_init(TMS),
  rdfs_assert_subclass(doyle:'SL-Justification', tms:'Justification', TMS),
  rdfs_assert_subclass(doyle:'CP-Justification', tms:'Justification', TMS).

%! is_cp_justification(+X) is semidet.

is_cp_justification(J):-
  nonvar(J),
  cp_justification(J).

%! doyle_is_in_node(+Node:iri) is semidet.
% A belief _b_ is _in_ iff a justification _|<In,Out>|_ of _b_ has
% _|in(b')|_ for all _b'_ in _In_ and _|out(b')|_ for all _b'_ in _Out_.
%
% A node with a *valid* justification is _in_.

doyle_is_in_node(Node):-
  has_support_status(Node, in).

%! is_justification(+X) is semidet.

is_justification(J):-
  nonvar(J),
  justification(J).

%! is_node(+Node:iri) is semidet.

is_node(Node):-
  nonvar(Node),
  rdfs_individual_of(Node, tms:'Node').

%! doyle_is_out_node(+Node:iri) is semidet.

doyle_is_out_node(Node):-
  has_support_status(Node, out).

%! is_valid(+Justification:iri) is semidet.
% A valid justification.

is_valid(J):-
  is_justification(J),
  forall(
    rdf(J, tms:has_in, In),
    doyle_is_in_node(In)
  ),
  forall(
    rdf(J, tms:has_out, Out),
    doyle_is_out_node(Out)
  ).

%! justification(?Justification:iri) is nondet.
% *Justification*
% There are two types of justification:
%     1. Support-list (SL)
%     2. Conditional-proof (CP)
%
% The *|external form|* of a justification is only significant for the problem
% solver.
%
% The *|internal form|* of a justification is only significant for the TMS.

justification(CP_Justification):-
  cp_justification(CP_Justification).
justification(SL_Justification):-
  sl_justification(SL_Justification).

%! member_of_cp_justification_set(
%!   ?CP_Justification:iri,
%!   ?Node:iri
%! ) is nondet.

member_of_cp_justification_set(Node, CP_Justification):-
  rdf(CP_Justification, tms:has_consequent, Node),
  rdfs_individual_of(CP_Justification, doyle:'CL-Justification').

%! member_of_justification_set(
%!   ?Justification:iri,
%!   ?Node:iri
%! ) is nondet.

member_of_justification_set(Node, J):-
  member_of_sl_justification_set(Node, J).
member_of_justification_set(Node, J):-
  member_of_cp_justification_set(Node, J).

%! member_of_sl_justification_set(
%!   ?SL_Justification:iri,
%!   ?Node:iri
%! ) is nondet.

member_of_sl_justification_set(SL_Justification, Node):-
  rdf(SL_Justification, tms:has_consequent, Node),
  rdfs_individual_of(SL_Justification, doyle:'SL-Justification').

%! premise(?Justification:iri) is nondet.

premise(J):-
  justification(J),
  \+ rdf(J, tms:has_in, _),
  \+ rdf(J, tms:has_out, _).

%! repercussions(+Node:iri, -Repercussions:ordset(node)) is det.

repercussions(Node, Repercussions):-
  transitive_closure(affected_consequences, Node, Repercussions).

%! doyle_reset(+TMS:atom) is det.

doyle_reset(TMS):-
  retractall(cp_consequence(TMS, _CP_Justification)),
  format(atom(JustificationsFlag), '~w_justifications', [TMS]),
  flag(JustificationsFlag, _OldJustificationsFlag, 2),
  format(atom(NodesFlag), '~w_nodes', [TMS]),
  flag(NodesFlag, _OldNodesFlag, 2),
  rdf_retractall(_, _, _, TMS:_).

%! set_support_status(
%!   +TMS:atom,
%!   +Node:iri,
%!   +SupportStatus:oneof([in,nil,out])
%! ) is det.

set_support_status(TMS, Node, SupportStatus):-
  % Type checking.
  rdf_graph(TMS),
  is_node(Node),
  memberchk(SupportStatus, [in,nil,out]),
  rdf_retractall_string(Node, doyle:has_support_status, _, TMS),
  rdf_assert_string(Node, doyle:has_support_status, SupportStatus, TMS).

%! set_supporting_justification(
%!   +TMS:atom,
%!   +Node:iri,
%!   +SupportingJustification:iri
%! ) is det.

set_supporting_justification(TMS, Node, SupportingJustification):-
  % Type checking.
  rdf_graph(TMS),
  is_node(Node),
  is_justification(SupportingJustification),

  rdf_retractall(Node, doyle:supporting_justification, _, TMS),
  rdf_assert(Node, doyle:supporting_justification, SupportingJustification,
      TMS).

%! set_supporting_nodes(
%!   +TMS:atom,
%!   +Validity:oneof([invalid,valid]),
%!   +Node:iri,
%!   +Justification:iri
%! ) is det.

set_supporting_nodes(TMS, Validity, Node, J):-
  % Type checking.
  rdf_graph(TMS),
  is_node(Node),
  is_justification(J),

  % Remove the old supporting nodes.
  rdf_retractall(Node, doyle:has_supporting_node, _, TMS),

  % Depening on validity
  (
    Validity == valid
  ->
    forall(
      rdf(J, tms:has_in, In, TMS),
      add_supporting_node(TMS, Node, In)
    ),
    forall(
      rdf(J, tms:has_out, Out, TMS),
      add_supporting_node(TMS, Node, Out)
    )
  ;
    Validity == invalid,
    rdf(J, tms:has_in, In, TMS),
    doyle_is_out_node(In)
  ->
    add_supporting_node(TMS, Node, In)
  ;
    Validity == invalid,
    rdf(J, tms:has_out, Out, TMS),
    doyle_is_in_node(Out)
  ->
    add_supporting_node(TMS, Node, Out)
  ).

%! sl_justification(?SL_Justification:iri) is nondet.

sl_justification(SL_Justification):-
  rdfs_individual_of(SL_Justification, doyle:'SL-Justification').

%! support_status(+Node:iri, ?SupportStatus:oneof([in,nil,out])) is nondet.

support_status(Node, SupportStatus):-
  is_node(Node),
  rdf_string(Node, doyle:has_support_status, SupportStatus, _).

%! supporting_nodes(+Node:iri, -SupportingNodes:ordset(node)) is det.

supporting_nodes(Node, SupportingNodes):-
  aggregate_all(
    set(SupportingNode),
    rdf(Node, doyle:has_supporting_node, SupportingNode),
    SupportingNodes
  ).


%! transitive_closure(
%!   +Predicate:atom,
%!   +Input:list(term),
%!   -Outputs:list(term)
%! ) is det.
% Returns the transitive closure of =Predicate= applied to =Input=,
% where `Predicate` is a nondeterministic binary predicate
% between elements and lists of elements.
%
% @arg Predicate The atomic name of a predicate.
% @arg Input Either a term or a list of terms.
% @arg Outputs A list of terms. This is the transitive closure.

transitive_closure(Pred, Input, Outputs):-
  \+ is_list(Input), !,
  transitive_closure(Pred, [Input], Outputs).
transitive_closure(_, [], []):- !.
transitive_closure(Pred, [Input|Inputs1], Outputs1):-
  call(Pred, Input, Intermediaries),
  ord_union(Intermediaries, Inputs1, Inputs2),
  transitive_closure(Pred, Inputs2, Outputs2),
  ord_union(Outputs2, Intermediaries, Outputs1).

