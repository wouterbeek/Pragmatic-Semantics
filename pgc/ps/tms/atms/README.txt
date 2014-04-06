# TMS

## What is a TMS?

A Truth Maintenance System (TMS) is one part of a Problem Solver (PS).
The other part is the Inference Engine (IE).
The IE draws inferences in the task domain.
The TMS handles beliefs, assumptions, and contexts.
The IE sends justifications and assumptions to the TMS.
The TMS sends beliefs and contradictions to the IE.

## History

TMS' were invented in the 1970's.

## Why use a TMS?

The reasons to use a TMS are:
  1. Identifying responsibility for conclusions.
  2. Recovering from inconsistencies.
  3. Mantaining a cache of inferences.
     Replacing chronological backtracking with dependency-directed
     backtracking.
  4. Allows defauklt reasoning to be incorporated.

## Philosophical ramifications of TMS'es

TMS'es enforce a rigid form of rationality.
Removing the belief in a fact requires removing each justification that has
the fact as a consequence.
A TMS keeps many irrelevant justifications around.

## Families of TMSs

|                 | *Simple* | *Complex* |
| *Constraint*    | *Label*  | *Label*   |
| Horn / definite | JTMS     | ATMS      |
| NM              | NMJTMS   |           |
| Clause          | LTMS     | CMS       |

Families:
  1. *|Justification-based TMS (JTMS)|*: simplest. definite clauses. no negation.
  2. *|Logical TMS (LTMS)|*: negation. propositional calculus.
  3. *|Assumption-based TMS (ATMS)|*: makes inferences in multiple
     contexts at once. Horn clauses.
  4. *|Non-monotonic JTMS (NMJTMS)|*: non-monotonic justifications.
     limited utility. hard to use. historically the first TMSs.
  5. *|Clause Management System (CMS)|*: propositional calculus.

Logically, every family is powerfull enough to represent the others.

## Vocabulary for the logical underpinings of a TMS

$ Atom :
See _|atomic formula|_.

$ Atomic formula :
(1) A formula without logical connectives.
(2) A formula that has no subformulas.

$ Clause :
A finite disjunction of literals.

$ Declarative sentence :
A sentence that makes a statement.

$ Definite clause :
A clause with one positive literal.
Subclass of _|Horn clauses|_.

$ Formula :
A word that is part of a formal language.

$ Horn clause :
A _clause_ with zero or one positive literals.

$ Literal :
An atomic formula (or _|positive literal|_)
or its negation (called _|negative literal|_).

$ Proposition :
(1) The meaning or content of a meaningfull _|declerative sentence|_.
Usually the meaning or content is either _true_ or _false_.
(2) The pattern of sounds, marks, or symbols that makes up a
meaningful _|declerative sentence|_.

$ Propositional symbol :
A _proposition_ in the second meaning of the word.

$ Unit clause :
A clause of one literal.


## Vocabulary of a TMS

$ Antecedent :
A TMS node that is part of the condition of a justification.

$ Assumption :
(1) In Prolem Solving, a datum the IE chose to believe without a
    justification.
(2) Philosophically, a current belief one of whose valid _|reasons|_ depends
    _|on a non-current belief|_.
(3) Technically, a _|justification|_ with a non-empty second set of
    antecedent belief nodes.
(4) Technically, a node whose _|well-founded support justification|_ is a
    _|non-monotonic justification|_.
(5) ATMS definition, a node that has itself as an _|environment|_.

$ Assumption justification :
A TMS justification that has a nonempty _outlist_.
Philosophically, the _inlist_ provides the _reason_ for assuming; the
_outlist_ provides the _|authorizing criteria|_ for assuming.
Example: (a) assumed node: ``The weather will be nice.''
(b) ``Be optimistic about the weather.'' (c) there is no reason to assume
``The weather is bad.''

$ Belief :
(1) Of a node: that at least one the justifications in its justification
    set is valid.

$ Belief set :
(1) Philosophically, the four belief states regarding a proposition P:
    1. neither P nor not-P is believed.
    2. Only P is believed.
    3. Only not-P is believed.
    4. P and not-P are believed.

$ Contradiction :
A node ...

$ Current belief :
(1) A TMS node that is _in_.
(2) A TMS node that has a _|valid reason|_.

$ Conditional-proof (CP) justification :
(1) Philosophically, a hypothetical argument.
(2) Technically, a justification that subtracts the dependencies of some
    nodes from the dependencies of others. For instance, the retraction of
    the assumption of P in the implication introduction rule in natural
    deduction, leading to P -> Q, involves a CP-justification that retracts
    the dependency Node(P) from the antecedents of the justification of
    Node(Q).

$ Consequent :
A TMS node that is the consequence of a justification.

$ Contradiction :
(1) A datum that can never hold according to the IE.
(2) An ATMS node with no environment.
(3) A contradiction can never be changed by a TMS.

$ Datum :
(1) A piece of knowledge the IE works with.
(2) Either an _assertion_, a _fact_, an _|inference rule|=, or a _procedure_.

$ Deduction :
(1) Philosophically, a _|reason|_ that is _|valid|_.
(1) Technically, a _|justification|_ with nonempty _|inlist|_ and
    empty _|outlist|_.

$ Denying :
(1) In a TMS, adding a [valid ???] justification for an _out_ belief
that supports an assumption.
(2) Philosophically, refuting a previous assumption.

$ Dependency-directed backtracking :
Analyse the well-founded argument of a contradiction node and locate the
assumptions occurring in the argument, then chaning one of the assumptions,
allowing the contradiction node to be disbelieved.

$ Four-element belief set :
The set consisting of the following belief states: (a) void, (b) \diamond p,
(c) \diamond \lnot p, (d) \diamond p \land \diamond \lnot p.
Similar to Belnap ???.

$ Inlist :
The former set of a _|Support List|_.

$ Innes :
The TMS property of a node that has a _|valid justification|_.

$ Inference Engine (IE) :
(1) That half of the Problem Solver that performs the
domain-dependent inferences.

$ Informant :
(1) A descriptive label of an IE inference.
(2) This label has no effect on the TMS operation.

$ Justification :
(1) The representation of a reason.
(2) The TMS representation of an IE _inference_.
(3) A constraint or condition on the nodes of a TMS.
(4) A strucutre consisting of the following three parts:
(5) consequent, (b) antecedents, (c) informant.
(6) Philosophically, a (valid or invalid) _reason_ to believe something.
(7) Either an _|SL-justification|_ or a _|CP-justification|_.
(8) Logically, a simple propositional _|definite clause|_.

$ Justification set :
The set of (valid or invalid) justifications of a node.

$ Justified belief :
(1) A belief that has a (valid or invalid [???]) justification.
(2) Philosophically, this is not the same as a _true_ belief (or knowledge).

$ Label :
(1) A TMS node's representation of the belief it embodies.
(2) In a JTMS, either _in_ or _out_.
(3) In an ATMS, a consise representation of the environments (or consistent
sets of _assumptions_) in which the node is _in_.

$ Node :
(1) The representation of a belief.
(2) The TMS representation of an IE _datum_.
(3) Logically, a _|propositional symbol|_.

$ Non-monotonic justification :
(1) Philosophically, a _|tentative guess|_.
(2) A justification that bases an argument for a node not only on
    current belief in other nodes, but also on the lack of current belief
    in other nodes.

$ Outlist :
The second set in a _|Support List|_.

$ Outnes :
The TMS property of a node that has no _|valid justification|_.

$ Premise node :
(1) In problem solving, an IE datum that holds universally.
(2) Derived definition, a node with a _|premise justification|_.
(3) Technically, a node that will always be _in_.
(4) ATMS definition, a node that has the _|empty environment|_.
(5) Logically, a _|unit clause|_.

$ Premise justification :
(1) Philosophically, a reason that is always valid.
(2) Technically, a justification that has empty _inlist_ and empty _outlist_.

$ Problem Solver :
(1) An IE, a TMS, and then communication between the two.

$ Reason :
A pair of an ordered pair of beliefs and a reasoned belief, such that the
reasoned belief is _in_ by virtue of the reason only if each belief
in the first ordered set is _in_ and each belief in the second
ordered set is _out_.

$ Reason Maintenance System :
The more correct naming of what is historically called a TMS.
The alternative (and the original [???]) term comed from Doyle.

$ Retracting :
See _denying_.

$ Support-list (SL) :
(1) Technically, pair of mutually exclusive sets of nodes. The former is called the _inlist_; the latter is called the _outlist_.

$ Support-ist (SL) justification :
(1) Definitorilly, justification representation that uses a _|support-list|_.
(2) Technically, justification that is a valid reason for a belief iff every
    node in the _inlist_ is believed and every node in the _outlist_ is not
    believed. Subsets include _|assumption justifications|_ and
    _|premise justifications|_.

$ Support status :
(1) Philosophically, whether a node is believed or not.
(2) Technically, whether a node is _in_ or _out_.

$ Truth :
The classification of statements into _true_ and _false_.
A TMS has nothing to do with this. See _Reason Maintenance System_.
*|Truth and justified belief are not the same.|*

$ Truth maintenance procedure :
The procedure the TMS performs to make revisions in the set of assumptions
in order to resolve contradictions.

$ Truth Maintenance System (TMS) :
The historic name for _|Reason Maintenance Systems|_.

$ Uncertainty :
A TMS that a nodes for a statement and for its negation.

$ Validity :
(1) Of an SL-justification <IN,OUT>: if IN is believed and OUT is not.

$ Well-founded support :
Each currently believes node has one distinguished justification.

## TMS actions

  1. Add a node.
  2. Add a justification.
  3. Mark a node as contradictory.
  4. Run the truth maintenance procedure.
  5. Run the dependency-directed procedure (keeping records of
     the assumptions of the well-founded arguments of contradictory nodes).

## JTMS specifics

## ATMS specifics

To be used when the rate of assumption changes is high compared to the
number of queries about whether a node is in or out. In such cases a JTMS
would spill a lot of time relabeling the nodes that will never be queried.

Querying gains an additional argument: the assumptions under which the
query is performed. Context switches are replaced by explicitly specifying
the assumptions.

Label size grows exponentially in terms of the number of assumptions.

$ Consistent environment :
An environment that is not _nogood_.

$ Context :
A set of nodes that hold in an environment.

$ Contradiction node :
Logically, the negative unit clause.

$ Empty label :
Means that there is no consistent environment for the node.
Means that the node does not hold in any environment.

$ Environment :
A set of assumptions.

$ Innes :
(1) That the node occurs in an environment's context.
(2) Logically, that the propositional symbol of the node is propositionally
derivable from an environment and the set of clauses (i.e. propositional
symbols of non-contradiction nodes, unit clauses of justifications, and
unit clauses of contradiction nodes).

$ Justification :
Logically, a definite clause.

$ Label :
A concise representation of the environments in whose context a node appears.
Due to _monotonicity_, the label contains no subsumed environments.
The label contains no _nogoods_.
An important label is the _|empty label|_.

$ Monotonicity :
If a node belongs to the context of an environment, then it also belongs
to the context of any superset of that environment.

$ Node :
Logically, a propositional symbol.

$ Nogood :
A set of assumptions in which a contradiction holds.

$ Premise :
A node with the _|empty environment|_ as label.


# Temporary section

Reading _|A Truth Maintenance System|_ by Jon Doyle now.

The conventional view: the process of reasoning is the process of
deriving new knowledge from old.

Problems of monotonic reasoning:
  1. Common-sense reasoning: People easily accomodate the correction of
     past assumptions.
  2. Frame problem: The belief of what is current - most of the time -
     changes only minimally accross time.
  3. Control problem: Decide what to do next requires a
     self-referential representation of the reasoner, allowing it to resolve
     inconsistencies by removing beliefs, i.e. by behaving non-monotonically.

An attitude (belief, desire, intent, or action) is rational if
there is some acceptable reason for holding the attitude.

Thought equates the current set of reasons.
Attitudes are byproducts of reasons.

_|P is in|_ if P has an acceptable reason.

_|P is out|_ if P has no reason or has only unacceptable ones.

Observe the assymmetry here: reasons can be P (or not-P) in,
but no reason can ever make P (or not-P) out.










