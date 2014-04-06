:- module(
  dcg_proof,
  [
    proof//2 % :Options:list(nvpair)
             % +Proof:compound
  ]
).

/** <module> DCG proof

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(option_ext)).
:- use_module(library(option)).

:- meta_predicate(proof(:,+,?,?)).



proof(O1, Proof) -->
  {
    meta_options(is_meta, O1, O2),
    add_default_option(O2, indent, 0, O3)
  },
  proof_inner(O3, Proof).

proof_inner(O1, Proof) -->
  {Proof =.. [Rule,Premises,Conclusion]},

  % Indentation.
  {update_option(O1, indent, succ, I, O2)},
  indent(I),

  % The name of the rule that was used for deduction.
  "[", atom(Rule), "]",

  % Separator between rule name and conclusion.
  space,

  % The conclusion.
  proposition(O1, Conclusion),
  newline,

  % Print premises / subproofs.
  dcg_multi1(proof(O2), Premises).

proposition(O1, Proposition) -->
  {
    option(transformation(Predicate), O1, identity),
    call(Predicate, Proposition, Atom)
  },
  atom(Atom).

