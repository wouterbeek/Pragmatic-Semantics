:- module(
  db_ext,
  [
    db_add/1, % +New
    db_add_clause/2, % +Head:term
                     % +Body:or([list(term),term])
    db_add_clause/3, % +Module:atom
                     % +Head:term
                     % +Body:or([list(term),term])
    db_add_dcg_rule/2, % +Head:term
                       % +Body:or([list(term),term])
    db_add_dcg_rule/3, % +Module:atom
                       % +Head:term
                       % +Body:or([list(term),term])
    db_add_novel/1, % +New
    db_read/1, % :Goal
    db_replace/2, % +New
                  % +Pattern:list(oneof([e,r]))
    db_replace/2, % +Old
                  % -New
    db_replace_novel/2, % +New
                        % +Pattern:list(oneof([e,r]))
    db_replace_novel/2 % +Old
                       % -New
  ]
).

/** <module> Database extensions

Replacement predicates should specify which parameters
 are to be replaced and which are to stay the same.

For example [1] should replace [2] but not [3].
~~~{.pl}
[1]   rdf_namespace_color(rdf, red)
[2]   rdf_namespace_color(rdf, blue)
[3]   rdf_namespace_color(rdfs, blue)
~~~

@author Wouter Beek
@version 2013/04-2013/05, 2013/08
*/

:- use_module(library(apply)).

:- meta_predicate(db_add(:)).
:- meta_predicate(db_add_novel(:)).
:- meta_predicate(db_read(:)).
:- meta_predicate(db_replace(:,:)).
:- meta_predicate(db_replace_novel(:,:)).



construct_body([Body], Body):- !.
construct_body([X,Y], Body):- !,
  Body =.. [',',X,Y].
construct_body([X|T], Outer):-
  construct_body(T, Inner),
  Outer =.. [',',X,Inner].

db_add(New):-
  assert(New).

%! db_add_clause(+Head:term, Body:or([list(term),term])) is det.
%! db_add_clause(+Module:atom, +Head:term, Body:or([list(term),term])) is det.
% Simplifies the assertion of clauses.

db_add_clause(Head, Body):-
  db_add_clause(user, Head, Body).
db_add_clause(Mod, Head, Body1):-
  (is_list(Body1) -> construct_body(Body1, Body2) ; Body2 = Body1),
  Clause =.. [':-',Head,Body2],
  assert(Mod:Clause).

db_add_dcg_rule(Head, Body):-
  db_add_dcg_rule(user, Head, Body).

db_add_dcg_rule(Mod, Head, Body1):-
  (is_list(Body1) -> construct_body(Body1, Body2) ; Body2 = Body1),
  DCG =.. ['-->',Head,Body2],
  dcg_translate_rule(DCG, Clause),
  assert(Mod:Clause).

%! db_add_novel(+New) is det.
% Asserts the given fact only if it does not already exist.

db_add_novel(New):-
  db_read(New), !.
db_add_novel(New):-
  assert(New).

db_read(Goal):-
  catch(Goal, error(existence_error(_,_),_), fail).

%! db_replace(:Old, +Pattern:list(oneof([e,r]))) is det.
%! db_replace(:Old, :New) is det.
% Replaces at most one asserted fact (if present) with another one.

db_replace(New, _Mod:Pattern):-
  is_list(Pattern), !,
  new_to_old(New, Pattern, Old),
  db_replace(Old, New).
% There is some fact that will be overwritten.
db_replace(Old, New):-
  retract(Old), !,
  assert(New).
% There is nothing to overwrite.
db_replace(_Old, New):-
  assert(New).

%! db_replace_novel(:New, +Pattern:list(oneof([e,r]))) is det.
%! db_replace_novel(:Old, :New) is det.
% In the pattern:
%   * =e= stands for arguments that should be the same
%     as in the given (new) fact.
%   * =r= stands for arguments that should be different
%     in a fact in order to count as an old fact.

db_replace_novel(New, _Mod:Pattern):-
  is_list(Pattern), !,
  new_to_old(New, Pattern, Old),
  db_replace_novel(Old, New).
db_replace_novel(Old, New):-
  db_read(New),
  \+ db_read(Old), !.
db_replace_novel(Old, New):-
  db_replace(Old, New).

match_argument(X, e, X):- !.
match_argument(_, _, _):- !.

%! new_to_old(:NewFact, +Pattern:list(oneof([e,r])), :OldFact) is nondet.
% Returns an old fact that matches the given new fact description,
% using the given pattern specification.
% In the pattern:
%   * =e= stands for arguments that should be the same
%     as in the given (new) fact.
%   * =r= stands for arguments that should be different
%     in a fact in order to count as an old fact.

new_to_old(New, Pattern, Module:Old):-
  strip_module(New, Module, Plain),
  Plain =.. [Predicate|NewArguments],
  maplist(match_argument, NewArguments, Pattern, OldArguments),
  Old =.. [Predicate|OldArguments].

