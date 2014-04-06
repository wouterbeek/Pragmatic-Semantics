:- module(dcg_multi_test, []).

/** <module> DCG multi test

Automated tests for module DCG_MULTI.

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(library(plunit)).



:- begin_tests(dcg_multi).

dcg_multi_example(apetail, 1-1, ['@']).
dcg_multi_example(apetail, 1-2, ['@','@@']).
dcg_multi_example(apetail, _-2, ['','@','@@']).

test(dcg_multi, [forall(dcg_multi_example(DCG, Rep, Atoms)),true]):-
  findall(
    A,
    (
      phrase(dcg_multi(DCG, Rep), Cs),
      atom_codes(A, Cs)
    ),
    As
  ),
  As == Atoms.

%dcg_multi_example('ALPHA', 1-8).

:- end_tests(dcg_multi).

