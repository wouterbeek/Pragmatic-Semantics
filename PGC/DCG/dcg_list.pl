:- module(
  dcg_list,
  [
    dcg_list//1, % +Elements:list
    dcg_list//2 % :ElementWriter
                % +Elements:list
  ]
).

/** <module> DCG list

Support for generating lists using DCG rules.

@author Wouter Beek
@version 2014/03
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_meta)).

:- meta_predicate(dcg_list(3,+,?,?)).



dcg_list(L) -->
  dcg_list(pl_term, L).

dcg_list(_, []) --> !, [].
dcg_list(Dcg, [H|T]) -->
  indent(1, dcg_call(Dcg, H)), nl,
  dcg_list(T).

