:- module(
  self_verb,
  [
    expression_to_sentence_list/2 % +Expression:expression
                                  % -SentenceList:list(atom)
  ]
).

/** <module> Self Verbalizations

Predicates that verbalize Self1993 content.

@author Wouter Beek
@version Sep 2012
*/

:- use_module(ccm(ccm_api)).



expression_to_sentence_list(Expression, [integer, is, Value]):-
  expression_to_argument(Expression, Value).
