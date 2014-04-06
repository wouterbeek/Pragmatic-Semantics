:- module(
  dcg_error,
  [
    dcg_catch//3 % :DCG
                 % -Exception:compound
                 % :Recover
  ]
).

/** <module> DCG error

Error and exception handling for DCGs.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(dcg(dcg_meta)).

:- meta_predicate(dcg_catch(//,?,//,?,?)).



dcg_catch(DCG, Exception, Recover, X, Y):-
  catch(
    dcg_call(DCG, X, Y),
    Exception,
    dcg_call(Recover, X, Y)
  ).
