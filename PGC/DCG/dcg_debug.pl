:- module(
  dcg_debug,
  [
    dcg_debug/2 % +Topic:atom
                % :DCG
  ]
).

/** <module> DCG debug

Debug support for DCGs.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_os)).
:- use_module(library(debug)).

:- meta_predicate(dcg_debug(+,//)).



dcg_debug(Topic, _):-
  debugging(Topic, false), !.
dcg_debug(Topic, DCG):-
  DebugStream = user_error,
  dcg_with_output_to(
    DebugStream,
    (dcg_debug_topic(Topic), phrase(DCG), newline)
  ).

dcg_debug_topic(Topic) -->
  bracketed(square, atom(Topic)).
