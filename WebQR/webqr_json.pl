:- module(
  webqr_json,
  [
    webqr_json_read/2, % +JSON_In
                       % -Prolog_In
    webqr_json_write/2 % +Prolog_Out
                       % -JSON_Out
  ]
).

:- use_module(library(http/json_convert)).

:- json_object circle(graph:atom, name:(atom|null)) + [type=circle].
:- json_object line(graph:atom, name:(atom|null)) + [type=line].



webqr_json_read(json(Attrs), PrologIn):-
  json_to_prolog(JSON_In, PrologIn).

webqr_json_write(PrologOut, JSON_Out):-
  prolog_to_json(PrologOut, JSON_Out).
