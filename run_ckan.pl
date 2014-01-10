:- [debug].

:- use_module(datasets(data_gov_uk)).

:- use_module(library(swi_ide)).
:- prolog_ide(debug_monitor).

go:-
  thread_create(ckan_to_rdf, _, []).

