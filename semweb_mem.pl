:- module(semweb_mem, []).

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(swi_ide)).

:- use_module(rdf(rdf_graph)).
:- use_module(rdf_file(rdf_serial)).

:- prolog_ide(debug_monitor).
:- debug(mem_triples).

:- initialization(test).

url('https://dl.dropboxusercontent.com/s/brxpfdwn4n72c2z/datahub_io.ttl?dl=1&token_hash=AAEd9UWXY3SsIBAILVE-yIH7fuRq-_s8RYFgdEAePb0oSQ').



test:-
  url(Url),
  test(Url).

test(Url):-
  rdf_load([], Graph, Url),
  rdf_unload_graph_deb(Graph),
  rdf_gc.

