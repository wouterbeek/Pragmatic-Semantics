:- module(semweb_mem, []).

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(swi_ide)).

:- use_module(rdf(rdf_graph)).
:- use_module(rdf_file(rdf_serial)).

:- prolog_ide(debug_monitor).
:- debug(mem_triples).

%url('http://www.babelnet.org/data/babelnet-2.0-lemon-URIs.tar.xz').
url('http://epsrc.rkbexplorer.com/models/dump.tgz').


test:-
  url(Url),
  test(Url).

test(Url):-
  rdf_load_any([graph(Graph)], Url),
  rdf_unload_graph_debug(Graph),
  rdf_gc.

