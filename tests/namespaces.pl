:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/turtle)).

:- initialization(test).

test:-
  test(1, []),
  test(2, [only_known_prefixes(true)]),
  test(3, [user_prefixes(true)]),
  halt.

test(N, O1):-
  absolute_file_name(onto, XML_File, [access(read),extensions([owl])]),
  G = test,
  setup_call_cleanup(
    rdf_load(XML_File, [format(xml),graph(G)]),
    (
      rdf_register_prefix(
        'IIMBDATA',
        'http://oaei.ontologymatching.org/2012/IIMBDATA/en/'
      ),
      rdf_register_prefix(
        'IIMBTBOX',
        'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
      ),
      atomic_list_concat([onto,N], '_', TTL_FileName),
      absolute_file_name(
        TTL_FileName,
        TTL_File,
        [access(write),extensions([ttl])]
      ),
      merge_options([graph(G)], O1, O2),
      rdf_save_turtle(TTL_File, O2)
    ),
    rdf_unload_graph_debug(G)
  ).
