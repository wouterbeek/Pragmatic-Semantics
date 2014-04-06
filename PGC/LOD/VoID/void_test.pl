:- module(void_test, []).

:- use_module(rdf(rdf_package)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(void(void_file)).
:- use_module(void(void_tabular)). % Debug tool.

:- initialization(void_test).



void_test:-
  absolute_file_name(
    home('STCN/VoID'),
    File,
    [access(read),file_type(turtle)]
  ),
  void_load(File, Graph),
  void_package_build([], Graph, 'test.tar.bz2').

