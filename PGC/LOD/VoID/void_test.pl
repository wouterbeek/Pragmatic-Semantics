:- module(void_test, []).

:- use_remote_module(rdf(rdf_package)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(void(void_file)).
:- use_remote_module(void(void_tabular)). % Debug tool.

:- initialization(void_test).



void_test:-
  absolute_file_name(
    home('STCN/VoID'),
    File,
    [access(read),file_type(turtle)]
  ),
  void_load(File, Graph),
  void_package_build([], Graph, 'test.tar.bz2').

