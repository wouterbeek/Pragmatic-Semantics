:- module(
  owl_mat,
  [
    owl_materialize/2 % +FromDir:atom
                      % +ToDir:atom
  ]
).

/** <module> RDF predicates for automated processes

@author Wouter Beek
@version 2013/11-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(os(java_ext)).

:- db_add_novel(user:prolog_file_type(jar, jar)).



owl_materialize(FromDir, ToDir):-
  absolute_file_name(
    iotw('iotw-0.0.1-SNAPSHOT'),
    JAR_File,
    [access(read),file_type(jar)]
  ),
  run_jar(JAR_File, [file(FromDir),file(ToDir)]).

