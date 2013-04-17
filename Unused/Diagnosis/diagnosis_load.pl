:- module(diagnosis_load, [load/0]).

/** <module> Diagnosis load

Loads the diagnosis context.

@author Wouter Beek
@version 2013/02
*/



assert_file_search_paths:-
  assert(user:file_search_path(ccm,  diagnosis('CCM'))),
  assert(user:file_search_path(dui,  diagnosis('DUI'))),
  assert(user:file_search_path(gde,  diagnosis('GDE'))),
  assert(user:file_search_path(self, diagnosis('Self'))).

load:-
  % Toggle for showing debugging information.
  assert(debug_mode(true)),
  assert_file_search_paths,
  register_namespaces.

register_namespaces:-
  rdf_register_prefix(diagnosis, 'http://www.wouterbeek.com/prasem/diagnosis.owl#').

