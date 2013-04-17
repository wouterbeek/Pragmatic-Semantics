:- module(ce_load, [load/0]).

/** <module> CE load

Loads the Causal Explanation context.

@author Wouter Beek
@version 2013/02
*/



load:-
  % Toggle for showing debugging information.
  assert(debug_mode(true)),
  register_namespaces.

register_namespaces:-
  rdf_register_prefix(ca, 'http://www.wouterbeek.com/prasem/ca.owl#'),
  rdf_register_prefix(explanation, 'http://www.wouterbeek.com/prasem/explanation.owl#'),
  rdf_register_prefix(request, 'http://www.wouterbeek.com/prasem/request.owl#').

