:- module(ccm_load, [load/0]).

/** <module> CCM load

Loads the CCM context.

@author Wouter Beek
@version 2013/02
*/



load:-
  % Toggle for showing debugging information.
  assert(debug_mode(true)),
  register_namespaces.

register_namespaces:-
  rdf_register_prefix(component, 'http://www.wouterbeek.com/prasem/component.owl#'),
  rdf_register_prefix(component_cloud, 'http://www.wouterbeek.com/prasem/component_cloud.owl#'),
  rdf_register_prefix(expression, 'http://www.wouterbeek.com/prasem/expression.owl#'),
  rdf_register_prefix(point, 'http://www.wouterbeek.com/prasem/point.owl#'),
  rdf_register_prefix(point_cloud, 'http://www.wouterbeek.com/prasem/point_cloud.owl#'),
  rdf_register_prefix(space, 'http://www.wouterbeek.com/prasem/space.owl#').

