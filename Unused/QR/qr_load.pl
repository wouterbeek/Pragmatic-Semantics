:- module(qr_load, [load/0]).

/** <module> QR load

Loads the QR context.

@author Wouter Beek
@version 2013/02
*/



load:-
  register_namespaces.

register_namespaces:-
  rdf_register_prefix(attribute, 'http://www.wouterbeek.com/prasem/attribute.owl#'),
  rdf_register_prefix(configuration, 'http://www.wouterbeek.com/prasem/configuration.owl#'),
  rdf_register_prefix(entity, 'http://www.wouterbeek.com/prasem/entity.owl#'),
  rdf_register_prefix(quantity, 'http://www.wouterbeek.com/prasem/quantity.owl#'),
  rdf_register_prefix(quantity_space, 'http://www.wouterbeek.com/prasem/quantity_space.owl#'),
  rdf_register_prefix(value, 'http://www.wouterbeek.com/prasem/value.owl#').

