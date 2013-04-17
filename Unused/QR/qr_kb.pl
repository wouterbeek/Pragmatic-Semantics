:- module(qr_kb, []).

/** <module> QR KB

QR static KB.

@author Wouter Beek
@version 2012/09.
*/

:- use_module(rdf(rdf_build)).



:-
% ATTRIBUTE
  rdf_assert(attribute:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(attribute:relation, 'attribute relation', ccm),

  rdf_assert(attribute:has_id, rdfs:subPropertyOf, attribute:relation, ccm),
  rdfs_assert_label(attribute:has_id, 'attribute has id', ccm),

  rdf_assert(attribute:has_possible_value, rdfs:subPropertyOf, attribute:relation, ccm),
  rdfs_assert_label(attribute:has_possible_value, 'attribute has possible value', ccm),

% CONFIGURATION
  rdf_assert(entity:has_configuration, rdfs:subPropertyOf, entity:relation, ccm),
  rdfs_assert_label(entity:has_configuration, 'configuration', ccm),
  rdf_assert_datatype(entity:has_configuration, configuration:has_id, integer, 0, ccm),

  rdf_assert(configuration:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(configuration:relation, 'configuration relation', ccm),

  rdf_assert(configuration:has_id, rdfs:subPropertyOf, configuration:relation, ccm),
  rdfs_assert_label(configuration:has_id, 'configuration has ID', ccm),

% ENTITY
  rdf_assert(entity:entity, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(entity:entity, 'entity', ccm),
  rdf_assert_datatype(entity:entity, entity:has_id, integer, 0, ccm),

  rdf_assert(entity:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(entity:relation, 'entity relation', ccm),

  rdf_assert(entity:has_id, rdfs:subPropertyOf, entity:relation, ccm),
  rdfs_assert_label(entity:has_id, 'entity has id', ccm),

  % All attributes should be sub-properties of this.
  % The subject is an entity; the object is an attribute value.
  rdf_assert(entity:has_attribute_value, rdfs:subPropertyOf, entity:relation, ccm),
  rdfs_assert_label(entity:has_attribute_value, 'entity has attribute value', ccm),

  rdf_assert(entity:has_quantity, rdfs:subPropertyOf, entity:relation, ccm),
  rdfs_assert_label(entity:has_quantity, 'entity has quantity', ccm),

% QUANTITY
  rdf_assert(quantity:quantity, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(quantity:quantity, 'quantity', ccm),
  rdf_assert_datatype(quantity:quantity, quantity:has_id, integer, 0, ccm),

  rdf_assert(quantity:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(quantity:relation, 'has relation', ccm),

  rdf_assert(quantity:has_id, rdfs:subPropertyOf, quantity:relation, ccm),
  rdfs_assert_label(quantity:has_id, 'has id', ccm),

  rdf_assert(quantity:has_engine_name, rdfs:subPropertyOf, quantity:relation, ccm),
  rdfs_assert_label(quantity:has_engine_name, 'has engine name', ccm),

  rdf_assert(quantity:has_entity, rdfs:subPropertyOf, quantity:relation, ccm),
  rdfs_assert_label(quantity:has_entity, 'has entity', ccm),

  rdf_assert(quantity:has_expression, rdfs:subPropertyOf, quantity:relation, ccm),
  rdfs_assert_label(quantity:has_expression, 'has expression', ccm),

  rdf_assert(quantity:has_quantity_space, rdfs:subPropertyOf, quantity:relation, ccm),
  rdfs_assert_label(quantity:has_quantity_space, 'has quantity space', ccm),

  rdf_assert(quantity:has_derivative_quantity_space, rdfs:subPropertyOf, quantity:has_quantity_space, ccm),
  rdfs_assert_label(quantity:has_derivative_quantity_space, 'has derivative quantity space', ccm),
  rdf_assert_literal(quantity:has_derivative_quantity_space, expression:has_abbreviation, 'dqs', ccm),

  rdf_assert(quantity:has_magnitude_quantity_space, rdfs:subPropertyOf, quantity:has_quantity_space, ccm),
  rdfs_assert_label(quantity:has_magnitude_quantity_space, 'has quantity space', ccm),
  rdf_assert_literal(quantity:has_magnitude_quantity_space, expression:has_abbreviation, 'mqs', ccm),

% QUANTITY SPACES
  rdf_assert(quantity_space:quantity_space, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(quantity_space:quantity_space, 'quantity space', ccm),
  rdf_assert_datatype(quantity_space:quantity_space, quantity_space:has_id, integer, 0, ccm),

  rdf_assert(quantity_space:magnitude_quantity_space, rdfs:subClassOf, quantity_space:quantity_space, ccm),
  rdfs_assert_label(quantity_space:magnitude_quantity_space, 'magnitude quantity space', ccm),
  rdf_assert_datatype(quantity_space:magnitude_quantity_space, quantity_space:has_id, integer, 1, ccm),

  rdf_assert(quantity_space:derivative_quantity_space, rdfs:subClassOf, quantity_space:quantity_space, ccm),
  rdfs_assert_label(quantity_space:derivative_quantity_space, 'derivative quantity space', ccm),
  rdf_assert_datatype(quantity_space:derivative_quantity_space, quantity_space:has_id, integer, 2, ccm),

  rdf_assert(quantity_space:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(quantity_space:relation, 'quantity space relation', ccm),

  rdf_assert(quantity_space:has_id, rdfs:subPropertyOf, quantity_space:relation, ccm),
  rdfs_assert_label(quantity_space:has_id, 'quantity space has ID', ccm),

  rdf_assert(quantity_space:has_quantity, rdfs:subPropertyOf, quantity_space:relation, ccm),
  rdfs_assert_label(quantity_space:has_quantity, 'quantity space has quantity', ccm),

  rdf_assert(quantity_space:has_quantity_value, rdfs:subPropertyOf, quantity_space:relation, ccm),
  rdfs_assert_label(quantity_space:has_quantity_value, 'quantity space has quantity value', ccm),

  rdf_assert(quantity_space:has_highest_quantity_value, rdfs:subPropertyOf, quantity_space:has_quantity_value, ccm),
  rdfs_assert_label(quantity_space:has_highest_quantity_value, 'quantity space has highest quantity value', ccm),

  rdf_assert(quantity_space:has_lowest_quantity_value, rdfs:subPropertyOf, quantity_space:has_quantity_value, ccm),
  rdfs_assert_label(quantity_space:has_lowest_quantity_value, 'quantity space has lowest quantity value', ccm),

  rdf_assert(quantity_space:has_positive_value, rdfs:subPropertyOf, quantity_space:has_quantity_value, ccm),
  rdfs_assert_label(quantity_space:has_positive_value, 'quantity space has positive value', ccm),

  rdf_assert(quantity_space:has_zero_value, rdfs:subPropertyOf, quantity_space:has_quantity_value, ccm),
  rdfs_assert_label(quantity_space:has_zero_value, 'quantity space has zero value', ccm),

  rdf_assert(quantity_space:has_negative_value, rdfs:subPropertyOf, quantity_space:has_quantity_value, ccm),
  rdfs_assert_label(quantity_space:has_negative_value, 'quantity space has negative value', ccm),

% VALUES
  rdf_assert(value:value, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(value:value, 'value', ccm),

  rdf_assert(value:attribute_value, rdfs:subClassOf, value:value, ccm),
  rdfs_assert_label(value:attribute_value, 'attribute value', ccm),

  rdf_assert(value:quantity_value, rdfs:subClassOf, value:value, ccm),
  rdfs_assert_label(value:quantity_value, 'quantity value', ccm),
  rdf_assert_datatype(value:quantity_value, value:has_id, integer, 0, ccm),


  rdf_assert(value:derivative_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:derivative_quantity_value, 'derivative quantity value', ccm),
  rdf_assert_datatype(value:derivative_quantity_value, value:has_id, integer, 4, ccm),

  rdf_assert(value:magnitude_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:magnitude_quantity_value, 'magnitude quantity value', ccm),
  rdf_assert_datatype(value:magnitude_quantity_value, value:has_id, integer, 5, ccm),


  rdf_assert(value:interval_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:interval_quantity_value, 'interval value', ccm),
  rdf_assert_datatype(value:interval_quantity_value, value:has_id, integer, 6, ccm),

  rdf_assert(value:point_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:point_quantity_value, 'point value', ccm),
  rdf_assert_datatype(value:point_quantity_value, value:has_id, integer, 7, ccm),


  rdf_assert(value:negative_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:negative_quantity_value, 'negative quantity value', ccm),
  rdf_assert_datatype(value:negative_quantity_value, value:has_id, integer, 1, ccm),

  rdf_assert(value:positive_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdfs_assert_label(value:positive_quantity_value, 'positive quantity value', ccm),
  rdf_assert_datatype(value:positive_quantity_value, value:has_id, integer, 2, ccm),

  rdf_assert(value:zero_quantity_value, rdfs:subClassOf, value:quantity_value, ccm),
  rdf_assert(value:zero_quantity_value, rdfs:subClassOf, value:point_quantity_value, ccm),
  rdfs_assert_label(value:zero_quantity_value, 'zero quantity value', ccm),
  rdf_assert_datatype(value:zero_quantity_value, value:has_id, integer, 3, ccm),


  rdf_assert(value:decreasing_derivative_value, rdfs:subClassOf, value:derivative_quantity_value, ccm),
  rdf_assert(value:decreasing_derivative_value, rdfs:subClassOf, value:interval_quantity_value, ccm),
  rdf_assert(value:decreasing_derivative_value, rdfs:subClassOf, value:negative_quantity_value, ccm),
  rdfs_assert_label(value:decreasing_derivative_value, 'decreasing derivative value', ccm),
  rdf_assert_datatype(value:decreasing_derivative_value, value:has_id, integer, 8, ccm),

  rdf_assert(value:increasing_derivative_value, rdfs:subClassOf, value:derivative_quantity_value, ccm),
  rdf_assert(value:increasing_derivative_value, rdfs:subClassOf, value:interval_quantity_value, ccm),
  rdf_assert(value:increasing_derivative_value, rdfs:subClassOf, value:positive_quantity_value, ccm),
  rdfs_assert_label(value:increasing_derivative_value, 'increasing derivative value', ccm),
  rdf_assert_datatype(value:increasing_derivative_value, value:has_id, integer, 9, ccm),

  rdf_assert(value:steady_derivative_value, rdfs:subClassOf, value:derivative_quantity_value, ccm),
  rdf_assert(value:steady_derivative_value, rdfs:subClassOf, value:point_quantity_value, ccm),
  rdf_assert(value:steady_derivative_value, rdfs:subClassOf, value:zero_quantity_value, ccm),
  rdfs_assert_label(value:steady_derivative_value, 'steady derivative value', ccm),
  rdf_assert_datatype(value:steady_derivative_value, value:has_id, integer, 10, ccm),

  rdf_assert(value:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(value:relation, 'value relation', ccm),

  rdf_assert(value:has_attribute, rdfs:subPropertyOf, value:relation, ccm),
  rdfs_assert_label(value:has_attribute, 'value has attribute', ccm),

  rdf_assert(value:has_id, rdfs:subPropertyOf, value:relation, ccm),
  rdfs_assert_label(value:has_id, 'value has ID', ccm),

  rdf_assert(value:has_index, rdfs:subPropertyOf, value:relation, ccm),
  rdfs_assert_label(value:has_index, 'value has index', ccm),

  rdf_assert(value:has_quantity_space, rdfs:subPropertyOf, value:relation, ccm),
  rdfs_assert_label(value:has_quantity_space, 'value has quantity space', ccm),

  rdf_assert(value:has_quantity_value, rdfs:subPropertyOf, value:relation, ccm),
  rdfs_assert_label(value:has_quantity_value, 'value has quantity value', ccm),

  rdf_assert(value:has_higher_quantity_value, rdfs:subPropertyOf, value:has_quantity_value, ccm),
  rdfs_assert_label(value:has_higher_quantity_value, 'value has higher quantity value', ccm).
