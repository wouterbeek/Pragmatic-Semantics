:- module(
  qr_db,
  [
% ATTRIBUTES
    add_attribute_definition/2, % +AttributeDefinitionName:atom
                                % -AttributeDefinition:attribute_definition

% ATTRIBUTE VALUES
    add_attribute_value/3, % +Attribute:attribute
                           % +AttributeValueName:atom
                           % -AttributeValue:attribute_value

% CONFIGURATIONS
    add_configuration/3, % +ConfigurationDefinition:configuration_definition
                         % +FromEntity:entity
                         % +ToEntity:entity
    add_configuration_definition/2, % +ConfigurationDefinitionName:atom
                                    % -ConfigurationDefinition:configuration_definition

% CORRESPONDENCE
    add_correspondence/5, % +Space:space
                          % +FromQuantityValue:quantity_value
                          % +ToQuantityValue:quantity_value
                          % -Expression:expression
                          % -Point:point

% ENTITIES
    add_entity/3, % +EntityDefinition:entity_definition
                  % +EntityName:atom
                  % -Entity:entity
    add_entity_definition/2, % +EntityDefinitionName:atom
                             % -EntityDefinition:entity

% QUANTITIES
    add_quantity/6, % +QuantityDefinition:quantity_definition
                    % +Entity:entity
                    % +QuantityName:atom
                    % +QuantitySpace:magnitude_quantity_space
                    % +DerivativeQuantitySpace:derivative_quantity_space
                    % -Quantity:quantity
    add_quantity_definition/2, % +QuantityDefinitionName:atom
                               % -QuantityDefinition:quantity_definition

% QUANTITY PROPORTIONALITY
    add_negative_quantity_proportionality/5, % +Space:space
                                             % +FromQuantity:quantity
                                             % +ToQuantity:quantity
                                             % -Expression:expression
                                             % -Point:point
    add_positive_quantity_proportionality/5, % +Space:space
                                             % +FromQuantity:quantity
                                             % +ToQuantity:quantity
                                             % -Expression:expression
                                             % -Point:point

% QUANTITY SPACES
    add_derivative_quantity_space/1, % -DerivativeQuantitySpace:derivative_quantity_space
    add_magnitude_quantity_space/3, % +QuantitySpaceDefinition:quantity_space_definition
                                    % +MagnitudeQuantitySpaceName:atom
                                    % -QuantitySpace:quantity_space
    add_quantity_space_definition/3, % +QuantitySpaceDefinitionName:atom
                                     % +QuantityValueNames:list(atom)
                                     % -QuantitySpaceDefinition:quantity_space_definition

% QUANTITY VALUES
    add_derivative_value/3, % +DerivativeQuantitySpace:derivative_quantity_space
                            % +DerivativeValueName:atom
                            % -DerivativeValue:derivative_quantity_value
    add_quantity_value/3, % +QuantitySpace:quantity_space
                          % +QuantityValueName:atom
                          % -QuantityValue:quantity_value
    add_quantity_value_definition/6 % +QuantitySpaceDefinition:quantity_space_definition
                                    % +QuantityValueType:atom
                                    % +QuantityValueSign:atom
                                    % +QuantityValueDefinitionName:atom
                                    % +QuantityValueDefinitionIndex:number
                                    % -QuantityValueDefinition:quantity_value_definition
  ]
).

/** <module> QR DB

Low-level database interactions for the QR representation.

@author Wouter Beek
@version 2011-2012
*/

:- use_module(ccm(ccm_build)).
:- use_module(generic(list_ext)).
:- use_module(qr(qr_api)).
:- use_module(qr(qr_build)).
:- use_module(rdf(rdf_wread)).
:- use_module(rdf(rdf_build)).



reset_ids:-
  flag(attribute_definition_id, _OldAttributeDefinitionID, 0),
  flag(attribute_value_definition_id, _OldAttributeValueDefinitionID, 0),
  flag(configuration_definition_id, _OldConfigurationDefinitionID, 0),
  flag(entity_id, _OldEntityID, 0),
  flag(entity_definition_id, _OldEntityDefinitionID, 1),
  flag(quantity_id, _OldQuanittyID, 0),
  flag(quantity_definition_id, _OldQuantityDefinitionID, 1),
  flag(quantity_space_id, _OldQuantitySpaceID, 3),
  flag(quantity_space_definition_id, _OldQuantitySpaceDefinitionID, 3),
  flag(quantity_value_id, _OldQuantityValueID, 0),
  flag(quantity_value_definition_id, _OldQuantityValueDefinitionID, 11).



% ATTRIBUTES %

add_attribute_definition(AttributeDefinitionName, AttributeDefinition):-
  % Create the resource.
  flag(attribute_definition_id, ID, ID + 1),
  format(atom(AtomicID), 'd~w', [ID]),
  rdf_global_id(attribute:AtomicID, AttributeDefinition),

  % Assert the subproperty/superproperty relation.
  rdf_assert(
    AttributeDefinition,
    rdfs:subPropertyOf,
    entity:has_attribute_value,
    ccm
  ),

  % Assert the identifier.
  rdf_assert_datatype(AttributeDefinition, attribute:has_id, integer, ID, ccm),

  % Assert the natural language label.
  rdfs_assert_label(AttributeDefinition, AttributeDefinitionName, ccm),

  % Add the attribute values.
  get(@app, currentModel, Model),
  forall(
    build_api:attribute_definition_value(
      Model,
      AttributeDefinitionName,
      AttributeValueName
    ),
    (
      add_attribute_value(
        AttributeDefinition,
        AttributeValueName,
        AttributeValue
      ),
      rdf_assert(
        AttributeDefinition,
        attribute:has_possible_value,
        AttributeValue,
        ccm
      )
    )
  ),

  debug(ccm_db, 'Added attribute definition\t~w', [AttributeDefinitionName]).

add_attribute_value(Attribute, AttributeValueName, AttributeValue):-
  % Create the resource.
  flag(attribute_value_definition_id, ID, ID + 1),
  format(atom(AtomicID), 'vd~w', [ID]),
  rdf_global_id(attribute:AtomicID, AttributeValue),

  % Assert the instance/definition relation.
  rdf_assert(AttributeValue, rdf:type, value:attribute_value, ccm),

  % Assert the natural language label.
  rdfs_assert_label(AttributeValue, AttributeValueName, ccm),

  % Assert the identifier.
  rdf_assert_datatype(AttributeValue, value:has_id, integer, ID, ccm),

  % Assert the relation between the value and its attribute.
  rdf_assert(Attribute, attribute:has_possible_value, AttributeValue, ccm),

  debug(ccm_db, 'Added attribute value\t~w', [AttributeValueName]).



% CONFIGURATIONS %

add_configuration(ConfigurationDefinition, FromEntity, ToEntity):-
  rdf_assert(FromEntity, ConfigurationDefinition, ToEntity, ccm),

  % DEB
  configuration_label(
    FromEntity,
    ConfigurationDefinition,
    ToEntity,
    ConfigurationDefinitionLabel
  ),
  debug(
    ccm_db,
    'Added configuration\t~w',
    [ConfigurationDefinitionLabel]
  ).

add_configuration_definition(
  ConfigurationDefinitionName,
  ConfigurationDefinition
):-
  % Create the resource.
  flag(configuration_definition_id, ID, ID + 1),
  format(atom(AtomicID), 'd~w', [ID]),
  rdf_global_id(configuration:AtomicID, ConfigurationDefinition),

  % Assert the subproperty/superproperty relationship.
  rdf_assert(
    ConfigurationDefinition,
    rdfs:subPropertyOf,
    entity:has_configuration,
    ccm
  ),

  % Assert the identifier.
  rdf_assert_datatype(ConfigurationDefinition, configuration:has_id, integer, ID, ccm),

  % Assert the natural language label.
  rdfs_assert_label(ConfigurationDefinition, ConfigurationDefinitionName, ccm),

  debug(
    ccm_db,
    'Added configuration definition\t~w',
    [ConfigurationDefinitionName]
  ).



% CORRESPONDENCES %

add_correspondence(
  Space,
  FromQuantityValue,
  ToQuantityValue,
  Expression,
  Point
):-
  find_or_add_point(
    Space,
    FromQuantityValue,
    expression:bidirectional_magnitude_quantity_value_correspondence,
    ToQuantityValue,
    Expression,
    Point
  ).



% ENTITIES %

%% add_entity(
%%   +EntityDefinition:entity_definition,
%%   +EntityName:atom,
%%   -Entity:entity
%% ) is det.
% Returns a newly created entity that is of the given definition.
%
% @param EntityDefinition An entity definition.
% @param Entity An entity.

add_entity(EntityDefinition, EntityName, Entity):-
  % Create the resource.
  flag(entity_id, EntityID, EntityID + 1),
  atom_number(AtomEntityID, EntityID),
  rdf_global_id(entity:AtomEntityID, Entity),

  % Assert the identifier.
  rdf_assert_datatype(Entity, entity:has_id, integer, EntityID, ccm),

  % Assert the label. This must be the same as for the definition.
  rdfs_assert_label(Entity, EntityName, ccm),

  % Assert the instance/definition relation.
  rdf_assert(Entity, rdf:type, EntityDefinition, ccm),

  debug(ccm_db, 'Added entity\t~w', [EntityName]).

%% add_entity_definition(
%%   +EntityDefinitionName:atom,
%%   -EntityDefinition:entity_definition
%% ) is det.
% Returns a newly created entity definition with the given name.
%
% @param EntityDefinitionName The atomic name of an entity definition.
% @param EntityDefinition An entity definition.

add_entity_definition(EntityDefinitionName, EntityDefinition):-
  % Create the resource.
  flag(entity_definition_id, EntityDefinitionID, EntityDefinitionID + 1),
  format(atom(AtomicEntityDefinitionID), 'd~w', [EntityDefinitionID]),
  rdf_global_id(entity:AtomicEntityDefinitionID, EntityDefinition),

  % Assert the identifier.
  rdf_assert_datatype(EntityDefinition, entity:has_id, integer, EntityDefinitionID, ccm),

  % Assert the superset/subset relation.
  rdf_assert(EntityDefinition, rdfs:subClassOf, entity:entity, ccm),

  % Assert the natural language label.
  rdfs_assert_label(EntityDefinition, EntityDefinitionName, ccm),

  debug(ccm_db, 'Added entity definition\t~w', [EntityDefinitionName]).



% QUANTITIES %

%% add_quantity(
%%   +QuantityDefinition:quantity_definition,
%%   +Entity:entity,
%%   +QuantityName:atom,
%%   +MagnitudeQuantitySpace:magnitude_quantity_space,
%%   +DerivativeQuantitySpace:derivative_quantity_space,
%%   -Quantity:quantity
%% ) is det.
% Creates a new quantity with the given name, the given quantity
% spaces, and that is of the given entity.
%
% @param QuantityDefinition A quantity definition.
% @param Entity An entity.
% @param QuantityName The atomic name of a quantity.
% @param MagnitudeQuantitySpace A magnitude quantity space.
% @param DerivativeQuantitySpace A derivative quantity space.
% @param Quantity A quantity.

add_quantity(
  QuantityDefinition,
  Entity,
  QuantityName,
  MagnitudeQuantitySpace,
  DerivativeQuantitySpace,
  Quantity
):-
  % Create the resource.
  flag(quantity_id, QuantityID, QuantityID + 1),
  atom_number(AtomicQuantityID, QuantityID),
  rdf_global_id(quantity:AtomicQuantityID, Quantity),

  % Assert the identifier.
  rdf_assert_datatype(Quantity, quantity:has_id, integer, QuantityID, ccm),

  % Assert the instance/definition relation.
  rdf_assert(Quantity, rdf:type, QuantityDefinition, ccm),

  % Assert the natural language label.
  rdfs_assert_label(Quantity, QuantityName, ccm),

  % Assert the relation with the magnitude quantity space.
  rdf_assert(
    Quantity,
    quantity:has_magnitude_quantity_space,
    MagnitudeQuantitySpace,
    ccm
  ),
  rdf_assert(
    MagnitudeQuantitySpace,
    quantity_space:has_quantity,
    Quantity,
    ccm
  ),

  % Assert the relation with the derivative quantity space.
  rdf_assert(
    Quantity,
    quantity:has_derivative_quantity_space,
    DerivativeQuantitySpace,
    ccm
  ),
  rdf_assert(
    DerivativeQuantitySpace,
    quantity_space:has_quantity,
    Quantity,
    ccm
  ),

  % Assert the relation between quantity and entity.
  rdf_assert(Quantity, quantity:has_entity, Entity, ccm),
  rdf_assert(Entity, entity:has_quantity, Quantity, ccm),

  debug(ccm_db, 'Added quantity\t~w', [QuantityName]).

add_quantity_definition(QuantityDefinitionName, QuantityDefinition):-
  % Assert the resource.
  flag(
    quantity_definition_id,
    QuantityDefinitionID,
    QuantityDefinitionID + 1
  ),
  format(atom(AtomicQuantityDefinitionID), 'd~w', [QuantityDefinitionID]),
  rdf_global_id(quantity:AtomicQuantityDefinitionID, QuantityDefinition),

  % Assert the identifier.
  rdf_assert_datatype(QuantityDefinition, quantity:has_id, integer, QuantityDefinitionID, ccm),

  % Assert the flat hierarchy relation.
  rdf_assert(QuantityDefinition, rdfs:subClassOf, quantity:quantity, ccm),

  % Assert the natural language label.
  rdfs_assert_label(QuantityDefinition, QuantityDefinitionName, ccm),

  debug(ccm_db, 'Added quantity definition\t~w', [QuantityDefinitionName]).



% QUANTITY PROPORTIONALITY %

%% add_negative_quantity_proportionality(
%%   +Space:space,
%%   +FromQuantity:quantity,
%%   +ToQuantity:quantity,
%%   -Expression:expression,
%%   -Point:point
%% ) is det.

add_negative_quantity_proportionality(
  Space,
  FromQuantity,
  ToQuantity,
  Expression,
  Point
):-
  find_or_add_point(
    Space,
    FromQuantity,
    expression:negative_quantity_proportionality,
    ToQuantity,
    Expression,
    Point
  ).

%% add_positive_quantity_proportionality(
%%   +Space:space,
%%   +FromQuantity:quantity,
%%   +ToQuantity:quantity,
%%   -Expression:expression,
%%   -Point:point
%% ) is det.

add_positive_quantity_proportionality(
  Space,
  FromQuantity,
  ToQuantity,
  Expression,
  Point
):-
  find_or_add_point(
    Space,
    FromQuantity,
    expression:positive_quantity_proportionality,
    ToQuantity,
    Expression,
    Point
  ).



% QUANTITY SPACES %

add_derivative_quantity_space(DerivativeQuantitySpace):-
  % Create resource.
  flag(quantity_space_id, ID, ID + 1),
  atom_number(AtomicID, ID),
  rdf_global_id(quantity_space:AtomicID, DerivativeQuantitySpace),

  % Assert the identifier.
  rdf_assert_datatype(DerivativeQuantitySpace, quantity_space:has_id, integer, ID, ccm),

  % Assert the instance/definition relation,
  rdf_assert(
    DerivativeQuantitySpace,
    rdf:type,
    quantity_space:derivative_quantity_space,
    ccm
  ),

  % Assert the natural language label.
  rdfs_assert_label(DerivativeQuantitySpace, dqs, ccm),

  % Assert the derivative quantity values.
  maplist(
    add_derivative_value(DerivativeQuantitySpace),
    [decreasing, steady, increasing],
    DerivativeValues
  ),
  assert_quantity_value_links(DerivativeValues),
  DerivativeValues = [FirstDerivativeValue | _DerivativeValues],
  rdf_assert(
    DerivativeQuantitySpace,
    quantity_space:has_lowest_quantity_value,
    FirstDerivativeValue,
    ccm
  ).

add_magnitude_quantity_space(
  QuantitySpaceDefinition,
  MagnitudeQuantitySpaceName,
  MagnitudeQuantitySpace
):-
  % Create the resource.
  flag(quantity_space_id, QuantitySpaceID, QuantitySpaceID + 1),
  atom_number(AtomicQuantitySpaceID, QuantitySpaceID),
  rdf_global_id(quantity_space:AtomicQuantitySpaceID, MagnitudeQuantitySpace),

  % Assert the identifier.
  rdf_assert_datatype(MagnitudeQuantitySpace, quantity_space:has_id, integer, QuantitySpaceID, ccm),

  % Assert the natural language label.
  rdfs_assert_label(MagnitudeQuantitySpace, MagnitudeQuantitySpaceName, ccm),

  % Assert the instance/definition relation.
  rdf_assert(
    MagnitudeQuantitySpace,
    rdf:type,
    QuantitySpaceDefinition,
    ccm
  ),
  findall(
    QuantityValueDefinitionName,
    (
      quantity_space_definition_quantity_value_definition(
        QuantitySpaceDefinition,
        QuantityValueDefinition
      ),
      quantity_value_definition_label(
        QuantityValueDefinition,
        QuantityValueDefinitionName
      )
    ),
    QuantityValueDefinitionNames
  ),
  maplist(
    add_quantity_value(MagnitudeQuantitySpace),
    QuantityValueDefinitionNames,
    QuantityValues
  ),

  assert_quantity_value_links(QuantityValues),
  first(QuantityValues, FirstQuantityValue),
  rdf_assert(
    MagnitudeQuantitySpace,
    quantity_space:has_lowest_quantity_value,
    FirstQuantityValue,
    ccm
  ),
  last(QuantityValues, LastQuantityValue),
  rdf_assert(
    MagnitudeQuantitySpace,
    quantity_space:has_highest_quantity_value,
    LastQuantityValue,
    ccm
  ).

add_quantity_space_definition(
  QuantitySpaceDefinitionName,
  QuantityValueNames,
  QuantitySpaceDefinition
):-
  % Create the resource.
  flag(
    quantity_space_definition_id,
    QuantitySpaceDefinitionID,
    QuantitySpaceDefinitionID + 1
  ),
  format(
    atom(AtomicQuantitySpaceDefinitionID),
    'd~w',
    [QuantitySpaceDefinitionID]
  ),
  rdf_global_id(
    quantity_space:AtomicQuantitySpaceDefinitionID,
    QuantitySpaceDefinition
  ),

  % Assert the identifier.
  rdf_assert_datatype(QuantitySpaceDefinition, quantity_space:has_id, integer, QuantitySpaceDefinitionID, ccm),

  % Assert the position in the flat hierarchy.
  rdf_assert(
    QuantitySpaceDefinition,
    rdfs:subClassOf,
    quantity_space:magnitude_quantity_space,
    ccm
  ),

  % Assert the natural language label.
  rdfs_assert_label(QuantitySpaceDefinition, QuantitySpaceDefinitionName, ccm),

  % Assert the quantity value definitions.
  findall(
    QuantityValueDefinition,
    (
      nth0(
        QuantityValueIndex,
        QuantityValueNames,
        QuantityValueType/QuantityValueSign/QuantityValueName
      ),
      find_or_add_quantity_value_definition(
        QuantitySpaceDefinition,
        QuantityValueType,
        QuantityValueSign,
        QuantityValueName,
        QuantityValueIndex,
        QuantityValueDefinition
      )
    ),
    QuantityValueDefinitions
  ),
  assert_quantity_value_links(QuantityValueDefinitions).

assert_quantity_value_links([]):-
  !.
assert_quantity_value_links([_QuantityValue]):-
  !.
assert_quantity_value_links([QuantityValue1, QuantityValue2]):-
  rdf_assert(
    QuantityValue1,
    value:has_higher_quantity_value,
    QuantityValue2,
    ccm
  ),
  !.
assert_quantity_value_links(
  [QuantityValue1, QuantityValue2 | QuantityValues]
):-
  assert_quantity_value_links([QuantityValue1, QuantityValue2]),
  assert_quantity_value_links([QuantityValue2 | QuantityValues]).



% QUANTITY VALUES %

add_derivative_value(
  DerivativeQuantitySpace,
  DerivativeValueName,
  DerivativeValue
):-
  % Create the resource.
  flag(quantity_value_id, DerivativeValueID, DerivativeValueID + 1),
  atom_number(AtomicDerivativeValueID, DerivativeValueID),
  rdf_global_id(value:AtomicDerivativeValueID, DerivativeValue),

  (
    DerivativeValueName == decreasing
  ->
    rdf_assert(DerivativeValue, rdf:type, value:decreasing_derivative_value, ccm),
    rdf_assert(DerivativeQuantitySpace, quantity_space:has_negative_value, DerivativeValue, ccm),
    rdf_assert_datatype(DerivativeValue, value:has_index, integer, 0, ccm)
  ;
    DerivativeValueName == increasing
  ->
    rdf_assert(DerivativeValue, rdf:type, value:increasing_derivative_value, ccm),
    rdf_assert(DerivativeQuantitySpace, quantity_space:has_positive_value, DerivativeValue, ccm),
    rdf_assert_datatype(DerivativeValue, value:has_index, integer, 2, ccm)
  ;
    DerivativeValueName == steady
  ->
    rdf_assert(DerivativeValue, rdf:type, value:steady_derivative_value, ccm),
    rdf_assert(DerivativeQuantitySpace, quantity_space:has_zero_value, DerivativeValue, ccm),
    rdf_assert_datatype(DerivativeValue, value:has_index, integer, 1, ccm)
  ;
    % This should never be the case.
    fail
  ),
  rdfs_assert_label(DerivativeValue, DerivativeValueName, ccm),
  rdf_assert(
    DerivativeValue,
    value:has_quantity_space,
    DerivativeQuantitySpace,
    ccm
  ),
  rdf_assert_datatype(DerivativeValue, value:has_id, integer, DerivativeValueID, ccm).

add_quantity_value(QuantitySpace, QuantityValueName, QuantityValue):-
  % Create the resource.
  flag(quantity_value_id, QuantityValueID, QuantityValueID + 1),
  atom_number(AtomicQuantityValueID, QuantityValueID),
  rdf_global_id(value:AtomicQuantityValueID, QuantityValue),

  % Assert the quantity value as an instance of a quantity value definition.
  quantity_space_definition_quantity_space(
    QuantitySpaceDefinition,
    QuantitySpace
  ),
  find_quantity_value_definition(
    QuantitySpaceDefinition,
    QuantityValueName,
    QuantityValueDefinition
  ),!,
  rdf_assert(QuantityValue, rdf:type, QuantityValueDefinition, ccm),

  % Assert the identifier.
  rdf_assert_datatype(QuantityValue, value:has_id, integer, QuantityValueID, ccm),

  % Assert the index.
  rdf_datatype(QuantityValueDefinition, value:has_index, integer, QuantityValueIndex, ccm),!,
  rdf_assert_datatype(QuantityValue, value:has_index, integer, QuantityValueIndex, ccm),

  % Assert the natural language label.
  rdfs_assert_label(QuantityValue, QuantityValueName, ccm),

  % Assert the relations between quantity value and quantity space.
  rdf_assert(QuantityValue, value:has_quantity_space, QuantitySpace, ccm),
  % Assert the zero value for the quantity space, if appropriate.
  (
    zero_quantity_value_definition(QuantityValueDefinition)
  ->
    rdf_assert(QuantityValue, rdf:type, value:zero_quantity_value, ccm),
    rdf_assert(
      QuantitySpace,
      quantity_space:has_zero_value,
      QuantityValue,
      ccm
    )
  ;
    negative_quantity_value_definition(QuantityValueDefinition)
  ->
    rdf_assert(QuantityValue, rdf:type, value:negative_quantity_value, ccm),
    rdf_assert(
      QuantitySpace,
      quantity_space:has_negative_value,
      QuantityValue,
      ccm
    )
  ;
    positive_quantity_value_definition(QuantityValueDefinition)
  ->
    rdf_assert(QuantityValue, rdf:type, value:positive_quantity_value, ccm),
    rdf_assert(
      QuantitySpace,
      quantity_space:has_positive_value,
      QuantityValue,
      ccm
    )
  ;
    % When there is no zero point, we do not know the sign of the quantity
    % values and use the more general (and less descriptive) property.
    rdf_assert(
      QuantitySpace,
      quantity_space:has_quantity_value,
      QuantityValue,
      ccm
    )
  ).

add_quantity_value_definition(
  QuantitySpaceDefinition,
  QuantityValueDefinitionType,
  QuantityValueDefinitionSign,
  QuantityValueDefinitionName,
  QuantityValueDefinitionIndex,
  QuantityValueDefinition
):-
  % Create the resource.
  flag(
    quantity_value_definition_id,
    QuantityValueDefinitionID,
    QuantityValueDefinitionID + 1
  ),
  format(
    atom(AtomicQuantityValueDefinitionID),
    'd~w',
    [QuantityValueDefinitionID]
  ),
  rdf_global_id(
    value:AtomicQuantityValueDefinitionID,
    QuantityValueDefinition
  ),

  % Add the index.
  rdf_assert_datatype(QuantityValueDefinition, value:has_index, integer, QuantityValueDefinitionIndex, ccm),

  rdf_assert(
    QuantityValueDefinition,
    rdfs:subClassOf,
    value:magnitude_quantity_value,
    ccm
  ),
  % Interval or point.
  (
    QuantityValueDefinitionType == interval
  ->
    rdf_assert(
      QuantityValueDefinition,
      rdfs:subClassOf,
      value:interval_quantity_value,
      ccm
    )
  ;
    QuantityValueDefinitionType == point
  ->
    rdf_assert(
      QuantityValueDefinition,
      rdfs:subClassOf,
      value:point_quantity_value,
      ccm
    )
  ;
    % This should never be the case.
    fail
  ),

  % Natural language label.
  rdfs_assert_label(QuantityValueDefinition, QuantityValueDefinitionName, ccm),

  % Add the link to the quantity space definition.
  rdf_assert(
    QuantityValueDefinition,
    value:has_quantity_space,
    QuantitySpaceDefinition,
    ccm
  ),

  % Add the identifier.
  rdf_assert_datatype(QuantityValueDefinition, value:has_id, integer, QuantityValueDefinitionID, ccm),

  (
    QuantityValueDefinitionSign == zero
  ->
    rdf_assert(
      QuantityValueDefinition,
      rdfs:subClassOf,
      value:zero_quantity_value,
      ccm
    ),
    rdf_assert(
      QuantitySpaceDefinition,
      quantity_space:has_zero_value,
      QuantityValueDefinition,
      ccm
    )
  ;
    QuantityValueDefinitionSign == negative
  ->
    rdf_assert(
      QuantityValueDefinition,
      rdfs:subClassOf,
      value:negative_quantity_value,
      ccm
    ),
    rdf_assert(
      QuantitySpaceDefinition,
      quantity_space:has_negative_value,
      QuantityValueDefinition,
      ccm
    )
  ;
    QuantityValueDefinitionSign == positive
  ->
    rdf_assert(
      QuantityValueDefinition,
      rdfs:subClassOf,
      value:positive_quantity_value,
      ccm
    ),
    rdf_assert(
      QuantitySpaceDefinition,
      quantity_space:has_positive_value,
      QuantityValueDefinition,
      ccm
    )
  ;
    % When there is no zero point, we do not know the sign of the quantity
    % value definitions and use the more general
    % (and less descriptive) property of being a quantity value.
    rdf_assert(
      QuantitySpaceDefinition,
      quantity_space:has_quantity_value,
      QuantityValueDefinition,
      ccm
    )
  ).
