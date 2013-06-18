:- module(
  qr_build,
  [
% ATTRIBUTE DEFINITIONS
    find_attribute_definition/2, % +AttributeDefinitionName:atom
                                 % -AttributeDefinition:attribute_definition
    find_or_add_attribute_definition/2, % +AttributeDefinitionName:atom
                                        % -AttributeDefinition:attribute_definition

% ATTRIBUTE VALUES
    find_attribute_value/3, % +AttributeDefinition:attribute_definition
                            % +AttributeValueName:atom
                            % -AttributeValue:attribute_value
    find_or_add_attribute_value/3, % +AttributeDefinition:attribute_definition
                                   % +AttributeValueName:atom
                                   % -AttributeValue:attribute_value

% CONFIGURATIONS
    find_or_add_configuration/3, % +ConfigurationDefinition:configuration_definition
                                 % +FromEntityName:atom
                                 % +ToEntityName:atom
    find_or_add_configuration_definition/2, % +Name:atom
                                            % -ConfigurationDefinition:configuration_definition

% DERIVATIVE VALUES
    find_derivative_value/3, % +DerivaitveQuantitySpace:derivative_quantity_space
                             % +DerivativeValueName:atom
                             % -DerivativeValue:derivative_quantity_value
    find_or_add_derivative_value/3, % +DerivativeQuantitySpace:derivative_quantity_space
                                    % +DerivativeValueName:atom
                                    % -DerivativeValue:derivative_quantity_value

% ENTITIES
    find_or_add_entity/3, % +EntityDefinition:entity_definition
                          % +EntityName:atom
                          % -Entity:entity

% ENTITY DEFINITIONS
    find_or_add_entity_definition/2, % +EntityDefinitionName:atom
                                     % -EntityDefinition:entity_definition

% QUANTITIES
    find_or_add_quantity/6, % +QuantityDefinition:quantity_definition
                            % +Entity:entity
                            % +QuantityName:atom
                            % +MagnitudeQuantitySpace:magnitude_quantity_space
                            % +DerivativeQuantitySpace:derivative_quantity_space
                            % -Quantity:quantity
    find_or_add_quantity_definition/2, % +QuantityDefinitionName:atom
                                       % -QuantityDefinition:quantity_definition

% QUANTITY SPACES
    find_derivative_quantity_space/3, % +Entity:entity
                                      % +QuantityName:atom
                                      % -DerivativeQuantitySpace:derivative_quantity_space
    find_magnitude_quantity_space/4, % +MagnitudeQuantitySpaceName:atom
                                     % +Entity:entity
                                     % +QuantityName:atom
                                     % -MagnitudeQuantitySpace:magnitude_quantity_space
    find_or_add_derivative_quantity_space/3, % +Entity:entity
                                             % +QuantityName:atom
                                             % -DerivativeQuantitySpace:derivative_quantity_space
    find_or_add_magnitude_quantity_space/4, % +MagnitudeQuantitySpaceName:atom
                                            % +Entity:entity
                                            % +QuantityName:atom
                                            % -MagnitudeQuantitySpace:magnitude_quantity_space
    find_or_add_quantity_space_definition/3, % +QuantitySpaceDefinitionName:atom
                                             % +QuantityValueNames:list(atom/atom/atom)
                                             % -QuantitySpaceDefinition:quantity_space_definition

% QUANTITY VALUES
    find_or_add_quantity_value/3, % +QuantitySpace:quantity_space
                                  % +QuantityValueName:atom
                                  % -QuantityValue:quantity_value
    find_or_add_quantity_value_definition/6, % +QuantitySpaceDefinition:quantity_space_definition
                                             % +QuantityValueType:atom
                                             % +QuantityValueSign:atom
                                             % +QuantityValueDefinitionName:atom
                                             % +QuantityValueIndex:number
                                             % -QuantityValueDefinition:quantity_value_definition
    find_quantity_value_definition/3 % +QuantitySpaceDefinition:quantity_space_definition
                                     % +QuantityValueDefinitionName:atom
                                     % -QuantityValueDefinition:quantity_value_definition
  ]
).

/** <module> QR BUILD

# Entities

These are concepts that do not have properties that change over time.
The a-temporal properties, that entities may have, are called attributes.

Entities have definitions in a hierarchy of arbitrary depth.
An child entity has exactly one parent.
A parent entity can have any number of children.

An entity and its definition must always have the same name.
Different entities that cannot be lifted out are different by their being
different (identifiers).
Different entities that can be lifted out have at least one different
property (attribute or quantity) in addition to this.

# Quantities

These are concepts that change over time.

Each quantitie belongs to exactly one entity or agent.
The same quantity may belong to any entity or agent.
The same quantities may not belong to the same entity or agent.

Quantities have definitions in a flat hierarchy.

A quantity is lifted out by (1) its entity and (2) its name.

A quantity has exactly one magnitude quantity space.
A quantity has exactly one derivative quantity space.

# Quantity spaces

A quantity space is a reified ordered set of value labels,
called quantity values.

A quantity space has at least one quantity value.

A quantity space is lifted out by (1) its quantity, (2) its quantity's
entity, and (3) its name.

@author Wouter Beek
@version 2012/09
*/

:- use_module(qr(qr_api)).
:- use_module(qr(qr_db)).



% ATTRIBUTE DEFINITIONS %

find_attribute_definition(AttributeDefinitionName, AttributeDefinition):-
  rdfs_subproperty_of(AttributeDefinition, entity:has_attribute_value),
  rdfs_label(AttributeDefinition, AttributeDefinitionName),
  !.

find_or_add_attribute_definition(
  AttributeDefinitionName,
  AttributeDefinition
):-
  find_attribute_definition(AttributeDefinitionName, AttributeDefinition),
  !.
find_or_add_attribute_definition(
  AttributeDefinitionName,
  AttributeDefinition
):-
  add_attribute_definition(AttributeDefinitionName, AttributeDefinition).



% ATTRIBUTE VALUES %

find_attribute_value(
  AttributeDefinition,
  AttributeValueName,
  AttributeValue
):-
  rdf(AttributeDefinition, attribute:has_possible_value, AttributeValue, ccm),
  rdfs_label(AttributeValue, AttributeValueName),
  !.

find_or_add_attribute_value(
  AttributeDefinition,
  AttributeValueName,
  AttributeValue
):-
  find_attribute_value(
    AttributeDefinition,
    AttributeValueName,
    AttributeValue
  ),
  !.
find_or_add_attribute_value(
  AttributeDefinition,
  AttributeValueName,
  AttributeValue
):-
  add_attribute_value(
    AttributeDefinition,
    AttributeValueName,
    AttributeValue
  ).



% CONFIGURATIONS

find_configuration(ConfigurationDefinition, FromEntityName, ToEntityName):-
  configuration_definition(ConfigurationDefinition),
  qr_base:find_or_add_entity_eng(FromEntityName, FromEntity),
  qr_base:find_or_add_entity_eng(ToEntityName, ToEntity),
  rdf(FromEntity, ConfigurationDefinition, ToEntity, ccm).

find_or_add_configuration(
  ConfigurationDefinition,
  FromEntityName,
  ToEntityName
):-
  find_configuration(ConfigurationDefinition, FromEntityName, ToEntityName),
  !.
find_or_add_configuration(
  ConfigurationDefinition,
  FromEntityName,
  ToEntityName
):-
  qr_base:find_or_add_entity_eng(FromEntityName, FromEntity),
  qr_base:find_or_add_entity_eng(ToEntityName, ToEntity),
  add_configuration(ConfigurationDefinition, FromEntity, ToEntity).

find_or_add_configuration_definition(Name, ConfigurationDefinition):-
  configuration_definition_label(ConfigurationDefinition, Name),
  !.
find_or_add_configuration_definition(Name, ConfigurationDefinition):-
  add_configuration_definition(Name, ConfigurationDefinition).



% DERIVATIVE VALUES %

find_derivative_value(
  DerivativeQuantitySpace,
  DerivativeValueName,
  DerivativeValue
):-
  rdfs_individual_of(DerivativeValue, value:derivative_quantity_value),
  quantity_space_quantity_value(DerivativeQuantitySpace, DerivativeValue),
  rdfs_label(DerivativeValue, DerivativeValueName),
  !.

find_or_add_derivative_value(
  DerivativeQuantitySpace,
  DerivativeValueName,
  DerivativeValue
):-
  find_derivative_value(
    DerivativeQuantitySpace,
    DerivativeValueName,
    DerivativeValue
  ),
  !.
find_or_add_derivative_value(
  DerivativeQuantitySpace,
  DerivativeValueName,
  DerivativeValue
):-
  add_derivative_value(
    DerivativeQuantitySpace,
    DerivativeValueName,
    DerivativeValue
  ).



% ENTITIES %

%% find_or_add_entity(
%%   +EntityDefinition:entity_definition,
%%   +EntityName:atom,
%%   -Entity:entity
%% ) is det.
% Returns an existing or a newly created entity with the given name.
%
% @param EntityDefinition An entity definition.
% @param EntityName The atomic name of an entity.
% @param Entity An entity.

find_or_add_entity(EntityDefinition, EntityName, Entity):-
  entity_definition_entity(EntityDefinition, Entity),
  entity_name(Entity, EntityName),
  !.
find_or_add_entity(EntityDefinition, EntityName, Entity):-
  add_entity(EntityDefinition, EntityName, Entity).



% ENTITY DEFINITIONS %

%% find_or_add_entity_definition(
%%   +EntityDefinitionName:atom,
%%   -EntityDefinition:entity_definition
%% ) is det.
% Returns an existing or a newly created entity definition with the
% given name.
%
% @param EntityDefinitionName The atomic name of an entity definition.
% @param EntityDefinition An entity definition.

find_or_add_entity_definition(EntityDefinitionName, EntityDefinition):-
  entity_definition_label(EntityDefinition, EntityDefinitionName),
  !.
find_or_add_entity_definition(EntityDefinitionName, EntityDefinition):-
  add_entity_definition(EntityDefinitionName, EntityDefinition).



% QUANTITIES %

%% find_or_add_quantity(
%%   +QuantityDefinition:quantity_definition,
%%   +Entity:entity,
%%   +QuantityName:atom,
%%   +MagnitudeQuantitySpace:magnitude_quantity_space,
%%   +DerivativeQuantitySpace:derivative_quantity_space,
%%   -Quantity:quantity
%% ) is det.
% Returns the newly created or existing quantity of the given entity,
% with the given name and quantity space name.
% The entity is necessary, since the height of a building is different
% from the height of a column of liquid.
% The name of the quantity space is important, since a dedicated quantity
% space is created for each quantity; because a quantity may have multiple
% allowed quantity spaces.
%
% @param QuantityDefinition A quantity definition.
% @param Entity An entity.
% @param QuantityName The atomic name of a quantity.
% @param MagnitudeQuantitySpace A magnitude quantity space.
% @param DerivativeQuantitySpace A derivative quantity space.
% @param Quantity A quantity.

find_or_add_quantity(
  _QuantityDefinition,
  Entity,
  QuantityName,
  _MagnitudeQuantitySpace,
  _DerivativeQuantitySpace,
  Quantity
):-
  quantity(Entity, QuantityName, Quantity),
  !.
find_or_add_quantity(
  QuantityDefinition,
  Entity,
  QuantityName,
  MagnitudeQuantitySpace,
  DerivativeQuantitySpace,
  Quantity
):-
  add_quantity(
    QuantityDefinition,
    Entity,
    QuantityName,
    MagnitudeQuantitySpace,
    DerivativeQuantitySpace,
    Quantity
  ).

%% find_or_add_quantity_definition(
%%   +QuantityDefinitionName:atom,
%%   -QuantityDefinition:quantity_definition
%% ) is det.
% Returns the newly created or existing quantity definition with the given
% name.
%
% @param QuantityDefinitionName The atomic name of a quantity defintion.
% @param QuantityDefinition A quantity definition.

find_or_add_quantity_definition(QuantityDefinitionName, QuantityDefinition):-
  quantity_definition_label(QuantityDefinition, QuantityDefinitionName),
  !.
find_or_add_quantity_definition(QuantityDefinitionName, QuantityDefinition):-
  add_quantity_definition(QuantityDefinitionName, QuantityDefinition).



% QUANTITY SPACE %

find_derivative_quantity_space(
  Entity,
  QuantityName,
  DerivativeQuantitySpace
):-
  quantity(Entity, QuantityName, Quantity),
  quantity_derivative_quantity_space(Quantity, DerivativeQuantitySpace).

%% find_magnitude_quantity_space(
%%   +MagnitudeQuantitySpaceName:atom,
%%   +Entity:entity,
%%   +QuantityName:atom,
%%   -MagnitudeQuantitySpace:magnitude_quantity_space
%% ) is semidet.
% Returns the quantity space with the given name, belonging to the
% given quantity and entity, if it exists. Fauls otherwise.
%
% @param MagnitudeQuantitySpaceName The atomic name of a magnitude
%        quantity space.
% @param Entity An entity.
% @param QuantityName The atomic name of a quantity.
% @param MagnitudeQuantitySpace A magnitude quantity space.

find_magnitude_quantity_space(
  MagnitudeQuantitySpaceName,
  Entity,
  QuantityName,
  MagnitudeQuantitySpace
):-
  entity_quantity(Entity, Quantity),
  quantity_name(Quantity, QuantityName),
  quantity_quantity_space(Quantity, MagnitudeQuantitySpace),
  quantity_space_label(MagnitudeQuantitySpace, MagnitudeQuantitySpaceName),
  !.

find_or_add_derivative_quantity_space(
  Entity,
  QuantityName,
  DerivativeQuantitySpace
):-
  find_derivative_quantity_space(
    Entity,
    QuantityName,
    DerivativeQuantitySpace
  ),
  !.
find_or_add_derivative_quantity_space(
  _Entity,
  _QuantityName,
  DerivativeQuantitySpace
):-
  add_derivative_quantity_space(DerivativeQuantitySpace).

find_or_add_magnitude_quantity_space(
  MagnitudeQuantitySpaceName,
  Entity,
  QuantityName,
  MagnitudeQuantitySpace
):-
  find_magnitude_quantity_space(
    MagnitudeQuantitySpaceName,
    Entity,
    QuantityName,
    MagnitudeQuantitySpace
  ),
  !.
find_or_add_magnitude_quantity_space(
  MagnitudeQuantitySpaceName,
  _Entity,
  _QuantityName,
  MagnitudeQuantitySpace
):-
  quantity_space_definition_label(
    QuantitySpaceDefinition,
    MagnitudeQuantitySpaceName
  ),
  add_magnitude_quantity_space(
    QuantitySpaceDefinition,
    MagnitudeQuantitySpaceName,
    MagnitudeQuantitySpace
  ).

% @param QuantityValueNames A list of triples of atoms representing the
%        following things:
%        1. The type of a quantity value. Either 'interval' or 'point'.
%        2. The sign of a quantity value. Either 'negative', 'positive',
%           or 'zero'.
%        3. The atomic name of a quantity value.

find_or_add_quantity_space_definition(
  QuantitySpaceDefinitionName,
  _QuantityValueNames,
  QuantitySpaceDefinition
):-
  quantity_space_definition_label(
    QuantitySpaceDefinition,
    QuantitySpaceDefinitionName
  ),
  !.
find_or_add_quantity_space_definition(
  QuantitySpaceDefinitionName,
  QuantityValueNames,
  QuantitySpaceDefinition
):-
  add_quantity_space_definition(
    QuantitySpaceDefinitionName,
    QuantityValueNames,
    QuantitySpaceDefinition
  ).



% QUANTITY VALUES %

find_or_add_quantity_value(QuantitySpace, QuantityValueName, QuantityValue):-
  quantity_space_quantity_value_name(
    QuantitySpace,
    QuantityValueName,
    QuantityValue
  ),
  !.
find_or_add_quantity_value(QuantitySpace, QuantityValueName, QuantityValue):-
  add_quantity_value(QuantitySpace, QuantityValueName, QuantityValue).

find_or_add_quantity_value_definition(
  QuantitySpaceDefinition,
  _QuantityValueType,
  _QuantityValueSign,
  QuantityValueDefinitionName,
  _QuantityValueIndex,
  QuantityValueDefinition
):-
  find_quantity_value_definition(
    QuantitySpaceDefinition,
    QuantityValueDefinitionName,
    QuantityValueDefinition
  ),
  !.
find_or_add_quantity_value_definition(
  QuantitySpaceDefinition,
  QuantityValueType,
  QuantityValueSign,
  QuantityValueDefinitionName,
  QuantityValueIndex,
  QuantityValueDefinition
):-
  add_quantity_value_definition(
    QuantitySpaceDefinition,
    QuantityValueType,
    QuantityValueSign,
    QuantityValueDefinitionName,
    QuantityValueIndex,
    QuantityValueDefinition
  ).

find_quantity_value_definition(
  QuantitySpaceDefinition,
  QuantityValueDefinitionName,
  QuantityValueDefinition
):-
  quantity_space_definition_quantity_value_definition(
    QuantitySpaceDefinition,
    QuantityValueDefinition
  ),
  quantity_value_definition_label(
    QuantityValueDefinition,
    QuantityValueDefinitionName
  ),
  !.
