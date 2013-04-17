:- module(
  qr_api,
  [
% ATTRIBUTES
    attribute/1, % ?Attribute:relation
    attribute/3, % ?Entity:entity
                 % ?Attribute:relation
                 % ?AttributeValue:attribute_value
    attribute_attribute_value/2, % ?Attribute:attribute
                                 % ?AttributeValue:attribute_value
    attribute_id/2, % ?Attribute:attribute
                    % ?AttributeID:number
    attribute_label/2, % ?Attribute:relation
                       % ?Label:atom
    attribute_to_dot_name/2, % +Attribute:attribute
                             % -AttributeDOTName:atom
    attributes/1, % -Attributes:ord_set(relation)

% ATTRIBUTE VALUES
    attribute_value/1, % ?AttributeValue:attribute_value
    attribute_value_id/2, % ?AttributeValue:value
                          % ?AttributeValueID:number
    attribute_value_label/2, % ?AttributeValue:value
                             % ?AttributeValueLabel:atom
    attribute_value_to_dot_name/2, % +AttributeValue:attribute_value
                                   % -AttributeValueDOTName:atom
    attribute_values/1, % -AttributeValues:ord_set(attribute_value)

% CALCULI
    calculus_expression/1, % ?CalculusExpression:expression
    calculus_expression_definition/1, % ?CalculusExpressionDefinition:expression_definition

% COMPONENTS
    component_to_sign/2, % ?Component:component
                         % ?Sign:sign
    scenario_value_component/1, % ?ScenarioValue:component

% COMPONENT CLOUDS
    component_cloud_to_sign/2, % +ComponentCloud:component_cloud
                               % -Sign:sign
    scenario_value_component_cloud/1, % ?ScenarioValue:component_cloud

% CONFIGURATIONS
    configuration/3, % ?FromEntity:entity
                     % ?ConfigurationDefinition:configuration_definition
                     % ?ToEntity:entity
    configuration_label/4, % +FromEntity:entity
                           % +ConfigurationDefinition:configuration_definition
                           % +ToEntity:entity
                           % -ConfigurationLabel:atom

% CONFIGURATION DEFINITIONS
    configuration_definition/1, % ?ConfigurationDefinition:configuration_definition
    configuration_definition_id/2, % ?ConfigurationDefinition:configuration
                                   % ?ConfigurationDefinitionID:number
    configuration_definition_label/2, % ?ConfigurationDefinition:configuration
                                      % ?Name:atom
    configuration_definition_to_dot_name/2, % +ConfigurationDefinition:configuration
                                            % -ConfigurationDefinitionDOTName:atom
    configuration_definitions/1, % -ConfigurationDefinitions:ord_set(configuration_definition)
    configuration_direct_parent_child/2, % ?ParentConfigurationDefinition:configuration_definition
                                         % ?ChildConfigurationDefinition:configuration_definition

% CORRESPONDENCE
    correspondence_expression/1, % ?CorrespondenceExpression:expression
    correspondence_expression_definition/1, % ?CorrespondenceExpressionDefinition:expression_definition

% DERIVATIVE QUANTITY VALUES
    decreasing_derivative_expression/1, % ?Expression:expression
    decreasing_derivative_quantity_value/1, % ?DerivativeQuantityValue:derivative_quantity_value
    derivative_quantity_value/1, % ?DerivativeQuantityValue:derivative_quantity_value
    derivative_quantity_value/4, % ?Space:space
                                 % ?Quantity:quantity
                                 % ?DerivativeQuantityValue:derivative_quantity_value
                                 % ?DerivativeQuantityValueExpression:expression
    derivative_quantity_value_expression/1, % ?DerivativeExpression:expression
    derivative_quantity_value_expression_definition/1, % ?DerivativeValueExpressionDefinition:expression_definition
    derivative_quantity_value_expression_sign/2, % ?DerivativeExpression:expression
                                                 % ?Sign:sign
    derivative_quantity_value_location/2, % ?Quantity:quantity
                                          % ?Location:point_cloud
    derivative_quantity_value_point/3, % ?Space:space
                                       % ?Quantity:quantity
                                       % ?Point:point
    derivative_quantity_value_sign/2, % ?DerivativeQuantityValue:derivative_quantity_value
                                      % ?Sign:sign
    increasing_derivative_expression/1, % ?Expression
    increasing_derivative_quantity_value/1, % ?DerivativeQuantityValue:derivative_quantity_value
    steady_derivative_expression/1, % ?Expression:expression
    steady_derivative_quantity_value/1, % ?DerivativeQuantityValue:derivative_quantity_value

% ENTITIES
    %%%%entity/1, % ?Entity:entity
    entity_attribute_value/2, % +Entity:entity
                                 % -AttributeValue:attribute_value
    entity_id/2, % +Entity:entity
                 % -EntityID:number
    entity_label/2, % +Entity:entity
                    % -EntityLabel:atom
    entity_name/2, % ?Entity:entity
                   % ?EntityName:atom
    entity_quantity/2, % ?Entity:entity
                       % ?Quantity:quantity
    entity_to_dot_name/2, % +Entity:entity
                          % -EntityDOTName:atom
    entity_to_quantities/2, % +Entity:entity
                            % -Quantities:ord_set(quantity)
    entities/1, % -Entities:ord_set(entity)

% ENTITY DEFINITIONS
    entity_definition/1, % ?EntityDefinition:entity_definition
    entity_definition_entity/2, % ?EntityDefinition:entity_definition
                                % ?Entity:entity
    entity_definition_id/2, % ?EntityDefinition:entity_definition
                            % ?EntityDefinitionID:number
    entity_definition_label/2, % ?EntityDefinition:entity_definition
                               % ?EntityDefinitionLabel:atom
    entity_definition_to_dot_name/2, % +EntityDefinition:entity_definition
                                     % -EntityDefinitionDOTName:atom
    entity_definitions/1, % -EntityDefinitions:ord_set(entity_definition)
    entity_direct_parent_child/2, % ?ParentEntityDefinition:entity_definition
                                  % ?ChildEntityDefinition:entity_definition
    entity_parent_child/2, % ?ParentEntityDefinition:entity_definition
                           % ?ChildEntityDefinition:entity_definition

% EXPRESSIONS (QR)
    causal_expression/1, % ?Expression:expression
    expression_to_ccm_label/2, % +Expression:expression
                               % -ExpressionCCMLabel:atom
    negative_calculus_expression/1, % ?Expression:expression
    positive_calculus_expression/1, % ?Expression:expression
    qq_magnitude_inequality_expression/1, % ?Expression:expression
    quantity_space_expression/1, % ?Expression:expression

% (IN)EQUALITIES
    derivative_inequality_expression/1, % ?Expression:expression
    derivative_inequality_expression_definition/1, % ?ExpressionDefinition:expression_definition
    inequality/4, % ?FromArgument:quantity|quantity_value
                  % ?Relation:expression_definition
                  % ?ToArgument:quantity|quantity_value
                  % ?Expression:expression
    inequality/6, % ?Space:space
                  % ?FromArgument:quantity|quantity_value
                  % ?Relation:expression_definition
                  % ?ToArgument:quantity|quantity_value
                  % ?Expression:expression
                  % ?Point:point
    inequality_expression/1, % ?InequalityExpression:expression
    inequality_expression_definition/1, % ?InequalityExpressionDefinition:expression_definition
    inequality_expression_definition_sign/2, % ?InequalityExpressionDefinition:expression_definition
                                             % ?Sign:sign
    inequality_expression_sign/2, % ?InequalityExpression:inequality_expression
                                  % ?Sign:sign
    magnitude_inequality_expression/1, % ?Expression:expression
    magnitude_inequality_expression_definition/1, % ?ExpressionDefinition:expression_definition
    quantity_derivative_sign/3, % +Space:space,
                                % +Quantity:quantity,
                                % -Sign:sign

% MAGNITUDE QUANTITY VALUES
    highest_magnitude_quantity_value/1, % ?MagnitudeValue:magnitude_quantity_value
    lowest_magnitude_quantity_value/1, % ?MagnitudeValue:magnitude_quantity_value
    magnitude_quantity_value/1, % ?MagnitudeQuantityValue:magnitude_quantity_value
    magnitude_quantity_value/3, % ?Space:space
                                % ?Quantity:quantity
                                % ?MagnitudeQuantityValue:magnitude_quantity_value
    magnitude_quantity_value/4, % ?Space:space
                                % ?Quantity:quantity
                                % ?MagnitudeQuantityValue:magnitude_quantity_value
                                % ?MagnitudeQuantityValueExpression:expression
    magnitude_quantity_value_expression/1, % ?MagnitudeExpression:expression
    magnitude_quantity_value_expression_definition/1, % ?MagnitudeQuantityValueExpressionDefinition:expression_definition
    magnitude_quantity_value_expression_sign/2, % ?MagnitudeQuantityValueExpression:expression
                                                % ?Sign:sign
    magnitude_quantity_value_location/2, % ?Quantity:quantity
                                         % ?Location:point_cloud
    magnitude_quantity_value_point/3, % ?Space:space
                                      % ?Quantity:quantity
                                      % ?Point:point
    magnitude_quantity_value_sign/2, % ?MagnitudeQuantityValue:magnitude_quantity_value
                                     % ?Sign:sign
    negative_magnitude_expression/1, % ?MagnitudeExpression:magnitude_quantity_value_expression
    negative_magnitude_quantity_value/1, % ?MagnitudeQuantityValue:magnitude_quantity_value
    positive_magnitude_expression/1, % ?MagnitudeExpression:magnitude_quantity_value_expression
    positive_magnitude_quantity_value/1, % ?MagnitudeQuantityValue:magnitude_quantity_value
    zero_magnitude_expression/1, % ?MagnitudeExpression:magnitude_quantity_value_expression
    zero_magnitude_quantity_value/1, % ?MagnitudeQuantityValue:magnitude_quantity_value

% QUANTITIES
    from_quantity_expression/2, % ?FromQuantity:quantity
                                % ?Expression:expression
    quantities/1, % -Quantities:ord_set(quantity)
    quantity/1, % ?Quantity:quantity
    quantity/3, % ?Entity:entity
                % ?QuantityName:atom
                % ?Quantity:quantity
    quantity_decreasing_derivative_quantity_value/2, % ?Quantity:quantity
                                                     % ?DerivativeQuantityValue:derivative_quantity_value
    quantity_derivative_quantity_space/2, % ?Quantity:quantity
                                          % ?DerivativeQuantitySpace:derivative_quantity_space
    quantity_derivative_quantity_value/2, % ?Quantity:quantity
                                          % ?DerivativeQuantityValue:derivative_quantity_value
    quantity_engine_name/2, % ?Quantity:quantity
                            % ?QuantityEntityName:atom
    quantity_expression/2, % ?Quantity:quantity
                           % ?Expression:expression
    quantity_id/2, % ?Quantity:quantity,
                   % ?QuantityID:number
    quantity_increasing_derivative_quantity_value/2, % ?Quantity:quantity
                                                     % ?DerivativeQuantityValue:derivative_quantity_value
    quantity_label/2, % +Quantity:quantity,
                      % -QuantityLabel:atom
    quantity_magnitude_quantity_space/2, % ?Quantity:quantity
                                         % ?MagnitudeQuantitySpace:magnitude_quantity_space
    quantity_magnitude_quantity_value/2, % ?Quantity:quantity
                                         % ?MagnitudeQuantityValue:magnitude_quantity_value
    quantity_name/2, % ?Quantity:quantity
                     % ?QuantityName:atom
    quantity_negative_magnitude_quantity_value/2, % ?Quantity:quantity
                                                  % ?MagnitudeQuantityValue:magnitude_quantity_value
    quantity_positive_magnitude_quantity_value/2, % ?Quantity:quantity
                                                  % ?MagnitudeQuantityValue:magnitude_quantity_value
    quantity_quantity_space/2, % ?Quantity:quantity
                               % ?QuantitySpace:quantity_space
    quantity_quantity_value/2, % ?Quantity:quantity
                               % ?QuantityValue:quantity_value
    quantity_quantity_value/3, % ?Quantity:quantity
                               % ?QuantityValueName:atom
                               % ?QuantityValue:quantity_value
    quantity_space/2, % ?Quantity:quantity
                      % ?Space:space
    quantity_state/2, % ?Quantity:quntity
                      % ?State:state
    quantity_steady_derivative_quantity_value/2, % ?Quantity:quantity
                                                 % ?DerivativeQuantityValue:derivative_quantity_value
    quantity_to_dot_name/2, % +Quantity:quantity
                            % -QuantityDOTName:atom
    quantity_zero_magnitude_quantity_value/2, % ?Quantity:quantity
                                              % ?ZeroMagnitudeQuantityValue:magnitude_quantity_value
    to_quantity_expression/2, % ?ToQuantity:quantity
                              % ?Expression:expression

% QUANTITY CAUSALITIES
    branching_quantity_causality/2, % ?FromQuantity:quantity
                                    % ?Triples:ord_set(quantity/expression/point_cloud)
    negative_quantity_causality_expression/1, % ?Expression:expression
    negative_quantity_causality_expression_definition/1, % ?ExpressionDefinition:expression_definition
    positive_quantity_causality_expression/1, % ?Expression:expression
    positive_quantity_causality_expression_definition/1, % ?ExpressionDefinition:expression_definition
    quantity_causality/5, % ?FromQuantity:quantity
                          % ?Relation:expression_definition
                          % ?ToQuantity:quantity
                          % ?Expression:expression
                          % ?PointCloud:point_cloud
    quantity_causality/7, % ?State:state
                          % ?FromQuantity:quantity
                          % ?Relation:expression_definition
                          % ?ToQuantity:quantity
                          % ?Expression:expression
                          % ?PointCloud:point_cloud
                          % ?Point:point
    quantity_causality_component/1, % ?Component:component
    quantity_causality_expression/1, % ?Expression:expression
    quantity_causality_expression_definition_sign/2, % ?ExpressionDefinition:expression_definition
                                                     % ?Sign:sign
    quantity_causality_expression_sign/2, % ?Expression:expression
                                          % ?Sign:sign
    quantity_causality_path/5, % ?FromQuantity:quantity
                               % ?Relation:expression_definition
                               % ?ToQuantity:quantity
                               % ?Expression:expression
                               % ?PointCloud:point_cloud
    quantity_causality_path/7, % ?State:state
                               % ?FromQuantity:quantity
                               % ?Relation:expression_definition
                               % ?ToQuantity:quantity
                               % ?Expression:expression
                               % ?PointCloud:point_cloud
                               % ?Point:point

% QUANTITY CORRESPONDENCES
    bidirectional_magnitude_quantity_space_correspondence/2, % ?FromQuantity:quantity
                                                             % ?ToQuantity:quantity
    bidirectional_magnitude_quantity_value_correspondence/2, % ?FromMagnitudeQuantityValue:magnitude_quantity_value
                                                             % ?ToMagnitudeQuantityValue:magnitude_quantity_value
    derivative_quantity_space_correspondence_expression/1, % ?DerivativeQuantityCorrespondenceExpression:expression
    inverted_magnitude_quantity_space_correspondence/2, % ?FromQuantity:quantity
                                                        % ?ToQuantity:quantity
    inverted_magnitude_quantity_space_correspondence_expression/1, % ?Expression:expression
    magnitude_quantity_space_correspondence/2, % ?Quantity1:quantity
                                               % ?Quantity2:quantity
    magnitude_quantity_space_correspondence/3, % ?Quantity1:quantity
                                               % ?Quantity2:quantity
                                               % ?Expression:expression
    magnitude_quantity_space_correspondence_expression/1, % ?MagnitudeQuantitySpaceCorrespondenceExpression:expression
    magnitude_quantity_value_correspondence/2, % ?FromMagnitudeQuantityValue:magnitude_quantity_value
                                               % ?ToMagnitudeQuantityValue:magnitude_quantity_value
    magnitude_quantity_value_correspondence/2, % ?FromMagnitudeQuantityValue:magnitude_quantity_value
                                               % ?ToMagnitudeQuantityValue:magnitude_quantity_value
    magnitude_quantity_value_correspondence/3, % ?FromMagnitudeQuantityValue:magnitude_quantity_value
                                               % ?ToMagnitudeQuantityValue:magnitude_quantity_value
                                               % ?Expression:expression
    quantity_correspondence_expression/1, % ?Expression:expression
    quantity_space_correspondence_expression/1, % ?Expression:quantity_space_correspondence_expression
    quantity_space_correspondence_expression_definition/1, % ?ExpressionDefinition:quantity_space_correspondence_expression_definition
    quantity_value_correspondence_expression/1, % ?Expression:quantity_value_correspondence_expression
    quantity_value_correspondence_expression_definition/1, % ?ExpressionDefinition:quantity_value_correspondence_expression_definition
    unidirectional_magnitude_quantity_space_correspondence/2, % ?FromQuantity:quantity
                                                              % ?ToQuantity:quantity
    unidirectional_magnitude_quantity_value_correspondence/2, % ?FromMagnitudeQuantityValue:magnitude_quantity_value
                                                              % ?ToMagnitudeQuantityValue:magnitude_quantity_value
    uninverted_magnitude_quantity_space_correspondence/2, % ?FromQuantity:quantity
                                                          % ?ToQuantity:quantity

% QUANTITY DEFINITIONS
    quantity_definition/1, % ?QuantityDefinition:quantity_definition
    quantity_definition_derivative_quantity_space_definition/2, % ?QuantityDefinition:quantity_definition
                                                                % ?DerivativeQuantitySpaceDefinition:derivative_quantity_space_definition
    quantity_definition_id/2, % ?QuantityDefinition:quantity_definition
                              % ?QuantityDefinitionID:number
    quantity_definition_label/2, % ?QuantityDefinition:quantity_definition
                                 % ?QuantityDefinitionLabel:atom
    quantity_definition_magnitude_quantity_space_definition/2, % ?QuantityDefinition:quantity_definition
                                                               % ?MagnitudeQuantitySpaceDefinition:magnitude_quantity_space_definition
    quantity_definition_quantity/2, % ?QuantityDefinition:quantity_definition
                                    % ?Quantity:quantity
    quantity_definition_quantity_space_definition/2, % ?QuantityDefinition:quantity_definition
                                                     % ?QuantitySpaceDefinition:quantity_space_definition
    quantity_definition_to_dot_name/2, % +QuantityDefinition:quantity_definition
                                       % -QuantityDefinitionDOTName:atom
    quantity_definition_to_quantities/2, % +QuantityDefinition:quantity_definition
                                         % -Quantities:ord_set(quantity)
    quantity_definitions/1, % -QuantityDefinitions:ord_set(quantity_definition)

% QUANTITY FEEDBACK
    direct_quantity_feedback/3, % ?Quantity:quantity
                                % ?Expression:expression
                                % -SubsumedExpressions:list(expression)
    double_quantity_feedback/3, % ?Quantity:quantity
                                % ?Expression:expression
                                % -SubsumedExpressions:list(expression)
    indirect_quantity_feedback/3, % ?Quantity:quantity
                                  % ?Expression:expression
                                  % -SubsumedExpressions:list(expression)
    quantity_feedback/4, % ?Quantity:quantity
                         % ?Expression:expression
                         % -SubsumedExpressions:list(expression)
                         % ?Length:number
    singular_quantity_feedback/3, % ?Quantity:quantity
                                  % ?Expression:expression
                                  % -SubsumedExpressions:list(expression)

% QUANTITY INFLUENCES
    branching_quantity_influence/2, % ?FromQuantity:quantity
                                    % -Triples:ord_set(quantity/expression/point_cloud)
    negative_quantity_influence_expression/1, % ?NegativeQuantityInfluenceExpression:expression
    negative_quantity_influence_expression_definition/1, % ?NegativeQuantityInfluenceExpressionDefinition:expression_definition
    positive_quantity_influence_expression/1, % ?PositiveQuantityInfluenceExpression:expression
    positive_quantity_influence_expression_definition/1, % ?PositiveQuantityInfluenceExpressionDefinition:expression_definition
    quantity_influence/5, % ?FromQuantity:quantity
                          % ?Relation:expression_definition
                          % ?ToQuantity:quantity
                          % ?Expression:expression
                          % ?PointCloud:point_cloud
    quantity_influence/7, % ?State:state
                          % ?FromQuantity:quantity
                          % ?Relation:expression_definition
                          % ?ToQuantity:quantity
                          % ?Expression:expression
                          % ?PointCloud:point_cloud
                          % ?Point:point
    quantity_influence_component/1, % ?QuantityInfluenceComponent:component
    quantity_influence_component_cloud/1, % ?QuantityInfluenceComponentCloud:component_cloud
    quantity_influence_expression/1, % ?Expression:expression
    quantity_influence_expression_definition/1, % ?ExpressionDefinition:expression_definition

% QUANTITY PROPORTIONALITIES
    branching_quantity_proportionality/2, % ?FromQuantity:quantity
                                          % -Triples:ord_set(quantity/expression/point_cloud)
    negative_quantity_proportionality_expression/1, % ?NegativeQuantityProportionalityExpression:expression
    negative_quantity_proportionality_expression_definition/1, % ?NegativeQuantityProportionalityExpressionDefinition:expression_definition
    positive_quantity_proportionality_expression/1, % ?PositiveQuantityProportionalityExpression:expression
    positive_quantity_proportionality_expression_definition/1, % ?PositiveQuantityProportionalityExpressionDefinition:expression_definition
    quantity_proportionality/5, % ?FromQuantity:quantity
                                % ?Relation:expression_definition
                                % ?ToQuantity:quantity
                                % ?Expression:expression
                                % ?PointCloud:point_cloud
    quantity_proportionality/7, % ?Space:space
                                % ?FromQuantity:quantity
                                % ?Relation:expression_definition
                                % ?ToQuantity:quantity
                                % ?Expression:expression
                                % ?PointCloud:point_cloud
                                % ?Point:point
    quantity_proportionality_component/1, % ?QuantityProportionalityComponent:component
    quantity_proportionality_component_cloud/1, % ?QuantityProportionalityComponentCloud:component_cloud
    quantity_proportionality_component_definition/1, % ?QuantityProportionalityComponentDefinition:component_definition
    quantity_proportionality_expression/1, % ?Expression:expression
    quantity_proportionality_expression_definition/1, % ?ExpressionDefinition:expression_definition

% QUANTITY SPACES
    derivative_quantity_space/1, % +DerivativeQuantitySpaces:derivative_quantity_space
    derivative_quantity_space_definition/1, % ?DerivativeQuantitySpaceDefinition:derivative_quantity_space_definition
    derivative_quantity_spaces/1, % +DerivativeQuantitySpaces:ord_set(derivative_quantity_space)
    magnitude_quantity_space/1, % +MagnitudeQuantitySpace:magnitude_quantity_space
    magnitude_quantity_spaces/1, % +MagnitudeQuantitySpaces:ord_set(magnitude_quantity_space)
    quantity_space/1, % -QuantitySpaces:quantity_space
    quantity_space_id/2, % ?QuantitySpace:quantity_space
                         % ?QuantitySpaceID:number
    quantity_space_label/2, % ?QuantitySpace:quantity_space
                            % ?QuantitySpaceLabel:atom
    quantity_space_highest_quantity_value/2, % ?QuantitySpace:quantity_space
                                             % ?HighestQuantityValue:quantity_value
    quantity_space_interval_quantity_value/2, % ?QuantitySpace:quantity_space
                                              % ?IntervalQuantityValue:interval_quantity_value
    quantity_space_lowest_quantity_value/2, % ?QuantitySpace:quantity_space
                                            % ?LowestQuantityValue:quantity_value
    quantity_space_negative_quantity_value/2, % ?QuantitySpace:quantity_space
                                              % ?NegativeLandmark:quantity_value
    quantity_space_point_quantity_value/2, % ?QuantitySpace:quantity_space
                                           % ?PointQuantityValue:point_quantity_value
    quantity_space_positive_quantity_value/2, % ?QuantitySpace:quantity_space
                                              % ?PositiveQuantityValue:quantity_value
    quantity_space_quantity_value/2, % ?QuantitySpace:quantity_space
                                     % ?QuantityValue:quantity_value
    quantity_space_quantity_value_index/3, % ?QuantitySpace:quantity_space
                                           % ?Index:number
                                           % ?QuantityValue:quantity_value
    quantity_space_quantity_value_name/3, % ?QuantitySpace:quantity_space
                                          % ?QuantityValueName:atom
                                          % ?QuantityValue:quantity_value
    quantity_space_to_dot_name/2, % +QuantitySpace:quantity_space
                                  % -QuantitySpaceDOTName:atom
    quantity_space_to_cardinality/2, % +QuantitySpace:quantity_space
                                     % -Cardinality:number
    quantity_space_to_quantity_values/2, % +QuantitySpace:quantity_space
                                         % -QuantityValues:ord_set(quantity_value)
    quantity_space_to_quantity_values/4, % +QuantityValue:quantity_value,
                                         % +QuantitySpace:quantity_space,
                                         % -LowerQuantityValues:list(quantity_value),
                                         % -HigherQuantityValues:list(qauntity_value)
    quantity_space_zero_quantity_value/2, % ?QuantitySpace:quantity_space
                                          % ?ZeroMagnitudeQuantityValue:magnitude_quantity_value
    quantity_spaces/1, % -QuantitySpaces:ord_set(quantity_space)

% QUANTITY SPACE DEFINITIONS
    quantity_space_definition/1, % ?QuantitySpaceDefinition:quantity_space_definition
    quantity_space_definition_id/2, % ?QuantitySpaceDefinition:quantity_space_definition
                                    % ?QuantitySpaceDefinitionID:number
    quantity_space_definition_label/2, % ?QuantitySpaceDefinition:quantity_space_definition
                                       % ?QuantitySpaceDefinitionLabel:atom
    quantity_space_definition_quantity_space/2, % ?QuantitySpaceDefinition:quantity_space_definition
                                                % ?QuantitySpace:quantity_space
    quantity_space_definition_quantity_value_definition/2, % ?QuantitySpaceDefinition:quantity_space_definition
                                                           % ?QuantityValueDefinition:quantity_value_definition
    quantity_space_definition_to_dot_name/2, % ?QuantitySpaceDefinition:quantity_space_definition
                                             % ?QuantitySpaceDefinitionDOTName:atom
    quantity_space_definitions/1, % -QuantitySpaceDefinitions:ord_set(quantity_space_definition)

% QUANTITY VALUES
    highest_quantity_value/1, % ?HighestQuantityValue:quantity_value
    interval_quantity_value/1, % ?IntervalQuantityValue:interval_quantity_value
    interval_quantity_value_between_point_quantity_values/3, % ?LowerPointQuantityValue:quantity_value,
                                                             % ?IntervalQuantityValue:quantity_value,
                                                             % ?HigherPointQuantityValue:quantity_value
    lowest_quantity_value/1, % ?LowestQuantityValue:quantity_value
    negative_quantity_value/1, % ?QuantityValue:quantity_value
    negative_quantity_value/2, % ?Quantity:quantity
                               % ?QuantityValue:quantity_value
    point_quantity_value/1, % ?PointQuantityValue:point_quantity_value
    positive_quantity_value/1, % ?QuantityValue:quantity_value
    positive_quantity_value/2, %?Quantity:quantity
                               % ?QuantityValue:quantity_value
    quantity_value/1, % ?QuantityValue:quantity_value
    quantity_value/6, % ?Space:space
                      % ?Quantity:quantity
                      % ?MagnitudeQuantityValue:magnitude_quantity_value
                      % ?MagnitudeQuantityValueExpression:expression
                      % ?DerivativeQuantityValue:derivative_quantity_value
                      % ?DerivativeQuantityValueExpression:expression
    quantity_value_expression/1, % ?QuantityValueExpression:quantity_value_expression
    quantity_value_greater_than_quantity_value/2, % ?HigherQuantityValue:quantity_value
                                                  % ?LowerQuantityValue:quantity_value
    quantity_value_greater_than_quantity_value/3, % ?HigherQuantityValue:quantity_value
                                                  % ?Distance:integer
                                                  % ?LowerQuantityValue:quantity_value
    quantity_value_id/2, % ?QuantityValue:quantity_value
                         % ?QuantityValueID:number
    quantity_value_index/2, % +QuantityValue:quantity_value
                            % -Index:number
    quantity_value_label/2, % ?QuantityValue:value
                            % ?QuantityValueLabel:atom
    quantity_value_quantity_value/2, % ?DirectlyLowerQuantityValue:quantity_value
                                     % ?DirectlyHigherQuantityValue:quantity_value
    quantity_value_quantity_value_strict/2, % ?DirectlyLowerQuantityValue:quantity_value
                                            % ?DirectlyHigherQuantityValue:quantity_value
    quantity_value_smaller_than_quantity_value/2, % ?LowerQuantityValue:quantity_value
                                                  % ?HigherQuantityValue:quantity_value
    quantity_value_smaller_than_quantity_value/3, % ?LowerQuantityValue:quantity_value
                                                  % ?Distance:integer
                                                  % ?HigherQuantityValue:quantity_value
    quantity_value_to_dot_name/2, % +QuantityValue:quantity_value
                                  % -QuantityValueDOTName:atom
    quantity_values/1, % -QuantityValues:ord_set(quantity_value)
    zero_quantity_value/1, % ?QuantityValue:quantity_value
    zero_quantity_value/2, % ?Quantity:quantity
                           % ?QuantityValue:quantity_value

% QUANTITY VALUE DEFINITIONS
    interval_quantity_value_definition/1, % ?IntervalQuantityValueDefinition:interval_quantity_value_definition
    negative_quantity_value_definition/1, % ?NegativeQuantityValueDefinition:quantity_value_definition
    point_quantity_value_definition/1, % ?PointQuantityValueDefinition:point_quantity_value_definition
    positive_quantity_value_definition/1, % ?PositiveQuantityValueDefinition:quantity_value_definition
    quantity_value_definition/1, % ?QuantityValueDefinition:quantity_value_definition
    quantity_value_definition_id/2, % ?QuantityValueDefinition:quantity_value_definition
                                    % QuantityValueDefinitionID:number
    quantity_value_definition_label/2, % ?QuantityValueDefinition:quantity_value_definition
                                       % ?QuantityValueDefinitionLabel:atom
    quantity_value_definition_quantity_value/2, % ?QuantityValueDefinition:quantity_value_definition
                                                % ?QuantityValue:quantity_value
    quantity_value_definition_to_dot_name/2, % +QuantityValueDefinition:quantity_value_definition
                                             % -QuantityValueDefinitionDOTName:atom
    quantity_value_definitions/1, % -QuantityValueDefinitions:ord_set(quantity_value_definition)
    zero_quantity_value_definition/1, % ?ZeroQuantityValueDefinition:quantity_value_definition

% RELATIONS
    relation_category/2, % ?Relation:expression_definition
                         % ?RelationCategory:expression_definition

% SIGNS
    equal_to/1, % ?Sign:sign
    equal_to_expression/1, % ?Expression:expression
    expression_sign/2, % ?Expression:expression
                       % ?Sign:sign
    greater_than/1, % ?Sign:sign
    greater_than_expression/1, % ?Expression:expression
    multiply_signs/2, % +Signs:list(sign)
                      % -Sign:sign
    multiply_signs/3, % +Sign1:sign
                      % +Sign2:sign
                      % -Sign3:sign
    sign/1, % ?Sign:sign
    smaller_than/1, % ?Sign:sign
    smaller_than_expression/1, % ?Expression:expression

% SPACES
    space_to_quantities/2, % +Space:space
                           % -Quantities:ord_set(quantity)

% STATES
    state_to_quantities/2 % +State:state
                          % -Quantities:ord_set(quantity)
  ]
).

/** <module> QR API

Methods for QR terminology.

@author Wouter Beek
@version 2012/04-2012/08
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_label)).
:- use_module(ccm(ccm_verb)).
:- use_module(generic(atom_ext)).
:- use_module(generic(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(standards(html)).

:- multifile
  relation_category(_Relation, _Category).



% ATTRIBUTES %

%% attribute(?Attribute:relation) is nondet.
% An attribute relation.
%
% @param Attribute An attribute relation between an entity and an attribute
%        value.

attribute(Attribute):-
  rdfs_subproperty_of(Attribute, entity:has_attribute_value),
  \+(rdf_global_id(entity:has_attribute_value, Attribute)).

attribute(Entity, Attribute, AttributeValue):-
  attribute(Attribute),
  rdf(Entity, Attribute, AttributeValue, ccm).

%% attribute_attribute_value(
%%   ?Attribute:relation,
%%   ?AttributeValue:attribute_value
%% ) is nondet.
% Pairs of attribute relations and attribute values.
%
% @param Attribute An attribute relation.
% @param AttributeValue An attribute value.

attribute_attribute_value(Attribute, AttributeValue):-
  nonvar(AttributeValue),
  !,
  attribute_attribute_value1(Attribute, AttributeValue),
  % An attribute value has exactly one attribute.
  !.
attribute_attribute_value(Attribute, AttributeValue):-
  attribute_attribute_value1(Attribute, AttributeValue).

attribute_attribute_value1(Attribute, AttributeValue):-
  rdf(Attribute, attribute:has_possible_value, AttributeValue, ccm).

%% attribute_id(?Attribute:relation, ?AttributeID:number) is nondet.
% Pairs of attributes and their identifiers.
%
% @param Attribute An attribute.
% @param AttributeID The numberic identifier of an attribute.

attribute_id(Attribute, AttributeID):-
  nonvar(Attribute),
  !,
  attribute_id1(Attribute, AttributeID),
  % An attribute has exactly one identifier.
  !.
attribute_id(Attribute, AttributeID):-
  attribute(Attribute),
  attribute_id1(Attribute, AttributeID).

attribute_id1(Attribute, AttributeID):-
  rdf_datatype(Attribute, attribute:has_id, integer, AttributeID, ccm).

%% attribute_label(?Attribute:attribute, ?AttributeLabel:atom) is nondet.
% Pairs of attributes and their natural language labels.
%
% @param Attribute An attribute.
% @param AttributeLabel The atomic natural language label of an attribute.

attribute_label(Attribute, AttributeLabel):-
  attribute(Attribute),
  rdfs_label(Attribute, AttributeLabel).

attribute_to_dot_name(Attribute, AttributeDOTName):-
  attribute_id(Attribute, AttributeID),
  format(atom(AttributeDOTName), 'at_~w', [AttributeID]).

%% attributes(-Attributes:ord_set(relation)) is det.
% Returns all attribute relations.
%
% @param Attributes An ordered set of attributes.

attributes(Attributes):-
  setoff(
    Attribute,
    attribute(Attribute),
    Attributes
  ).


% ATTRIBUTE VALUES %

%% attribute_value(?AttributeValue:attribute_value) is nondet.
% Attribute values.
%
% @param AttributeValue An attribute value.

attribute_value(AttributeValue):-
  rdfs_individual_of(AttributeValue, value:attribute_value).

attribute_value_id(AttributeValue, AttributeValueID):-
  nonvar(AttributeValue),
  !,
  attribute_value_id1(AttributeValue, AttributeValueID),
  % An attribute value has exactly one identifier.
  !.
attribute_value_id(AttributeValue, AttributeValueID):-
  attribute_value(AttributeValue),
  attribute_value_id1(AttributeValue, AttributeValueID).

attribute_value_id1(AttributeValue, AttributeValueID):-
  rdf_datatype(AttributeValue, value:has_id, integer, AttributeValueID, ccm).

%% attribute_value_label(+AttributeValue:value, -Label:atom) is det.
% Reutrns the natural language label of the given attribute value.
%
% @param AttributeValue An attribute value.
% @param Label The atomic natural language label of a attribute value.

attribute_value_label(AttributeValue, Label):-
  attribute_value(AttributeValue),
  rdfs_label(AttributeValue, Label).

attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName):-
  attribute_value_id(AttributeValue, AttributeValueID),
  format(atom(AttributeValueDOTName), 'av_~w', [AttributeValueID]).

%% attribute_values(-AttributeValues:ord_set(attribute_value)) is det.
% Returns all attribute values.
%
% @param AttributeValues An ordered set of attribute values.

attribute_values(AttributeValues):-
  setoff(
    AttributeValue,
    attribute_value(AttributeValue),
    AttributeValues
  ).



% CALCULI %

%% calculus_expression(
%%   ?CalculusExpression:expression
%% ) is nondet.
% Calculus expressions.
%
% @param CalculusExpression A calculus expression.

calculus_expression(CalculusExpression):-
  rdfs_individual_of(CalculusExpression, expression:calculus).

%% calculus_expression_definition(
%%   ?CalculusExpressionDefinition:expression_definition
%% ) is nondet.
% Calculus expression definitions.
%
% @param CalculusExpressionDefinition A calculus expression definition.

calculus_expression_definition(CalculusExpressionDefinition):-
  rdfs_subclass_of(CalculusExpressionDefinition, expression:calculus).



% COMPONENTS %

component_to_sign(Component, Sign):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_to_sign(ComponentCloud, Sign).

scenario_value_component(ScenarioValue):-
  component_cloud_component(ScenarioValue_, ScenarioValue),
  scenario_value_component_cloud(ScenarioValue_).



% COMPONENT CLOUDS %

%% component_cloud_to_sign(
%%   +ComponentCloud:component_cloud,
%%   -Sign:sign
%% ) is nondet.
% Returns the sign for the given component cloud.
% We know that a component cloud can only have components with the same
% sign, because the components of a component cloud must share the same
% supports.
% NOTE: This method is not deterministic, since there may be multiple
% support ports that have signs (e.g., for competitive quantity causality
% component clouds).
%
% @param ComponentCloud A component cloud.
% @param Sign A QR sign.

component_cloud_to_sign(ComponentCloud, Sign):-
  component_cloud_support_point_cloud(ComponentCloud, SupportPointCloud),
  % We know that all support expressions are the same for a given
  % component cloud.
  once(expression_point_cloud(SupportExpression, SupportPointCloud)),
  expression_sign(SupportExpression, Sign).

%% scenario_value_component_cloud(?ScenarioValue:component_cloud) is nondet.
% Scenario value component clouds.
%
% @param ScenarioValue A component cloud.

scenario_value_component_cloud(ScenarioValue):-
  rdfs_individual_of(ScenarioValue, component:scenario_value).



% CONFIGURATIONS %

%% configuration(
%%   ?FromEntity:entity,
%%   ?ConfigurationDefinition:configuration_definition,
%%   ?ToEntity:entity
%% ) is nondet.
% A configurations between entities.
%
% @param FromEntity An entity.
% @param ConfigurationDefinition A configuration definition.
% @param ToEntity An entity.

configuration(FromEntity, ConfigurationDefinition, ToEntity):-
  configuration_definition(ConfigurationDefinition),
  rdf(FromEntity, ConfigurationDefinition, ToEntity, ccm).

%% configuration_label(
%%   +FromEntity:entity,
%%   +ConfigurationDefinition:configuration_definition,
%%   +ToEntity:entity,
%%   -ConfigurationLabel:atom
%% ) is det.
% Returns a concise label for the given configuration relation.
%
% @param FromEntity An entity.
% @param ConfigurationDefinition A configuration definition.
% @param ToEntity An entity.
% @param ConfigurationLabel A concise atomic label of a
%        configuration relation.

configuration_label(
  FromEntity,
  ConfigurationDefinition,
  ToEntity,
  ConfigurationLabel
):-
  entity_label(FromEntity, FromEntityLabel),
  configuration_definition_label(
    ConfigurationDefinition,
    ConfigurationDefinitionLabel
  ),
  entity_label(ToEntity, ToEntityLabel),
  format(
    atom(ConfigurationLabel),
    '(~w,~w,~w)',
    [FromEntityLabel, ConfigurationDefinitionLabel, ToEntityLabel]
  ).


% CONFIGURATION DEFINITIONS %

%% configuration_definition(
%%   ?ConfigurationDefinition:configuration_definition
%% ) is nondet.
% A configuration relation.
%
% @param ConfigurationDefinition A relation.

configuration_definition(ConfigurationDefinition):-
  rdfs_subproperty_of(ConfigurationDefinition, entity:has_configuration).

configuration_definition_id(
  ConfigurationDefinition,
  ConfigurationDefinitionID
):-
  nonvar(ConfigurationDefinition),
  !,
  configuration_definition_id1(
    ConfigurationDefinition,
    ConfigurationDefinitionID
  ),
  % A configuration has exactly one identifier.
  !.
configuration_definition_id(
  ConfigurationDefinition,
  ConfigurationDefinitionID
):-
  configuration_definition(ConfigurationDefinition),
  configuration_definition_id1(
    ConfigurationDefinition,
    ConfigurationDefinitionID
  ).

configuration_definition_id1(
  ConfigurationDefinition,
  ConfigurationDefinitionID
):-
  rdf_datatype(ConfigurationDefinition, configuration:has_id, integer, ConfigurationDefinitionID, ccm).

configuration_definition_label(ConfigurationDefinition, Name):-
  var(ConfigurationDefinition),
  var(Name),
  !,
  configuration_definition(ConfigurationDefinition),
  configuration_definition_label_(ConfigurationDefinition, Name).
% There is a one-to-one-mapping between configuration definitions
% and their names.
configuration_definition_label(ConfigurationDefinition, Name):-
  configuration_definition_label_(ConfigurationDefinition, Name),
  !.

configuration_definition_label_(ConfigurationDefinition, Name):-
  rdfs_label(ConfigurationDefinition, Name).

configuration_definition_to_dot_name(
  ConfigurationDefinition,
  ConfigurationDefinitionDOTName
):-
  configuration_definition_id(
    ConfigurationDefinition,
    ConfigurationDefinitionID
  ),
  format(
    atom(ConfigurationDefinitionDOTName),
    'cd_~w',
    [ConfigurationDefinitionID]
  ).

%% configuration_definitions(
%%   -ConfigurationDefinitions:ord_set(configuration_definition)
%% ) is det.
% Returns the ordered set of configuration definitions.
%
% @param ConfigurationDefinitions An ordered set of relations.

configuration_definitions(ConfigurationDefinitions):-
  setoff(
    ConfigurationDefinition,
    configuration_definition(ConfigurationDefinition),
    ConfigurationDefinitions
  ).

configuration_direct_parent_child(
  ParentConfigurationDefinition,
  ChildConfigurationDefinition
):-
  configuration_definition(ChildConfigurationDefinition),
  rdf(
    ChildConfigurationDefinition,
    rdfs:subPropertyOf,
    ParentConfigurationDefinition,
    ccm
  ),
  configuration_definition(ParentConfigurationDefinition),
  ChildConfigurationDefinition \== ParentConfigurationDefinition.



% CORRESPONDENCES %

correspondence_expression(CorrespondenceExpression):-
  rdfs_individual_of(CorrespondenceExpression, expression:correspondence).

%% correspondence_expression_definition(
%%    ?CorrespondenceExpressionDefinition:expression_definition
%% ) is nondet.
% Correpsondence expression definitions.
%
% @param CorrespondenceExpressionDefinition A correspondence
%        expression definition.

correspondence_expression_definition(CorrespondenceExpressionDefinition):-
  rdfs_subclass_of(
    CorrespondenceExpressionDefinition,
    expression:correspondence
  ).



% DERIVATIVE QUANTITY VALUES %

decreasing_derivative_expression(Expression):-
  (
    derivative_quantity_value_expression(Expression)
  ;
    rdfs_individual_of(
      Expression,
      expression:qp_derivative_smaller_than_or_equal_to
    )
  ),
  expression_to_argument(Expression, DerivativeQuantityValue),
  decreasing_derivative_quantity_value(DerivativeQuantityValue).
decreasing_derivative_expression(Expression):-
  rdfs_individual_of(Expression, expression:qp_derivative_smaller_than),
  expression_to_argument(Expression, DerivativeQuantityValue),
  (
    steady_derivative_quantity_value(DerivativeQuantityValue)
  ;
    decreasing_derivative_quantity_value(DerivativeQuantityValue)
  ).

%% decreasing_derivative_quantity_value(
%%   ?DecreasingDerivativeValue:interval_derivative_value
%% ) is nondet.
% Either succeeds if the given derivative value is increasing, or
% returns a decreasing derivative value if one is available.
%
% @param DecreasingDerivativeValue A decreasing derivative value.

decreasing_derivative_quantity_value(DecreasingDerivativeValue):-
  rdfs_individual_of(
    DecreasingDerivativeValue,
    value:decreasing_derivative_value
  ).

%% derivative_quantity_value(
%%   ?DerivativeValue:derivative_quantity_value
%% ) is nondet.
% Either succeeds if the given argument is a derivative value, or
% returns a derivative value if one is available in the CCM.
%
% @param DerivativeValue A derivative value.

derivative_quantity_value(DerivativeValue):-
  rdfs_individual_of(DerivativeValue, value:derivative_quantity_value).

derivative_quantity_value(
  Space,
  Quantity,
  DerivativeQuantityValue,
  DerivativeQuantityValueExpression
):-
  nonvar(Space),
  nonvar(Quantity),
  !,
  derivative_quantity_value_(
    Space,
    Quantity,
    DerivativeQuantityValue,
    DerivativeQuantityValueExpression
  ),
  % A quantity has at most one derivative quantity value per space.
  !.
derivative_quantity_value(
  Space,
  Quantity,
  DerivativeQuantityValue,
  DerivativeQuantityValueExpression
):-
  derivative_quantity_value_(
    Space,
    Quantity,
    DerivativeQuantityValue,
    DerivativeQuantityValueExpression
  ).

derivative_quantity_value_(
  Space,
  Quantity,
  DerivativeQuantityValue,
  DerivativeQuantityValueExpression
):-
  derivative_quantity_value_expression(DerivativeQuantityValueExpression),
  from_quantity_expression(Quantity, DerivativeQuantityValueExpression),
  space_expression(Space, DerivativeQuantityValueExpression),
  expression_to_argument(
    DerivativeQuantityValueExpression,
    DerivativeQuantityValue
  ).

%% derivative_quantity_value_expression(
%%   ?DerivativeValueExpression:expression
%% ) is nondet.
% Derivative value expressions.
%
% @param DerivativeValueExpression An expression.

derivative_quantity_value_expression(DerivativeValueExpression):-
  rdfs_individual_of(
    DerivativeValueExpression,
    expression:qp_derivative_equal_to
  ).

%% derivative_quantity_value_expression_definition(
%%   ?DerivativeValueExpression:expression_definition
%% ) is nondet.
% Derivative value expression definitions.
%
% @param DerivativeValueExpressionDefinition An expression definition.

derivative_quantity_value_expression_definition(
  DerivativeValueExpressionDefinition
):-
  rdfs_subclass_of(
    DerivativeValueExpressionDefinition,
    expression:qp_derivative_equal_to
  ).

derivative_quantity_value_expression_sign(
  DerivativeQuantityValueExpression,
  Sign
):-
  expression_to_argument(
    DerivativeQuantityValueExpression,
    DerivativeQuantityValue
  ),
  derivative_quantity_value_sign(DerivativeQuantityValue, Sign).

% A quantity determines a derivative quantity value location.
derivative_quantity_value_location(Quantity, Location):-
  nonvar(Quantity),
  !,
  derivative_quantity_value_location_(Quantity, Location),
  !.
derivative_quantity_value_location(Quantity, Location):-
  derivative_quantity_value_location_(Quantity, Location).

derivative_quantity_value_location_(Quantity, Location):-
  expression_point_cloud(Expression, Location),
  expression_from_argument(Expression, Quantity),
  derivative_quantity_value_expression(Expression).

derivative_quantity_value_point(Space, Quantity, Point):-
  point(Expression, Space, Point),
  derivative_quantity_value_expression(Expression),
  from_quantity_expression(Quantity, Expression).

%% derivative_quantity_value_sign(
%%   ?DerivativeQuantityValue:derivative_quantity_value,
%%   ?Sign:sign
%% ) is nondet.

derivative_quantity_value_sign(DerivativeQuantityValue, Sign):-
  nonvar(DerivativeQuantityValue),
  !,
  derivative_quantity_value_sign_(DerivativeQuantityValue, Sign),
  !.
derivative_quantity_value_sign(DerivativeQuantityValue, Sign):-
  derivative_quantity_value_sign_(DerivativeQuantityValue, Sign).

derivative_quantity_value_sign_(DerivativeQuantityValue, Sign):-
  increasing_derivative_quantity_value(DerivativeQuantityValue),
  !,
  greater_than(Sign).
derivative_quantity_value_sign_(DerivativeQuantityValue, Sign):-
  decreasing_derivative_quantity_value(DerivativeQuantityValue),
  !,
  smaller_than(Sign).
derivative_quantity_value_sign_(DerivativeQuantityValue, Sign):-
  steady_derivative_quantity_value(DerivativeQuantityValue),
  equal_to(Sign).

increasing_derivative_expression(Expression):-
  (
    derivative_quantity_value_expression(Expression)
  ;
    rdfs_individual_of(
      Expression,
      expression:qp_derivative_greater_than_or_equal_to
    )
  ),
  expression_to_argument(Expression, DerivativeQuantityValue),
  increasing_derivative_quantity_value(DerivativeQuantityValue).
increasing_derivative_expression(Expression):-
  rdfs_individual_of(Expression, expression:qp_derivative_greater_than),
  expression_to_argument(Expression, DerivativeQuantityValue),
  (
    steady_derivative_quantity_value(DerivativeQuantityValue)
  ;
    increasing_derivative_quantity_value(DerivativeQuantityValue)
  ).

%% increasing_derivative_quantity_value(
%%   ?IncreasingDerivativeValue:interval_derivative_value
%% ) is nondet.
% Either succeeds if the given derivative value is increasing, or
% returns a decreasing derivative value if one is available.
%
% @param IncreasingDerivativeValue An increasing derivative value.

increasing_derivative_quantity_value(IncreasingDerivativeValue):-
  rdfs_individual_of(IncreasingDerivativeValue, value:increasing_derivative_value).

%% steady_derivative_quantity_value(
%%   ?SteadyDerivativeValue:derivative_point_value
%% ) is nondet.
% Either succeeds if the given derivative value is steady, or
% returns a steady derivative value if one is available.
%
% @param SteadyDerivativeValue A steady derivative value.

steady_derivative_quantity_value(SteadyDerivativeValue):-
  rdfs_individual_of(SteadyDerivativeValue, value:steady_derivative_value).

steady_derivative_expression(Expression):-
  derivative_quantity_value_expression(Expression),
  expression_to_argument(Expression, DerivativeQuantityValue),
  steady_derivative_quantity_value(DerivativeQuantityValue).



% ENTITIES %

%% entity(?Entity:entity) is nondet.
% An entity.
%
% @param Entity An entity.

entity(Entity):-
  rdfs_individual_of(Entity, entity:entity).

entity_attribute_value(Entity, AttributeValue):-
  attribute(Entity, _Attribute, AttributeValue).

entity_id(Entity, EntityID):-
  nonvar(Entity),
  !,
  entity_id1(Entity, EntityID),
  % An entity has exactly one identifier.
  !.
entity_id(Entity, EntityID):-
  qr_api:entity(Entity),
  entity_id1(Entity, EntityID).

entity_id1(Entity, EntityID):-
  rdf_datatype(Entity, entity:has_id, integer, EntityID, ccm).

%% entity_label(+Entity:entity, -EntityLabel:atom) is det.
% Returns the natural language label of the given entity.
%
% @param Entity An entity.
% @param EntityLabel The atomic natural language label of an entity.

entity_label(Entity, EntityLabel):-
  rdfs_label(Entity, EntityLabel1),
  % An entity has exactly one label.
  !,
  underscores_to_spaces(EntityLabel1, EntityLabel).

%% entity_name(?Entity:entity, ?EntityName:atom) is nondet.
% Pairs of entities and their names.
%
% @param Entity An entity.
% @param EntityName The atomic name of an entity.

entity_name(Entity, EntityName):-
  nonvar(Entity),
  !,
  entity_name1(Entity, EntityName),
  !.
entity_name(Entity, EntityName):-
  entity_name1(Entity, EntityName).

entity_name1(Entity, EntityName):-
  rdfs_label(Entity, EntityName).

%% entity_quantity(?Entity:entity, ?Quantity:quantity) is nondet.
% Paoirs of entities and quantities.
%
% @param Entity An entity.
% @param Quantity A quantity.

entity_quantity(Entity, Quantity):-
  var(Entity),
  var(Quantity),
  !,
  entity(Entity),
  rdf(Quantity, quantity:has_entity, Entity, ccm).
entity_quantity(Entity, Quantity):-
  rdf(Quantity, quantity:has_entity, Entity, ccm).

entity_to_dot_name(Entity, EntityDOTName):-
  entity_id(Entity, EntityID),
  format(atom(EntityDOTName), 'e_~w', [EntityID]).

%% entity_to_quantities(+Entity:entity, -Quantities:ord_set(quantity)) is det.
% Returns the quantities of the given entity.
%
% @param Entity The URI of an entity.
% @param Quantities The ordered set of URIs of quantities.

entity_to_quantities(Entity, Quantities):-
  setoff(
    Quantity,
    entity_quantity(Entity, Quantity),
    Quantities
  ).

%% entities(-Entities:ord_set(entity)) is det.
% Returns the ordered set of all entities.
%
% @param Entities The ordered set of entities.

entities(Entities):-
  setoff(
    Entity,
    qr_api:entity(Entity),
    Entities
  ).


% ENTITY DEFINITIONS %

entity_definition(EntityDefinition):-
  rdfs_subclass_of(EntityDefinition, entity:entity).

%% entity_definition_entity(
%%   ?EntityDefinition:entity_definition,
%%   ?Entity:entity
%% ) is nondet.
% Pairs of entity definitions and entities.
%
% @param EntityDefinition An entity definition.
% @param Entity An entity.

entity_definition_entity(EntityDefinition, Entity):-
  nonvar(Entity),
  !,
  entity_definition_entity1(EntityDefinition, Entity),
  % An entity has exactly one entity definition.
  !.
entity_definition_entity(EntityDefinition, Entity):-
  entity_definition(EntityDefinition),
  entity_definition_entity1(EntityDefinition, Entity).

entity_definition_entity1(EntityDefinition, Entity):-
  rdfs_individual_of(Entity, EntityDefinition).

entity_definition_id(EntityDefinition, ID):-
  nonvar(EntityDefinition),
  !,
  entity_definition_id1(EntityDefinition, ID),
  % An entity definition has exactly one identifier.
  !.
entity_definition_id(EntityDefinition, ID):-
  entity_definition(EntityDefinition),
  entity_definition_id1(EntityDefinition, ID).

entity_definition_id1(EntityDefinition, ID):-
  rdf_datatype(EntityDefinition, entity:has_id, integer, ID, ccm).

entity_definition_label(EntityDefinition, Label):-
  var(EntityDefinition),
  var(Label),
  !,
  entity_definition(EntityDefinition),
  entity_definition_label_(EntityDefinition, Label).
entity_definition_label(EntityDefinition, Label):-
  entity_definition_label_(EntityDefinition, Label),
  !.

entity_definition_label_(EntityDefinition, Label):-
  rdfs_label(EntityDefinition, Label).

entity_definition_to_dot_name(EntityDefinition, EntityDefinitionDOTName):-
  entity_definition_id(EntityDefinition, EntityDefinitionID),
  format(atom(EntityDefinitionDOTName), 'ed_~w', [EntityDefinitionID]).

entity_definitions(EntityDefinitions):-
  setoff(
    EntityDefinition,
    entity_definition(EntityDefinition),
    EntityDefinitions
  ).

entity_direct_parent_child(ParentEntityDefinition, ChildEntityDefinition):-
  nonvar(ParentEntityDefinition),
  nonvar(ChildEntityDefinition),
  !,
  entity_direct_parent_child_(ParentEntityDefinition, ChildEntityDefinition).
entity_direct_parent_child(ParentEntityDefinition, ChildEntityDefinition):-
  entity_definition(ParentEntityDefinition),
  entity_direct_parent_child_(ParentEntityDefinition, ChildEntityDefinition).

entity_direct_parent_child_(ParentEntityDefinition, ChildEntityDefinition):-
  rdf(ChildEntityDefinition, rdfs:subClassOf, ParentEntityDefinition, ccm),
  ParentEntityDefinition \== ChildEntityDefinition.

%% entity_parent_child(
%%   ?ParentEntityDefinition:entity,
%%   ?ChildEntityDefinition:entity
%% ) is nondet.
% Pairs of parent and child relations between entity definitions.
%
% @param ParentEntityDefinition An entity definition.
% @param ChildEntityDefinition An entity definition.

entity_parent_child(ParentEntityDefinition, ChildEntityDefinition):-
  entity_definition(ChildEntityDefinition),
  rdfs_subclass_of(ChildEntityDefinition, ParentEntityDefinition),
  ChildEntityDefinition \== ParentEntityDefinition,
  entity_definition(ParentEntityDefinition).


% EXPRESSIONS (QR) %

%% causal_expression(?CausalExpression:expression) is semidet.
% A causal expression.
%
% @param CausalExpression An expression.

causal_expression(CausalExpression):-
  rdfs_individual_of(CausalExpression, expression:causal).

expression_to_ccm_label(Expression, ExpressionLabel):-
  rdfs_individual_of(Expression, expression:pc),
  inequality_expression(Expression),
  !,
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, QuantityValue),
  quantity_value_label(QuantityValue, QuantityValueLabel),
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, Calculus),
  expression_to_ccm_label(Calculus, CalculusLabel),
  format(
    atom(ExpressionLabel),
    '~w:~w~w[~w]',
    [
      ExpressionID,
      QuantityValueLabel,
      ExpressionDefinitionAbbreviation1,
      CalculusLabel
    ]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  rdfs_individual_of(Expression, expression:qc),
  inequality_expression(Expression),
  !,
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, Quantity),
  quantity_label(Quantity, QuantityLabel),
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, Calculus),
  expression_to_ccm_label(Calculus, CalculusLabel),
  format(
    atom(ExpressionLabel),
    '~w:~w~w[~w]',
    [
      ExpressionID,
      QuantityLabel,
      ExpressionDefinitionAbbreviation1,
      CalculusLabel
    ]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  rdfs_individual_of(Expression, expression:qp),
  inequality_expression(Expression),
  !,
  (
    rdfs_individual_of(Expression, expression:derivative)
  ->
    DerivativeOrMagnitude = '&#x3b4;'
  ;
    rdfs_individual_of(Expression, expression:magnitude)
  ->
    DerivativeOrMagnitude = ''
  ),
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, Quantity),
  quantity_label(Quantity, QuantityLabel),
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, QuantityValue),
  quantity_value_label(QuantityValue, QuantityValueLabel),
  format(
    atom(ExpressionLabel),
    '~w:~w(~w)~w~w',
    [
      ExpressionID,
      DerivativeOrMagnitude,
      QuantityLabel,
      ExpressionDefinitionAbbreviation1,
      QuantityValueLabel
    ]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  rdfs_individual_of(Expression, expression:qq),
  inequality_expression(Expression),
  !,
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, FromQuantity),
  quantity_label(FromQuantity, FromQuantityLabel),
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, ToQuantity),
  quantity_label(ToQuantity, ToQuantityLabel),
  format(
    atom(ExpressionLabel),
    '~w:(~w)~w(~w)',
    [
      ExpressionID,
      FromQuantityLabel,
      ExpressionDefinitionAbbreviation1,
      ToQuantityLabel
    ]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  rdfs_individual_of(Expression, expression:pp),
  inequality_expression(Expression),
  !,
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, FromMagnitudeQuantityValue),
  quantity_magnitude_quantity_value(FromQuantity, FromMagnitudeQuantityValue),
  quantity_label(FromQuantity, FromQuantityLabel),
  quantity_value_label(
    FromMagnitudeQuantityValue,
    FromMagnitudeQuantityValueLabel
  ),
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, ToMagnitudeQuantityValue),
  quantity_magnitude_quantity_value(ToQuantity, ToMagnitudeQuantityValue),
  quantity_label(ToQuantity, ToQuantityLabel),
  quantity_value_label(
    ToMagnitudeQuantityValue,
    ToMagnitudeQuantityValueLabel
  ),
  format(
    atom(ExpressionLabel),
    '~w:~w(~w)~w~w(~w)',
    [
      ExpressionID,
      FromQuantityLabel,
      FromMagnitudeQuantityValueLabel,
      ExpressionDefinitionAbbreviation1,
      ToQuantityLabel,
      ToMagnitudeQuantityValueLabel
    ]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  conditional_expression(Expression),
  !,
  expression_from_argument(Expression, ConditionalExpression),
  expression_to_ccm_label(ConditionalExpression, ConditionalExpressionLabel),
  expression_to_argument(Expression, ConsequentialExpression),
  expression_to_ccm_label(
    ConsequentialExpression,
    ConsequentialExpressionLabel
  ),
  format(
    atom(ExpressionLabel),
    'IF[~w]THEN[~w]',
    [ConditionalExpressionLabel, ConsequentialExpressionLabel]
  ).
expression_to_ccm_label(Expression, ExpressionLabel):-
  expression_id(Expression, ExpressionID),
  expression_from_argument(Expression, FromArgument),
  rdfs_label(FromArgument, FromArgumentLabel),!,
  expression_definition_expression(ExpressionDefinition, Expression),
  expression_definition_abbreviation(
    ExpressionDefinition,
    ExpressionDefinitionAbbreviation
  ),
  html_convert(
    ExpressionDefinitionAbbreviation,
    ExpressionDefinitionAbbreviation1
  ),
  expression_to_argument(Expression, ToArgument),
  rdfs_label(ToArgument, ToArgumentLabel),!,
  format(
    atom(ExpressionLabel),
    '~w:~w(~w,~w)',
    [
      ExpressionID,
      ExpressionDefinitionAbbreviation1,
      FromArgumentLabel,
      ToArgumentLabel
    ]
  ).

negative_calculus_expression(Expression):-
  expression_definition_expression(ExpressionDefinition, Expression),
  rdf_literal(ExpressionDefinition, expression:has_abbreviation, '-', ccm).

positive_calculus_expression(Expression):-
  expression_definition_expression(ExpressionDefinition, Expression),
  rdf_literal(ExpressionDefinition, expression:has_abbreviation, '+', ccm).

qq_magnitude_inequality_expression(Expression):-
  rdfs_individual_of(Expression, expression:qq),
  rdfs_individual_of(Expression, expression:magnitude),
  inequality_expression(Expression).

%% quantity_space_expression(?QuantitySpaceExpression:expression) is nondet.
% A quantity space expression.
%
% @param QuantitySpaceExpression An expression.

quantity_space_expression(QuantitySpaceExpression):-
  rdfs_individual_of(QuantitySpaceExpression, expression:quantity_space).



% (IN)EQUALITIES %

derivative_inequality_expression(DerivativeInequalityExpression):-
  inequality_expression(DerivativeInequalityExpression),
  rdfs_individual_of(DerivativeInequalityExpression, expression:derivative).

derivative_inequality_expression_definition(ExpressionDefinition):-
  inequality_expression_definition(ExpressionDefinition),
  rdfs_subclass_of(ExpressionDefinition, expression:derivative).

inequality(FromArgument, Relation, ToArgument, Expression):-
  inequality_expression(Expression),
  expression_from_argument(Expression, FromArgument),
  expression_to_argument(Expression, ToArgument),
  expression_definition_expression(Relation, Expression).

%% inequality(
%%   ?Space:space,
%%   ?FromArgument:quantity_or_quantity_value,
%%   ?Relation:expression_definition,
%%   ?ToArgument:quantity_or_quantity_value,
%%   ?Expression:expression,
%%   ?Point:point
%% ) is nondet.
% Temporally designated inequalities.
% If the space and both arguments are knwon, then this method is det.

inequality(Space, FromArgument, Relation, ToArgument, Expression, Point):-
  nonvar(Space),
  nonvar(FromArgument),
  nonvar(ToArgument),
  !,
  inequality_(Space, FromArgument, Relation, ToArgument, Expression, Point),
  !.
inequality(Space, FromArgument, Relation, ToArgument, Expression, Point):-
  inequality_(Space, FromArgument, Relation, ToArgument, Expression, Point).

inequality_(Space, FromArgument, Relation, ToArgument, Expression, Point):-
  point(Expression, FromArgument, Relation, ToArgument, Space, Point),
  inequality_expression(Expression).

%% inequality_expression(?InequalityExpression:expression) is nondet.
% An inequality expression.
%
% @param InequalityExpression An expression.

inequality_expression(InequalityExpression):-
  rdfs_individual_of(InequalityExpression, expression:inequality).

%% inequality_expression_definition(
%%   ?ExpressionDefinition:expression_definition
%% ) is semidet.
% Succeeds if the given expression definition is an inequality expression
% definition.
%
% @param ExpressionDefinition The URI of an expression definition.

inequality_expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(ExpressionDefinition, expression:inequality).

%% inequality_expression_definition_sign(
%%   ?ExpressionDefinition:expression_definition,
%%   ?Sign:sign
%% ) is nondet.

inequality_expression_definition_sign(ExpressionDefinition, Sign):-
  nonvar(ExpressionDefinition),
  !,
  inequality_expression_definition_sign_(ExpressionDefinition, Sign),
  !.
inequality_expression_definition_sign(ExpressionDefinition, Sign):-
  inequality_expression_definition_sign_(ExpressionDefinition, Sign).

% Either <, -, or >.
inequality_expression_definition_sign_(
  ExpressionDefinition,
  expression:greater_than
):-
  sign(Sign),
  rdfs_subclass_of(ExpressionDefinition, Sign).
% \leq and \geq are super classes of [< and =] and of [> and =] respectively.
% Therefore, they appear later in this file.
inequality_expression_definition_sign_(
  ExpressionDefinition,
  expression:greater_than_or_equal_to
):-
  rdfs_subclass_of(
    ExpressionDefinition,
    expression:greater_than_or_equal_to
  ).
inequality_expression_definition_sign_(
  ExpressionDefinition,
  expression:smaller_than_or_equal_to
):-
  rdfs_subclass_of(
    ExpressionDefinition,
    expression:smaller_than_or_equal_to
  ).

%% inequality_expression_sign(
%%   ?Expression:inequality_expression,
%%   ?Sign:sign
%% ) is nondet.

inequality_expression_sign(Expression, Sign):-
  nonvar(Expression),
  !,
  once(expression_definition_expression(ExpressionDefinition, Expression)),
  inequality_expression_definition_sign(ExpressionDefinition, Sign).
inequality_expression_sign(Expression, Sign):-
  inequality_expression_definition_sign(ExpressionDefinition, Sign),
  expression_definition_expression(ExpressionDefinition, Expression).

magnitude_inequality_expression(Expression):-
  inequality_expression(Expression),
  rdfs_individual_of(Expression, expression:magnitude).

magnitude_inequality_expression_definition(ExpressionDefinition):-
  inequality_expression_definition(ExpressionDefinition),
  rdfs_subclass_of(ExpressionDefinition, expression:magnitude).

%% quantity_derivative_sign(
%%   +Space:space,
%%   +Quantity:quantity,
%%   -Sign:sign
%% ) is det.
% Returns the inequality sign of the given quantity with respect to its
% steady derivative quantity value.

quantity_derivative_sign(Space, Quantity, Sign):-
  quantity_steady_derivative_quantity_value(Quantity, Steady),
  inequality(Space, Quantity, Relation, Steady, _Expression, _Point),
  inequality_expression_definition_sign(Relation, Sign).



% MAGNITUDE QUANTITY VALUES %

%% highest_magnitude_quantity_value(
%%   ?HighestMagnitudeQuantityValue:magnitude_quantity_value
%% ) is nondet.
% A highest magnitude quantity value.
%
% @param HighestMagnitudeQuantityValue A magnitude quantity value.

highest_magnitude_quantity_value(HighestMagnitudeQuantityValue):-
  magnitude_quantity_value(HighestMagnitudeQuantityValue),
  highest_quantity_value(HighestMagnitudeQuantityValue).

%% lowest_magnitude_quantity_value(
%%   ?LowestMagnitudeQuantityValue:magnitude_quantity_value
%% ) is nondet.
% A lowest magnitude quantity value.
%
% @param LowestMagnitudeQuantityValue A magnitude quantity value.

lowest_magnitude_quantity_value(LowestMagnitudeQuantityValue):-
  magnitude_quantity_value(LowestMagnitudeQuantityValue),
  lowest_quantity_value(LowestMagnitudeQuantityValue).

%% magntude_quantity_value(
%%   ?MagnitudeQuantityValue:magnitude_quantity_value
%% ) is nondet.
% Magnitude quantity values.
%
% @param MagnitudeQuantityValue A magnitude quantity value.

magnitude_quantity_value(MagnitudeQuantityValue):-
  quantity_value(MagnitudeQuantityValue),
  \+(derivative_quantity_value(MagnitudeQuantityValue)).

magnitude_quantity_value(Space, Quantity, MagnitudeQuantityValue):-
  magnitude_quantity_value(
    Space,
    Quantity,
    MagnitudeQuantityValue,
    _MagnitudeQuantityValueExpression
  ).

%% magnitude_quantity_value(
%%   ?Space:space,
%%   ?Quantity:quantity,
%%   ?MagnitudeQuantityValue:magnitude_quantity_value,
%%   ?MagnitudeQuantityValueExpression:expression
%% ) is nondet.
% Triples of spaces, quantities, and magnitude quantity values.
%
% @param Space A space.
% @param Quantity A quantity.
% @param MagnitudeQuantityValue The magnitude value of a quantity.
% @param MagnitudeQuantityValueExpression A magnitude quantity expression.

magnitude_quantity_value(
  Space,
  Quantity,
  MagnitudeQuantityValue,
  MagnitudeQuantityValueExpression
):-
  nonvar(Space),
  nonvar(Quantity),
  !,
  magnitude_quantity_value_(
    Space,
    Quantity,
    MagnitudeQuantityValue,
    MagnitudeQuantityValueExpression
  ),
  !.
magnitude_quantity_value(
  Space,
  Quantity,
  MagnitudeQuantityValue,
  MagnitudeQuantityValueExpression
):-
  magnitude_quantity_value_(
    Space,
    Quantity,
    MagnitudeQuantityValue,
    MagnitudeQuantityValueExpression
  ).

magnitude_quantity_value_(
  Space,
  Quantity,
  MagnitudeQuantityValue,
  MagnitudeQuantityValueExpression
):-
  magnitude_quantity_value_expression(MagnitudeQuantityValueExpression),
  from_quantity_expression(Quantity, MagnitudeQuantityValueExpression),
  space_expression(Space, MagnitudeQuantityValueExpression),
  expression_to_argument(
    MagnitudeQuantityValueExpression,
    MagnitudeQuantityValue
  ).

%% magnitude_quantity_value_expression(
%%   ?MagnitudeQuantityValueExpression:expression
%% ) is nondet.
% Magnitude value expressions.
%
% @param MagnitudeQuantityValueExpression

magnitude_quantity_value_expression(MagnitudeQuantityValueExpression):-
  rdfs_individual_of(
    MagnitudeQuantityValueExpression,
    expression:qp_magnitude_equal_to
  ).

%% magnitude_quantity_value_expression_definition(
%%   ?MagnitudeQuantityValueExpressionDefinition:expression_definition
%% ) is nondet.
% Magnitude value expression definitions.
%
% @param MagnitudeQuantityValueExpressionDefinition An expression definition.

magnitude_quantity_value_expression_definition(
  MagnitudeQuantityValueExpressionDefinition
):-
  rdfs_subclass_of(
    MagnitudeQuantityValueExpressionDefinition,
    expression:qp_magnitude_equal_to
  ).

magnitude_quantity_value_expression_sign(
  MagnitudeQuantityValueExpression,
  Sign
):-
  expression_to_argument(
    MagnitudeQuantityValueExpression,
    MagnitudeQuantityValue
  ),
  magnitude_quantity_value_sign(MagnitudeQuantityValue, Sign).

magnitude_quantity_value_point(Space, Quantity, Point):-
  point(Expression, Space, Point),
  magnitude_quantity_value_expression(Expression),
  from_quantity_expression(Quantity, Expression).

magnitude_quantity_value_sign(MagnitudeQuantityValue, Sign):-
  nonvar(MagnitudeQuantityValue),
  !,
  magnitude_quantity_value_sign_(MagnitudeQuantityValue, Sign),
  !.
magnitude_quantity_value_sign(MagnitudeQuantityValue, Sign):-
  magnitude_quantity_value_sign_(MagnitudeQuantityValue, Sign).

magnitude_quantity_value_sign_(MagnitudeQuantityValue, Sign):-
  positive_magnitude_quantity_value(MagnitudeQuantityValue),
  !,
  greater_than(Sign).
magnitude_quantity_value_sign_(MagnitudeQuantityValue, Sign):-
  negative_magnitude_quantity_value(MagnitudeQuantityValue),
  !,
  smaller_than(Sign).
magnitude_quantity_value_sign_(MagnitudeQuantityValue, Sign):-
  zero_magnitude_quantity_value(MagnitudeQuantityValue),
  equal_to(Sign).

% A quantity pairs determine a magnitude quantity value location.
magnitude_quantity_value_location(Quantity, Location):-
  nonvar(Quantity),
  !,
  magnitude_quantity_value_location_(Quantity, Location),
  !.
magnitude_quantity_value_location(Quantity, Location):-
  magnitude_quantity_value_location_(Quantity, Location).

magnitude_quantity_value_location_(Quantity, Location):-
  expression_point_cloud(Expression, Location),
  expression_from_argument(Expression, Quantity),
  magnitude_quantity_value_expression(Expression).

negative_magnitude_expression(Expression):-
  (
    magnitude_quantity_value_expression(Expression)
  ;
    rdfs_individual_of(
      Expression,
      expression:qp_magnitude_smaller_than_or_equal_to
    )
  ),
  expression_to_argument(Expression, MagnitudeQuantityValue),
  negative_magnitude_quantity_value(MagnitudeQuantityValue).
negative_magnitude_expression(Expression):-
  rdfs_individual_of(Expression, expression:qp_magnitude_smaller_than),
  expression_to_argument(Expression, MagnitudeQuantityValue),
  (
    zero_magnitude_quantity_value(MagnitudeQuantityValue)
  ;
    negative_magnitude_quantity_value(MagnitudeQuantityValue)
  ).

negative_magnitude_quantity_value(NegativeMagnitudeValue):-
  negative_quantity_value(NegativeMagnitudeValue),
  magnitude_quantity_value(NegativeMagnitudeValue).

positive_magnitude_expression(Expression):-
  (
    magnitude_quantity_value_expression(Expression)
  ;
    rdfs_individual_of(
      Expression,
      expression:qp_magnitude_greater_than_or_equal_to
    )
  ),
  expression_to_argument(Expression, MagnitudeQuantityValue),
  positive_magnitude_quantity_value(MagnitudeQuantityValue).
positive_magnitude_expression(Expression):-
  rdfs_individual_of(Expression, expression:qp_magnitude_greater_than),
  expression_to_argument(Expression, MagnitudeQuantityValue),
  (
    zero_magnitude_quantity_value(MagnitudeQuantityValue)
  ;
    positive_magnitude_quantity_value(MagnitudeQuantityValue)
  ).

positive_magnitude_quantity_value(PositiveMagnitudeValue):-
  positive_quantity_value(PositiveMagnitudeValue),
  magnitude_quantity_value(PositiveMagnitudeValue).

zero_magnitude_expression(Expression):-
  magnitude_quantity_value_expression(Expression),
  expression_to_argument(Expression, MagnitudeQuantityValue),
  zero_magnitude_quantity_value(MagnitudeQuantityValue).

zero_magnitude_quantity_value(ZeroMagnitudeValue):-
  zero_quantity_value(ZeroMagnitudeValue),
  magnitude_quantity_value(ZeroMagnitudeValue).


% QUANTITIES %

%% from_quantity_expression(
%%   ?FromQuantity:quantity,
%%   ?Expression:expression
%% ) is nondet.
% The relation between an expression and its from quantity, if any.
%
% @param FromQuantity A quantity.
% @param Expression An expression.

from_quantity_expression(FromQuantity, Expression):-
  rdf(Expression, expression:has_from_argument, FromQuantity, ccm),
  quantity(FromQuantity).

quantities(Quantities):-
  setoff(
    Quantity,
    quantity(Quantity),
    Quantities
  ).

%% quantity(?Quantity:quantity) is nondet.
% Quantities.
%
% @param Quantity A quantity.

quantity(Quantity):-
  rdfs_individual_of(Quantity, quantity:quantity).

quantity(Entity, QuantityName, Quantity):-
  nonvar(Entity),
  nonvar(QuantityName),
  !,
  quantity_(Entity, QuantityName, Quantity),
  !.
quantity(Entity, QuantityName, Quantity):-
  nonvar(Quantity),
  !,
  quantity_(Entity, QuantityName, Quantity),
  !.
quantity(Entity, QuantityName, Quantity):-
  quantity_(Entity, QuantityName, Quantity).

quantity_(Entity, QuantityName, Quantity):-
  entity_quantity(Entity, Quantity),
  quantity_name(Quantity, QuantityName).

quantity_decreasing_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  var(Quantity),
  var(DerivativeQuantityValue),
  !,
  quantity_decreasing_derivative_quantity_value_(
    Quantity,
    DerivativeQuantityValue
  ).
quantity_decreasing_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_decreasing_derivative_quantity_value_(
    Quantity,
    DerivativeQuantityValue
  ),
  % Between quantities and decreasing quantity values there is a
  % one-to-one mapping.
  !.

quantity_decreasing_derivative_quantity_value_(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_derivative_quantity_value(Quantity, DerivativeQuantityValue),
  decreasing_derivative_quantity_value(DerivativeQuantityValue).

%% quantity_derivative_quantity_space(
%%   ?Quantity:quantity,
%%   ?DerivativeQuantitySpace:derivative_quantity_space
%% ) is nondet.
% Return the derivative quantity space of the given =Quantity=.
%
% @param Quantity A quantity.
% @param DerivativeQuantitySpace A derivative quantity space.

quantity_derivative_quantity_space(Quantity, DerivativeQuantitySpace):-
  var(Quantity),
  var(DerivativeQuantitySpace),
  !,
  % The generative case, so non-deterministic.
  quantity_derivative_quantity_space1(Quantity, DerivativeQuantitySpace).
quantity_derivative_quantity_space(Quantity, DerivativeQuantitySpace):-
  quantity_derivative_quantity_space1(Quantity, DerivativeQuantitySpace),
  % There is a one-to-one correspondence between quantities and
  % derivative quantity spaces, so this is deterministic.
  !.

quantity_derivative_quantity_space1(Quantity, DerivativeQuantitySpace):-
  rdf(
    Quantity,
    quantity:has_derivative_quantity_space,
    DerivativeQuantitySpace,
    ccm
  ).

quantity_derivative_quantity_value(Quantity, DerivativeQuantityValue):-
  quantity_quantity_value(Quantity, DerivativeQuantityValue),
  derivative_quantity_value(DerivativeQuantityValue).

quantity_engine_name(Quantity, QuantityEngineName):-
  var(Quantity),
  var(QuantityEngineName),
  !,
  quantity_engine_name1(Quantity, QuantityEngineName).
quantity_engine_name(Quantity, QuantityEngineName):-
  quantity_engine_name1(Quantity, QuantityEngineName),
  % There is a one-to-one mapping between quantities and their engine names.
  !.

quantity_engine_name1(Quantity, QuantityEngineName):-
  rdf_literal(Quantity, quantity:has_engine_name, QuantityEngineName, ccm).

%% quantity_expression(Quantity, Expression) is nondet.
% Pairs of quantities and expressions.
%
% @param Quantity A quantity.
% @param Expression An expression.

quantity_expression(Quantity, Expression):-
  from_quantity_expression(Quantity, Expression).
quantity_expression(Quantity, Expression):-
  to_quantity_expression(Quantity, Expression).

%% quantity_id(?Quantity:quantity, ?QuantityID:number) is nondet.
% Pairs of quantities and their identifiers.
%
% @param Quantity A quantity.
% @param QuantityID A numberic identifier of a quantity.

quantity_id(Quantity, QuantityID):-
  nonvar(Quantity),
  !,
  quantity_id1(Quantity, QuantityID),
  % A quantity has exactly one identifier.
  !.
quantity_id(Quantity, QuantityID):-
  quantity_id1(Quantity, QuantityID).

quantity_id1(Quantity, QuantityID):-
  rdf_datatype(Quantity, quantity:has_id, integer, QuantityID, ccm).

quantity_increasing_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  var(Quantity),
  var(DerivativeQuantityValue),
  !,
  quantity_increasing_derivative_quantity_value_(
    Quantity,
    DerivativeQuantityValue
  ).
quantity_increasing_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_increasing_derivative_quantity_value_(
    Quantity,
    DerivativeQuantityValue
  ),
  !.

quantity_increasing_derivative_quantity_value_(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_derivative_quantity_value(Quantity, DerivativeQuantityValue),
  increasing_derivative_quantity_value(DerivativeQuantityValue).

%% quantity_label(+Quantity:quantity, -QuantityLabel:atom) is det.
% Returns the natural language label of the given quantity.
%
% @param Quantity A quantity.
% @param QuantityLabel The atomic natural language label of a quantity.

quantity_label(Quantity, QuantityLabel):-
  entity_quantity(Entity, Quantity),
  % A quantity has exactly one entity.
  !,
  quantity_name(Quantity, QuantityName1),
  underscores_to_spaces(QuantityName1, QuantityName),
  entity_label(Entity, EntityLabel),
  format(atom(QuantityLabel), '~w\'s ~w', [EntityLabel, QuantityName]).

%% quantity_magnitude_quantity_space(
%%   ?Quantity:quantity,
%%   ?MagnitudeQuantitySpace:magnitude_quantity_space
%% ) is nondet.
% Returns the magnitude quantity space of the given quantity.
%
% @param Quantity A quantity.
% @param MagnitudeQuantitySpace A magnitude quantity space.

quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace):-
  var(Quantity),
  var(MagnitudeQuantitySpace),
  !,
  quantity_magnitude_quantity_space1(Quantity, MagnitudeQuantitySpace).
quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace):-
  quantity_magnitude_quantity_space1(Quantity, MagnitudeQuantitySpace),
  % There is a one-to-one correspondence between quantities and
  % magnitude quantity spaces.
  !.

quantity_magnitude_quantity_space1(Quantity, MagnitudeQuantitySpace):-
  rdf(
    Quantity,
    quantity:has_magnitude_quantity_space,
    MagnitudeQuantitySpace,
    ccm
  ).

quantity_magnitude_quantity_value(Quantity, MagnitudeQuantityValue):-
  nonvar(MagnitudeQuantityValue),
  !,
  quantity_magnitude_quantity_value1(Quantity, MagnitudeQuantityValue),
  !.
quantity_magnitude_quantity_value(Quantity, MagnitudeQuantityValue):-
  quantity_magnitude_quantity_value1(Quantity, MagnitudeQuantityValue).

quantity_magnitude_quantity_value1(Quantity, MagnitudeQuantityValue):-
  quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace),
  quantity_space_quantity_value(MagnitudeQuantitySpace, MagnitudeQuantityValue).

%% quantity_name(?Quantity:quantity, ?QuantityName:atom) is nondet.
% Pairs of quantities and their names.
%
% @param Quantity A quantity.
% @param QuantityName The atomic name of a quantity.

quantity_name(Quantity, QuantityName):-
  nonvar(Quantity),
  !,
  quantity_name1(Quantity, QuantityName),
  !.
quantity_name(Quantity, QuantityName):-
  quantity(Quantity),
  quantity_name1(Quantity, QuantityName).

quantity_name1(Quantity, QuantityName):-
  rdfs_label(Quantity, QuantityName).

quantity_negative_magnitude_quantity_value(Quantity, NegativeMagnitudeValue):-
  quantity_quantity_value(Quantity, NegativeMagnitudeValue),
  negative_magnitude_quantity_value(NegativeMagnitudeValue).

quantity_positive_magnitude_quantity_value(Quantity, PositiveMagnitudeValue):-
  quantity_quantity_value(Quantity, PositiveMagnitudeValue),
  positive_magnitude_quantity_value(PositiveMagnitudeValue).

%% quantity_quantity_space(
%%   ?Quantity:quantity,
%%   ?QuantitySpace:quantity_space
%% ) is nondet.
% Pairs of quantities and quantity spaces.
%
% @param Quantity A quantity.
% @param QuantitySpace A quantity space.

quantity_quantity_space(Quantity, QuantitySpace):-
  quantity_derivative_quantity_space(Quantity, QuantitySpace).
quantity_quantity_space(Quantity, QuantitySpace):-
  quantity_magnitude_quantity_space(Quantity, QuantitySpace).

%% quantity_quantity_value(
%%   ?QuantityValue:quantity_value,
%%   ?Quantity:quantity
%% ) is nondet.
% Pairs of quantities and quantity values.
%
% @param Quantity A quantity.
% @param QuantityValue A quantity value.

quantity_quantity_value(Quantity, QuantityValue):-
  nonvar(QuantityValue),
  !,
  quantity_quantity_value1(Quantity, QuantityValue),
  !.
quantity_quantity_value(Quantity, QuantityValue):-
  quantity_quantity_value1(Quantity, QuantityValue).

quantity_quantity_value1(Quantity, QuantityValue):-
  quantity_quantity_space(Quantity, QuantitySpace),
  quantity_space_quantity_value(QuantitySpace, QuantityValue).

quantity_quantity_value(Quantity, QuantityValueName, QuantityValue):-
  nonvar(Quantity),
  nonvar(QuantityValueName),
  !,
  quantity_quantity_value1(Quantity, QuantityValueName, QuantityValue),
  % Assume uniqueness.
  !.
quantity_quantity_value(Quantity, QuantityValueName, QuantityValue):-
  quantity_quantity_value1(Quantity, QuantityValueName, QuantityValue).

quantity_quantity_value1(Quantity, QuantityValueName, QuantityValue):-
  quantity_quantity_value(Quantity, QuantityValue),
  quantity_value_label(QuantityValue, QuantityValueName).

%% quantity_space(?Quantity:quantity, ?Space:space) is nondet.
% Pairs of quantities and spaces.
%
% @param Quantity A quantity.
% @param Space A space.

quantity_space(Quantity, Space):-
  nonvar(Quantity),
  !,
  quantity_expression(Quantity, Expression),
  expression_space(Expression, Space).
quantity_space(Quantity, Space):-
  expression_space(Expression, Space),
  quantity_expression(Quantity, Expression).

%% quantity_state(?Quantity:quantity, ?State:state) is nondet.
% Pairs of quantities and states.
%
% @param Quantity A quantity.
% @param State A state.

quantity_state(Quantity, State):-
  quantity_space(Quantity, State),
  state(State).

quantity_steady_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  var(Quantity),
  var(DerivativeQuantityValue),
  !,
  quantity_derivative_quantity_value_steady_(
    Quantity,
    DerivativeQuantityValue
  ).
quantity_steady_derivative_quantity_value(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_derivative_quantity_value_steady_(
    Quantity,
    DerivativeQuantityValue
  ),
  !.

quantity_derivative_quantity_value_steady_(
  Quantity,
  DerivativeQuantityValue
):-
  quantity_derivative_quantity_value(Quantity, DerivativeQuantityValue),
  steady_derivative_quantity_value(DerivativeQuantityValue).

%% quantity_to_dot_name(+Quantity:quantity, -QuantityDOTName:atom) is det.
% Returns the name that is used for exporting the quantity to the
% GraphViz DOT format.
%
% @param Quantity A quantity.
% @param QuantityDOTName The atomic DOT export name of a quantity.

quantity_to_dot_name(Quantity, QuantityDOTName):-
  quantity_id(Quantity, QuantityID),
  format(atom(QuantityDOTName), 'q_~w', [QuantityID]).

quantity_zero_magnitude_quantity_value(Quantity, ZeroMagnitudeQuantityValue):-
  quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace),
  quantity_space_zero_quantity_value(
    MagnitudeQuantitySpace,
    ZeroMagnitudeQuantityValue
  ).

%% to_quantity_expression(
%%   ?ToQuantity:quantity,
%%   ?Expression:expression
%% ) is nondet.
% Pairs of quantities and expressions in which the quantity is the second
% argument.
%
% @param ToQuantity A quantity.
% @param Expression An expression.

to_quantity_expression(ToQuantity, Expression):-
  rdf(Expression, expression:has_to_argument, ToQuantity, ccm),
  quantity(ToQuantity).



% QUANTITY CAUSALITIES %

branching_quantity_causality(FromQuantity, Triples):-
  branching_quantity_influence(FromQuantity, Triples).
branching_quantity_causality(FromQuantity, Triples):-
  branching_quantity_proportionality(FromQuantity, Triples).

%% negative_quantity_causality_expression(?Expression:expression) is nondet.
% Negative quantity causality expressions.
%
% @param Expression

negative_quantity_causality_expression(Expression):-
  negative_quantity_influence_expression(Expression).
negative_quantity_causality_expression(Expression):-
  negative_quantity_proportionality_expression(Expression).

negative_quantity_causality_expression_definition(ExpressionDefinition):-
  negative_quantity_influence_expression_definition(ExpressionDefinition).
negative_quantity_causality_expression_definition(ExpressionDefinition):-
  negative_quantity_proportionality_expression_definition(
    ExpressionDefinition
  ).

%% positive_quantity_causality_expression(?Expression:expression) is nondet.
% Positive quantity causality expressions.
%
% @param Expression

positive_quantity_causality_expression(Expression):-
  positive_quantity_influence_expression(Expression).
positive_quantity_causality_expression(Expression):-
  positive_quantity_proportionality_expression(Expression).

positive_quantity_causality_expression_definition(ExpressionDefinition):-
  positive_quantity_influence_expression_definition(ExpressionDefinition).
positive_quantity_causality_expression_definition(ExpressionDefinition):-
  positive_quantity_proportionality_expression_definition(ExpressionDefinition).

quantity_causality(
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud
):-
  quantity_influence(
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud
  ).
quantity_causality(
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud
):-
  quantity_proportionality(
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud
  ).

quantity_causality(
  State,
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud,
  Point
):-
  quantity_influence(
    State,
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud,
    Point
  ).
quantity_causality(
  State,
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud,
  Point
):-
  quantity_proportionality(
    State,
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud,
    Point
  ).

%% quantity_causality_component(?Component:component) is nondet.
% Quantity causal components, i.e. quantity propostionality and/or
% quantity influence components.
%
% @param Component

quantity_causality_component(Component):-
  quantity_influence_component(Component).
quantity_causality_component(Component):-
  quantity_proportionality_component(Component).

quantity_causality_expression(Expression):-
  quantity_influence_expression(Expression).
quantity_causality_expression(Expression):-
  quantity_proportionality_expression(Expression).

%% quantity_causality_expression_definition_sign(
%%   ?ExpressionDefinition:expression_definition,
%%   ?Sign:sign
%% ) is nondet.
% Causal expression definitions and their signs.

quantity_causality_expression_definition_sign(ExpressionDefinition, Sign):-
  nonvar(ExpressionDefinition),
  !,
  quantity_causality_expression_definition_sign_(ExpressionDefinition, Sign),
  !.
quantity_causality_expression_definition_sign(ExpressionDefinition, Sign):-
  quantity_causality_expression_definition_sign_(ExpressionDefinition, Sign).

quantity_causality_expression_definition_sign_(ExpressionDefinition, Sign):-
  positive_quantity_causality_expression_definition(ExpressionDefinition),
  !,
  greater_than(Sign).
quantity_causality_expression_definition_sign_(ExpressionDefinition, Sign):-
  negative_quantity_causality_expression_definition(ExpressionDefinition),
  smaller_than(Sign).

%% quantity_causality_expression_sign(
%%   ?Expression:expression,
%%   ?Sign:sign
%% ) is nondet.
% Causal expressions and their signs.

quantity_causality_expression_sign(Expression, Sign):-
  nonvar(Expression),
  !,
  once(expression_definition_expression(ExpressionDefinition, Expression)),
  quantity_causality_expression_definition_sign(ExpressionDefinition, Sign),
  !.
quantity_causality_expression_sign(Expression, Sign):-
  quantity_causality_expression_definition_sign(ExpressionDefinition, Sign),
  expression_definition_expression(ExpressionDefinition, Expression).

quantity_causality_path(
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud
):-
  quantity_causality(
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud
  ),
  \+(subsumed_expression(Expression)).

quantity_causality_path(
  State,
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud,
  Point
):-
  quantity_causality_path(
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud
  ),
  point_cloud(_Expression, State, Point, PointCloud).



% QUANTITY CORRESPONDENCE %

bidirectional_magnitude_quantity_space_correspondence(
  FromQuantity,
  ToQuantity
):-
  magnitude_quantity_space_correspondence(
    FromQuantity,
    ToQuantity,
    Expression
  ),
  bidirectional_expression(Expression).

bidirectional_magnitude_quantity_value_correspondence(
  FromMagnitudeQuantityValue,
  ToMagnitudeQuantityValue
):-
  magnitude_quantity_value_correspondence(
    FromMagnitudeQuantityValue,
    ToMagnitudeQuantityValue,
    Expression
  ),
  bidirectional_expression(Expression).

derivative_quantity_space_correspondence_expression(Expression):-
  quantity_space_correspondence_expression(Expression),
  rdfs_individual_of(Expression, expression:derivative).

inverted_magnitude_quantity_space_correspondence(FromQuantity, ToQuantity):-
  magnitude_quantity_space_correspondence(
    FromQuantity,
    ToQuantity,
    Expression
  ),
  inverted_expression(Expression).

inverted_magnitude_quantity_space_correspondence_expression(Expression):-
  rdfs_individual_of(Expression, expression:inverted),
  quantity_space_correspondence_expression(Expression).

magnitude_quantity_space_correspondence(FromQuantity, ToQuantity):-
  magnitude_quantity_space_correspondence(
    FromQuantity,
    ToQuantity,
    _Expression
  ).

magnitude_quantity_space_correspondence(
  FromQuantity,
  ToQuantity,
  Expression
):-
  quantity_space_correspondence_expression(Expression),
  from_quantity_expression(FromQuantity, Expression),
  to_quantity_expression(ToQuantity, Expression).

magnitude_quantity_space_correspondence_expression(Expression):-
  rdfs_individual_of(Expression, expression:magnitude),
  quantity_space_correspondence_expression(Expression).

magnitude_quantity_value_correspondence(
  FromMagnitudeQuantityValue,
  ToMagnitudeQuantityValue
):-
  magnitude_quantity_value_correspondence(
    FromMagnitudeQuantityValue,
    ToMagnitudeQuantityValue,
    _Expression
  ).

magnitude_quantity_value_correspondence(
  FromMagnitudeQuantityValue,
  ToMagnitudeQuantityValue,
  Expression
):-
  quantity_value_correspondence_expression(Expression),
  rdfs_individual_of(Expression, expression:magnitude),
  expression_from_argument(Expression, FromMagnitudeQuantityValue),
  expression_to_argument(Expression, ToMagnitudeQuantityValue).

quantity_correspondence_expression(Expression):-
  quantity_space_correspondence_expression(Expression).
quantity_correspondence_expression(Expression):-
  quantity_value_correspondence_expression(Expression).

quantity_space_correspondence_expression(Expression):-
  rdfs_individual_of(Expression, expression:quantity_space_correspondence).

quantity_space_correspondence_expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(
    ExpressionDefinition,
    expression:quantity_space_correspondence
  ).

quantity_value_correspondence_expression(Expression):-
  rdfs_individual_of(Expression, expression:quantity_value_correspondence).

quantity_value_correspondence_expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(
    ExpressionDefinition,
    expression:quantity_value_correspondence
  ).

unidirectional_magnitude_quantity_space_correspondence(
  FromQuantity,
  ToQuantity
):-
  magnitude_quantity_space_correspondence(
    FromQuantity,
    ToQuantity,
    Expression
  ),
  unidirectional_expression(Expression).

unidirectional_magnitude_quantity_value_correspondence(
  FromMagnitudeQuantityValue,
  ToMagnitudeQuantityValue
):-
  magnitude_quantity_value_correspondence(
    FromMagnitudeQuantityValue,
    ToMagnitudeQuantityValue,
    Expression
  ),
  unidirectional_expression(Expression).

uninverted_magnitude_quantity_space_correspondence(FromQuantity, ToQuantity):-
  magnitude_quantity_space_correspondence(
    FromQuantity,
    ToQuantity,
    Expression
  ),
  uninverted_expression(Expression).



% QUANTITY DEFINITIONS %

quantity_definition(QuantityDefinition):-
  rdfs_subclass_of(QuantityDefinition, quantity:quantity).

quantity_definition_derivative_quantity_space_definition(
  QuantityDefinition,
  DerivativeQuantitySpaceDefinition
):-
  rdf(
    QuantityDefinition,
    quantity:has_derivative_quantity_space,
    DerivativeQuantitySpaceDefinition,
    ccm
  ).

quantity_definition_id(QuantityDefinition, QuantityDefinitionID):-
  nonvar(QuantityDefinition),
  !,
  quantity_definition_id1(QuantityDefinition, QuantityDefinitionID),
  % A quantity definition has exactly one identifier.
  !.
quantity_definition_id(QuantityDefinition, QuantityDefinitionID):-
  quantity_definition_id1(QuantityDefinition, QuantityDefinitionID).

quantity_definition_id1(QuantityDefinition, QuantityDefinitionID):-
  rdf_datatype(QuantityDefinition, quantity:has_id, integer, QuantityDefinitionID, ccm).

quantity_definition_label(QuantityDefinition, QuantityDefinitionLabel):-
  var(QuantityDefinition),
  var(QuantityDefinitionLabel),
  !,
  quantity_definition(QuantityDefinition),
  quantity_definition_label_(QuantityDefinition, QuantityDefinitionLabel).
quantity_definition_label(QuantityDefinition, QuantityDefinitionLabel):-
  quantity_definition_label_(QuantityDefinition, QuantityDefinitionLabel),
  !.

quantity_definition_label_(QuantityDefinition, QuantityDefinitionLabel):-
  rdfs_label(QuantityDefinition, QuantityDefinitionLabel).

quantity_definition_magnitude_quantity_space_definition(
  QuantityDefinition,
  MagnitudeQuantitySpaceDefinition
):-
  rdf(
    QuantityDefinition,
    quantity:has_magnitude_quantity_space,
    MagnitudeQuantitySpaceDefinition,
    ccm
  ).

quantity_definition_quantity(QuantityDefinition, Quantity):-
  nonvar(Quantity),
  !,
  quantity_definition_quantity1(QuantityDefinition, Quantity).
quantity_definition_quantity(QuantityDefinition, Quantity):-
  quantity_definition(QuantityDefinition),
  quantity_definition_quantity1(QuantityDefinition, Quantity).

quantity_definition_quantity1(QuantityDefinition, Quantity):-
  rdfs_individual_of(Quantity, QuantityDefinition),
  quantity_definition(QuantityDefinition).

quantity_definition_quantity_space_definition(
  QuantityDefinition,
  QuantitySpaceDefinition
):-
  var(QuantityDefinition),
  var(QuantitySpaceDefinition),
  !,
  quantity_definition(QuantityDefinition),
  quantity_definition_quantity_space_definition_(
    QuantityDefinition,
    QuantitySpaceDefinition
  ).
quantity_definition_quantity_space_definition(
  QuantityDefinition,
  QuantitySpaceDefinition
):-
  quantity_definition_quantity_space_definition_(
    QuantityDefinition,
    QuantitySpaceDefinition
  ),
  !.

quantity_definition_quantity_space_definition_(
  QuantityDefinition,
  QuantitySpaceDefinition
):-
  rdfs(
    QuantityDefinition,
    quantity:has_quantity_space,
    QuantitySpaceDefinition
  ).

quantity_definition_to_dot_name(
  QuantityDefinition,
  QuantityDefinitionDOTName
):-
  quantity_definition_id(QuantityDefinition, QuantityDefinitionID),
  format(atom(QuantityDefinitionDOTName), 'qd_~w', [QuantityDefinitionID]).

quantity_definition_to_quantities(QuantityDefinition, Quantities):-
  setoff(
    Quantity,
    quantity_definition_quantity(QuantityDefinition, Quantity),
    Quantities
  ).

quantity_definitions(QuantityDefinitions):-
  setoff(
    QuantityDefinition,
    quantity_definition(QuantityDefinition),
    QuantityDefinitions
  ).



% QUANTITY FEEDBACK %

direct_quantity_feedback(Quantity, Expression, SubsumedExpressions):-
  % Direct quantity influences are those with cardinality 2.
  quantity_feedback(Quantity, Expression, SubsumedExpressions, 2).

double_quantity_feedback(Quantity, Expression, SubsumedExpressions):-
  quantity_feedback(Quantity, Expression, SubsumedExpressions, _Length),
  length(SubsumedExpressions, 4).

indirect_quantity_feedback(Quantity, Expression, SubsumedExpressions):-
  % Indirect quantity influences are those with cardinality >2.
  quantity_feedback(Quantity, Expression, SubsumedExpressions, Length),
  Length > 2.

quantity_feedback(Quantity, Expression, SubsumedExpressions, Length):-
  quantity_influence(Quantity, _Relation, Quantity, Expression, _Pointcloud),
  component_support_expression(Component, Expression),
  component_cardinality(Component, Length),
  expression_to_subsumed_expressions(Expression, SubsumedExpressions).

singular_quantity_feedback(Quantity, Expression, SubsumedExpressions):-
  quantity_feedback(Quantity, Expression, SubsumedExpressions, _Length),
  length(SubsumedExpressions, 2).



% QUANTITY INFLUENCES %

branching_quantity_influence(FromQuantity, Triples):-
  quantity(FromQuantity),
  setoff(
    ToQuantity/Expression/PointCloud,
    (
      quantity_influence(
        FromQuantity,
        _Relation,
        ToQuantity,
        Expression,
        PointCloud
      ),
      \+(subsumed_expression(Expression))
    ),
    Triples
  ).

negative_quantity_influence_expression(Expression):-
  rdfs_individual_of(Expression, expression:negative_quantity_influence).

negative_quantity_influence_expression_definition(ExpressionDefinition):-
  rdf_global_id(expression:negative_quantity_influence, ExpressionDefinition).

positive_quantity_influence_expression(Expression):-
  rdfs_individual_of(Expression, expression:positive_quantity_influence).

positive_quantity_influence_expression_definition(ExpressionDefinition):-
  rdf_global_id(expression:positive_quantity_influence, ExpressionDefinition).

quantity_influence(
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud
):-
  quantity_influence(
    _State,
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud,
    _Point
  ).

quantity_influence(
  State,
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud,
  Point
):-
  quantity_influence_expression_definition(Relation),
  point_cloud(
    Expression,
    FromQuantity,
    Relation,
    ToQuantity,
    State,
    Point,
    PointCloud
  ).

quantity_influence_component(Component):-
  rdfs_individual_of(Component, component:quantity_influence).

quantity_influence_component_cloud(ComponentCloud):-
 component_cloud_component(ComponentCloud, Component),
  quantity_influence_component(Component).

%% quantity_influence_expression(?Expression:expression) is nondet.
% Quantity influence expressions.
%
% @param Expression A quantity influence expression.

quantity_influence_expression(Expression):-
  rdfs_individual_of(Expression, expression:quantity_influence).

quantity_influence_expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(ExpressionDefinition, expression:quantity_influence).



% QUANTITY PROPORTIONALITIES %

branching_quantity_proportionality(FromQuantity, Triples):-
  quantity(FromQuantity),
  setoff(
    ToQuantity/Expression/PointCloud,
    (
      quantity_proportionality(
        FromQuantity,
        _Relation,
        ToQuantity,
        Expression,
        PointCloud
      ),
      \+(subsumed_expression(Expression))
    ),
    Triples
  ).

negative_quantity_proportionality_expression(
  NegativeQuantityProportionalityExpression
):-
  rdfs_individual_of(
    NegativeQuantityProportionalityExpression,
    expression:negative_quantity_proportionality
  ).

negative_quantity_proportionality_expression_definition(
  NegativeQuantityProportionalityExpressionDefinition
):-
  rdf_global_id(
    expression:negative_quantity_proportionality,
    NegativeQuantityProportionalityExpressionDefinition
  ).

positive_quantity_proportionality_expression(
  PositiveQuantityProportionalityExpression
):-
  rdfs_individual_of(
    PositiveQuantityProportionalityExpression,
    expression:positive_quantity_proportionality
  ).

positive_quantity_proportionality_expression_definition(
  PositiveQuantityProportionalityExpressionDefinition
):-
  rdf_global_id(
    expression:positive_quantity_proportionality,
    PositiveQuantityProportionalityExpressionDefinition
  ).

%% quantity_proportionality(
%%   ?FromQuantity:quantity,
%%   ?Relation:expression_definition,
%%   ?ToQuantity:quantity,
%%   ?Expression:expression,
%%   ?PointCloud:point_cloud
%% ) is nondet.

quantity_proportionality(
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud
):-
  quantity_proportionality_expression_definition(Relation),
  point_cloud(
    Expression,
    FromQuantity,
    Relation,
    ToQuantity,
    _Space,
    _Point,
    PointCloud
  ).

quantity_proportionality(
  State,
  FromQuantity,
  Relation,
  ToQuantity,
  Expression,
  PointCloud,
  Point
):-
  quantity_proportionality(
    FromQuantity,
    Relation,
    ToQuantity,
    Expression,
    PointCloud
  ),
  point(Expression, State, Point).

quantity_proportionality_component(QuantityProportionalityComponent):-
  rdfs_individual_of(
    QuantityProportionalityComponent,
    component:quantity_proportionality
  ).

quantity_proportionality_component_cloud(ComponentCloud):-
  component_cloud_component(ComponentCloud, Component),
  quantity_proportionality_component(Component).

quantity_proportionality_component_definition(
  QuantityProportionalityComponentDefinition
):-
  rdfs_subclass_of(
    QuantityProportionalityComponentDefinition,
    component:quantity_proportionality
  ).

%% quantity_proportionality_expression(?Expression:expression) is nondet.
% Quantity proportionality expressions.
%
% @param Expression A quantity proportionality expression.

quantity_proportionality_expression(Expression):-
  rdfs_individual_of(Expression, expression:quantity_proportionality).

quantity_proportionality_expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(ExpressionDefinition, expression:quantity_proportionality).



% QUANTITY SPACES %

%% derivative_quantity_space(
%%   ?DerivativeQuantitySpace:derivative_quantity_space
%% ) is nondet.
% Derivative quantity space.
%
% @param DerivativeQuantitySpace A derivative quantity space.

derivative_quantity_space(DerivativeQuantitySpace):-
  rdfs_individual_of(
    DerivativeQuantitySpace,
    quantity_space:derivative_quantity_space
  ).

derivative_quantity_space_definition(DerivativeQuantitySpaceDefinition):-
  rdf_global_id(
    quantity_space:derivative_quantity_space,
    DerivativeQuantitySpaceDefinition
  ).

%% derivative_quantity_spaces(
%%   ?DerivativeQuantitySpaces:ord_set(derivative_quantity_space)
%% ) is nondet.
% Returns all derivative quantity spaces.
%
% @param DerivativeQuantitySpaces The ordered set of
%        derivative quantity spaces.

derivative_quantity_spaces(DerivativeQuantitySpaces):-
  setoff(
    DerivativeQuantitySpace,
    derivative_quantity_space(DerivativeQuantitySpace),
    DerivativeQuantitySpaces
  ).

%% magnitude_quantity_space(
%%   ?MagnitudeQuantitySpace:magnitude_quantity_space
%% ) is nondet.
% Magnitude quantity space.
%
% @param MagnitudeQuantitySpace A magnitude quantity space.

magnitude_quantity_space(MagnitudeQuantitySpace):-
  rdfs_individual_of(
    MagnitudeQuantitySpace,
    quantity_space:magnitude_quantity_space
  ).

%% magnitude_quantity_spaces(
%%   ?MagnitudeQuantitySpaces:ord_set(magnitude_quantity_space)
%% ) is nondet.
% Returns all magnitude quantity spaces.
%
% @param MagnitudeQuantitySpaces The ordered set of
%        magnitude quantity spaces.

magnitude_quantity_spaces(MagnitudeQuantitySpaces):-
  setoff(
    MagnitudeQuantitySpace,
    magnitude_quantity_space(MagnitudeQuantitySpace),
    MagnitudeQuantitySpaces
  ).

%% quantity_space(?QuantitySpace:quantity) is nondet.
% Quantity spaces.
%
% @param QuantitySpace A quantity space. This is either a
%        derivative or a magnitude quantity space.

quantity_space(QuantitySpace):-
  rdfs_individual_of(QuantitySpace, quantity_space:quantity_space).

quantity_space_id(QuantitySpace, QuantitySpaceID):-
  nonvar(QuantitySpace),
  !,
  quantity_space_id1(QuantitySpace, QuantitySpaceID),
  % Quantity spaces have exactly one identifier.
  !.
quantity_space_id(QuantitySpace, QuantitySpaceID):-
  quantity_space_id1(QuantitySpace, QuantitySpaceID).

quantity_space_id1(QuantitySpace, QuantitySpaceID):-
  rdf_datatype(QuantitySpace, quantity_space:has_id, integer, QuantitySpaceID, ccm).

%% quantity_space_label(
%%   ?QuantitySpace:quantity_space,
%%   ?QuantitySpaceLabel:atom
%%) is det.
% Pairs of quantity spaces and their labels.
%
% @param QuantitySpace A quantity space.
% @param QuantitySpaceLabel The atomic natural language label
%        of a quantity space.

quantity_space_label(QuantitySpace, QuantitySpaceLabel):-
  var(QuantitySpace),
  var(QuantitySpaceLabel),
  !,
  quantity_space(QuantitySpace),
  quantity_space_label_(QuantitySpace, QuantitySpaceLabel).
quantity_space_label(QuantitySpace, QuantitySpaceLabel):-
  quantity_space_label_(QuantitySpace, QuantitySpaceLabel),
  !.

quantity_space_label_(QuantitySpace, QuantitySpaceLabel):-
  rdfs_label(QuantitySpace, QuantitySpaceLabel).

%% quantity_space_highest_quantity_value(
%%   ?QuantitySpace:quantity_space,
%%   ?HighestQuantityValue:quantity_value
%% ) is semidet.
% Pairs of quantity spaces and their highest quantity values.
%
% @param QuantitySpace A quantity space.
% @param HighestQuantityValue A quantity value.

quantity_space_highest_quantity_value(QuantitySpace, HighestValue):-
  var(QuantitySpace),
  var(HighestValue),
  !,
  quantity_space_highest_quantity_value_(QuantitySpace, HighestValue).
quantity_space_highest_quantity_value(QuantitySpace, HighestValue):-
  quantity_space_highest_quantity_value_(QuantitySpace, HighestValue),
  % Quantity space - highest quantity value is a one-to-one mapping.
  !.

quantity_space_highest_quantity_value_(QuantitySpace, HighestValue):-
  quantity_space_quantity_value(QuantitySpace, HighestValue),
  highest_quantity_value(HighestValue).

quantity_space_interval_quantity_value(QuantitySpace, IntervalQuantityValue):-
  quantity_space_quantity_value(QuantitySpace, IntervalQuantityValue),
  interval_quantity_value(IntervalQuantityValue).

%% quantity_space_lowest_quantity_value(
%%    ?QuantitySpace:quantity_space,
%%    ?LowestQuantityValue:quantity_value
%% ) is nondet.
% Paits of quantity spaces and theor lowest quantity values.
%
% @param QuantitySpace A quantity space.
% @param LowestQuantityValue A quantity value.

quantity_space_lowest_quantity_value(QuantitySpace, LowestQuantityValue):-
  var(QuantitySpace),
  var(LowestQuantityValue),
  !,
  quantity_space_to_lowest_value_(QuantitySpace, LowestQuantityValue).
quantity_space_lowest_quantity_value(QuantitySpace, LowestQuantityValue):-
  quantity_space_to_lowest_value_(QuantitySpace, LowestQuantityValue),
  % Quantity space - lowest quantity value is a one-to-one mapping.
  !.

quantity_space_negative_quantity_value(QuantitySpace, NegativeLandmark):-
  rdf(
    QuantitySpace,
    quantity_space:has_negative_value,
    NegativeLandmark,
    ccm
  ).

quantity_space_point_quantity_value(QuantitySpace, PointQuantityValue):-
  quantity_space_quantity_value(QuantitySpace, PointQuantityValue),
  point_quantity_value(PointQuantityValue).

quantity_space_positive_quantity_value(QuantitySpace, PositiveQuantityValue):-
  rdf(
    QuantitySpace,
    quantity_space:has_positive_value,
    PositiveQuantityValue,
    ccm
  ).

%% quantity_space_quantity_value(
%%   ?QuantitySpace:quantity_space,
%%   ?QuantityValue:quantity_value
%% ) is nondet.
% Pairs of one quantity space and multiple quantity values.
%
% @param QuantitySpace A quantity space.
% @param QuantityValue A quantity value.

% Quantity value to quantity space.
quantity_space_quantity_value(QuantitySpace, QuantityValue):-
  nonvar(QuantityValue),
  quantity_space_quantity_value1(QuantitySpace, QuantityValue),
  % A quantity value has exactly one quantity space.
  !.
% Quantity space to quantity value.
% Or generation of pairs.
quantity_space_quantity_value(QuantitySpace, QuantityValue):-
  quantity_space_quantity_value1(QuantitySpace, QuantityValue).

quantity_space_quantity_value1(QuantitySpace, QuantityValue):-
  rdfs(QuantitySpace, quantity_space:has_quantity_value, QuantityValue),
  % Exclude relations between quantity space definitions
  % and quantity value definitions.
  quantity_value(QuantityValue).

%% quantity_space_quantity_value_index(
%%    ?QuantitySpace:quantity_space,
%%    ?Index:number,
%%    ?QuantityValue:quantity_value
%% ) is nondet.
% The relation between a quantity space and a quantity value and index pair.
%
% @param QuantitySpace A quantity space.
% @param Index The integer of the quantity value inside the list
%        of the quantity space.
% @param QuantityValue A quantity value.

quantity_space_quantity_value_index(
  QuantitySpace,
  Index,
  QuantityValue
):-
  (
    nonvar(QuantitySpace),
    nonvar(Index)
  ;
    nonvar(QuantitySpace),
    nonvar(QuantityValue)
  ),
  !,
  quantity_space_quantity_value_index1(
    QuantitySpace,
    Index,
    QuantityValue
  ),
  !.
quantity_space_quantity_value_index(
  QuantitySpace,
  Index,
  QuantityValue
):-
  quantity_space_quantity_value_index1(
    QuantitySpace,
    Index,
    QuantityValue
  ).

quantity_space_quantity_value_index1(
  QuantitySpace,
  Index,
  QuantityValue
):-
  quantity_space_quantity_value(QuantitySpace, QuantityValue),
  quantity_value_index(QuantityValue, Index).

quantity_space_quantity_value_name(
  QuantitySpace,
  QuantityValueName,
  QuantityValue
):-
  (
    nonvar(QuantitySpace),
    nonvar(QuantityValueName)
  ;
    nonvar(QuantityValue)
  ),
  !,
  quantity_space_quantity_value_name1(
    QuantitySpace,
    QuantityValueName,
    QuantityValue
  ),
  !.
quantity_space_quantity_value_name(
  QuantitySpace,
  QuantityValueName,
  QuantityValue
):-
  quantity_space_quantity_value_name1(
    QuantitySpace,
    QuantityValueName,
    QuantityValue
  ).

quantity_space_quantity_value_name1(
  QuantitySpace,
  QuantityValueName,
  QuantityValue
):-
  quantity_space_quantity_value(QuantitySpace, QuantityValue),
  quantity_value_label(QuantityValue, QuantityValueName).

quantity_space_to_dot_name(QuantitySpace, QuantitySpaceDOTName):-
  quantity_space_id(QuantitySpace, QuantitySpaceID),
  format(atom(QuantitySpaceDOTName), 'qs_~w', [QuantitySpaceID]).

%% quantity_space_to_cardinality(
%%   +QuantitySpace:quantity_space,
%%   -Cardinality:number
%% ) is det.
% Returns the cardinality of the given quantity space. This is the
% number of quantity values inside the quantity space.
%
% @param QuantitySpace A quantity space (either a derivative or a magnitude
%        quantity space.
% @param Cardinality A positive integer.

quantity_space_to_cardinality(QuantitySpace, Cardinality):-
  quantity_space_to_quantity_values(QuantitySpace, QuantityValues),
  length(QuantityValues, Cardinality).

quantity_space_to_lowest_value_(QuantitySpace, LowestQuantityValue):-
  rdf(
    QuantitySpace,
    quantity_space:has_lowest_quantity_value,
    LowestQuantityValue,
    ccm
  ).

%% quantity_space_to_quantity_values(
%%   +QuantitySpace:quantity_space,
%%   -QuantityValues:list(quantity_value)
%% ) is det.
% Returns the values of the given quantity space.
% The order in which the quantity values occur is significant.
% They are ordered from lowest to highest.
%
% @param QuantitySpace A quantity space.
% @param QuantityValues A list of quantity values.
%        The order in the list is meaningfull.

quantity_space_to_quantity_values(QuantitySpace, QuantityValues):-
  quantity_space_to_quantity_values(QuantitySpace, 0, QuantityValues).

quantity_space_to_quantity_values(
  QuantitySpace,
  Index,
  [QuantityValue | QuantityValues]
):-
  quantity_space_quantity_value_index(
    QuantitySpace,
    Index,
    QuantityValue
  ),
  NewIndex is Index + 1,
  !,
  quantity_space_to_quantity_values(
    QuantitySpace,
    NewIndex,
    QuantityValues
  ).
% If the last value has already been retrieved, then there is no
% quantity value for the new index, adn the method ends here.
quantity_space_to_quantity_values(_QuantitySpace, _QuantityValueIndex, []).

%% quantity_space_to_quantity_values(
%%   +QuantityValue:quantity_value,
%%   +QuantitySpace:quantity_space,
%%   -LowerQuantityValues:list(quantity_value),
%%   -HigherQuantityValues:list(qauntity_value)
%% ) is det.
% Separate the given quantity space into a list of lower and a list of higher
% quantity values relative to the given quantity value.

quantity_space_to_quantity_values(
  QuantityValue,
  QuantitySpace,
  LowerQuantityValues,
  HigherQuantityValues
):-
  quantity_space_quantity_value_index(QuantitySpace, I, QuantityValue),
  findall(
    LowerQuantityValue,
    (
      quantity_space_quantity_value_index(
        QuantitySpace,
        J,
        LowerQuantityValue
      ),
      J < I
    ),
    LowerQuantityValues
  ),
  findall(
    HigherQuantityValue,
    (
      quantity_space_quantity_value_index(
        QuantitySpace,
        J,
        HigherQuantityValue
      ),
      J > I
    ),
    HigherQuantityValues
  ).

quantity_space_zero_quantity_value(QuantitySpace, ZeroLandmark):-
  var(QuantitySpace),
  var(ZeroLandmark),
  !,
  quantity_space_zero_quantity_value1(QuantitySpace, ZeroLandmark).
quantity_space_zero_quantity_value(QuantitySpace, ZeroLandmark):-
  quantity_space_zero_quantity_value1(QuantitySpace, ZeroLandmark),
  % The mapping between quantity spaces and zero quantity values is one
  % to zero-or-one.
  !.

quantity_space_zero_quantity_value1(QuantitySpace, ZeroLandmark):-
  rdf(QuantitySpace, quantity_space:has_zero_value, ZeroLandmark, ccm).

%% quantity_spaces(-QuantitySpaces:ord_set(quantity_space)) is det.
% Returns all quantity spaces.
%
% @param QuantitySpaces An ordered set of URIs of quantity spaces.

quantity_spaces(QuantitySpaces):-
  setoff(
    QuantitySpace,
    quantity_space(QuantitySpace),
    QuantitySpaces
  ).


% QUANTITY SPACE DEFINITIONS %

quantity_space_definition(QuantitySpaceDefinition):-
  rdfs_subclass_of(QuantitySpaceDefinition, quantity_space:quantity_space).

quantity_space_definition_id(
  QuantitySpaceDefinition,
  QuantitySpaceDefinitionID
):-
  nonvar(QuantitySpaceDefinition),
  !,
  quantity_space_definition_id1(
    QuantitySpaceDefinition,
    QuantitySpaceDefinitionID
  ),
  !.
quantity_space_definition_id(
  QuantitySpaceDefinition,
  QuantitySpaceDefinitionID
):-
  quantity_space_definition_id1(
    QuantitySpaceDefinition,
    QuantitySpaceDefinitionID
  ).

quantity_space_definition_id1(
  QuantitySpaceDefinition,
  QuantitySpaceDefinitionID
):-
  rdf_datatype(QuantitySpaceDefinition, quantity_space:has_id, integer, QuantitySpaceDefinitionID, ccm).

quantity_space_definition_label(QuantitySpaceDefinition, Label):-
  var(QuantitySpaceDefinition),
  var(Label),
  !,
  quantity_space_definition(QuantitySpaceDefinition),
  quantity_space_definition_label_(QuantitySpaceDefinition, Label).
quantity_space_definition_label(QuantitySpaceDefinition, Label):-
  quantity_space_definition_label_(QuantitySpaceDefinition, Label),
  !.

quantity_space_definition_label_(QuantitySpaceDefinition, Label):-
  rdfs_label(QuantitySpaceDefinition, Label).

quantity_space_definition_quantity_space(
  QuantitySpaceDefinition,
  QuantitySpace
):-
  nonvar(QuantitySpace),
  !,
  quantity_space_definition_quantity_space1(
    QuantitySpaceDefinition,
    QuantitySpace
  ),
  % A quantity space has exactly one quantity space definition.
  !.
quantity_space_definition_quantity_space(
  QuantitySpaceDefinition,
  QuantitySpace
):-
  quantity_space_definition_quantity_space1(
    QuantitySpaceDefinition,
    QuantitySpace
  ).

quantity_space_definition_quantity_space1(
  QuantitySpaceDefinition,
  QuantitySpace
):-
  rdfs_individual_of(QuantitySpace, QuantitySpaceDefinition).

quantity_space_definition_quantity_value_definition(
  QuantitySpaceDefinition,
  QuantityValueDefinition
):-
  quantity_space_definition(QuantitySpaceDefinition),
  rdfs(
    QuantityValueDefinition,
    value:has_quantity_space,
    QuantitySpaceDefinition
  ).

quantity_space_definition_to_dot_name(
  QuantitySpaceDefinition,
  QuantitySpaceDefinitionDOTName
):-
  quantity_space_definition_id(
    QuantitySpaceDefinition,
    QuantitySpaceDefinitionID
  ),
  format(
    atom(QuantitySpaceDefinitionDOTName),
    'qsd_~w',
    [QuantitySpaceDefinitionID]
  ).

quantity_space_definitions(QuantitySpaceDefinitions):-
  setoff(
    QuantitySpaceDefinition,
    quantity_space_definition(QuantitySpaceDefinition),
    QuantitySpaceDefinitions
  ).


% QUANTITY VALUES %

%% highest_quantity_value(?HighestQuantityValue:quantity_value) is nondet.
% A highest quantity value.
%
% @param HighestQuantityValue A quantity value.

highest_quantity_value(HighestQuantityValue):-
  quantity_value(HighestQuantityValue),
  \+(
    rdf(
      HighestQuantityValue,
      value:has_higher_quantity_value,
      _QuantityValue,
      ccm
    )
  ).

interval_quantity_value(IntervalQuantityValue):-
  rdfs_individual_of(IntervalQuantityValue, value:interval_quantity_value).

%% interval_quantity_value_between_point_quantity_values(
%%   ?LowerPointQuantityValue,
%%   ?IntervalQuantityValue:quantity_value,
%%   ?HigherPointQuantityValue:quantity_value
%% ) is semidet.

interval_quantity_value_between_point_quantity_values(
  LowerPointQuantityValue,
  IntervalQuantityValue,
  HigherPointQuantityValue
):-
  interval_quantity_value(IntervalQuantityValue),
  quantity_value_smaller_than_quantity_value(
    LowerPointQuantityValue,
    IntervalQuantityValue
  ),
  point_quantity_value(LowerPointQuantityValue),
  quantity_value_smaller_than_quantity_value(
    IntervalQuantityValue,
    HigherPointQuantityValue
  ),
  point_quantity_value(HigherPointQuantityValue).

lowest_quantity_value(LowestQuantityValue):-
  quantity_value(LowestQuantityValue),
  \+(
    rdf(
      LowestQuantityValue,
      value:has_lower_quanitity_value,
      _QuantityValue,
      ccm
    )
  ).

negative_quantity_value(NegativeLandmark):-
  rdfs_individual_of(NegativeLandmark, value:negative_quantity_value).

negative_quantity_value(Quantity, NegativeLandmark):-
  quantity_quantity_value(Quantity, NegativeLandmark),
  negative_quantity_value(NegativeLandmark).

point_quantity_value(PointQuantityValue):-
  rdfs_individual_of(PointQuantityValue, value:point_quantity_value).

positive_quantity_value(PositiveQuantityValue):-
  rdfs_individual_of(PositiveQuantityValue, value:positive_quantity_value).

positive_quantity_value(Quantity, PositiveQuantityValue):-
  quantity_quantity_value(Quantity, PositiveQuantityValue),
  positive_quantity_value(PositiveQuantityValue).

quantity_value(QuantityValue):-
  rdfs_individual_of(QuantityValue, value:quantity_value).

quantity_value(
  Space,
  Quantity,
  MagnitudeQuantityValue,
  MagnitudeQuantityValueExpression,
  DerivativeQuantityValue,
  DerivativeQuantityValueExpression
):-
  magnitude_quantity_value(
    Space,
    Quantity,
    MagnitudeQuantityValue,
    MagnitudeQuantityValueExpression
  ),
  derivative_quantity_value(
    Space,
    Quantity,
    DerivativeQuantityValue,
    DerivativeQuantityValueExpression
  ).

quantity_value_expression(Expression):-
  derivative_quantity_value_expression(Expression).
quantity_value_expression(Expression):-
  magnitude_quantity_value_expression(Expression).

quantity_value_greater_than_quantity_value(
  HigherQuantityValue,
  LowerQuantityValue
):-
  quantity_value_greater_than_quantity_value(
    HigherQuantityValue,
    _Distance,
    LowerQuantityValue
  ).

quantity_value_greater_than_quantity_value(
  HigherQuantityValue,
  Distance,
  LowerQuantityValue
):-
  nonvar(HigherQuantityValue),
  nonvar(LowerQuantityValue),
  !,
  quantity_value_greater_than_quantity_value_(
    HigherQuantityValue,
    Distance,
    LowerQuantityValue
  ),
  !.
quantity_value_greater_than_quantity_value(
  HigherQuantityValue,
  Distance,
  LowerQuantityValue
):-
  quantity_value_greater_than_quantity_value_(
    HigherQuantityValue,
    Distance,
    LowerQuantityValue
  ).

quantity_value_greater_than_quantity_value_(
  HigherQuantityValue,
  1,
  LowerQuantityValue
):-
  rdf(
    LowerQuantityValue,
    value:has_higher_quantity_value,
    HigherQuantityValue,
    ccm
  ).
quantity_value_greater_than_quantity_value_(
  HigherQuantityValue,
  NewDistance,
  LowerQuantityValue
):-
  var(LowerQuantityValue),
  !,
  rdf(
    InBetweenQuantityValue,
    value:has_higher_quantity_value,
    HigherQuantityValue,
    ccm
  ),
  quantity_value_greater_than_quantity_value_(
    InBetweenQuantityValue,
    Distance,
    LowerQuantityValue
  ),
  succ(Distance, NewDistance).
quantity_value_greater_than_quantity_value_(
  HigherQuantityValue,
  NewDistance,
  LowerQuantityValue
):-
  var(HigherQuantityValue),
  !,
  rdf(
    LowerQuantityValue,
    value:has_higher_quantity_value,
    InBetweenQuantityValue,
    ccm
  ),
  quantity_value_greater_than_quantity_value_(
    HigherQuantityValue,
    Distance,
    InBetweenQuantityValue
  ),
  succ(Distance, NewDistance).

%% quantity_value_quantity_value(
%%   ?DirectlyLowerQuantityValue:quantity_value,
%%   ?DirectlyHigherQuantityValue:quantity_value
%% ) is det.
% Pairs of consecutive quantity values.
%
% @param DirectlyLowerQuantityValue A quantity value relative to whom the next
%        quantity value is returned.
% @param DirectlyHigherQuantityValue The next quantity value.

quantity_value_quantity_value(LowerQuantityValue, HigherQuantityValue):-
  quantity_value_quantity_value_strict(LowerQuantityValue, HigherQuantityValue).
% There is a higher value.
% An alternative to going to a higher quantity value, is staying in
% the current interval (i.e. increasing within an interval).
quantity_value_quantity_value(QuantityValue, QuantityValue):-
  interval_quantity_value(QuantityValue).

quantity_value_quantity_value_strict(DirectlyLowerQuantityValue, DirectlyHigherQuantityValue):-
  quantity_value(DirectlyLowerQuantityValue),
  rdf(
    DirectlyLowerQuantityValue,
    value:has_higher_quantity_value,
    DirectlyHigherQuantityValue,
    ccm
  ).

%% quantity_value_id(
%%   ?QuantityValue:quantity_value,
%%   ?QuantityValueID:number
%% ) is nondet.
% Pairs of quantity values and their identifiers.
%
% @param QuantityValue A quantity value. Either a derivative or a
%        magnitude quantity value.
% @param QuantityValueID A numeric identifier of a quantity value.

quantity_value_id(QuantityValue, QuantityValueID):-
  nonvar(QuantityValue),
  !,
  quantity_value_id1(QuantityValue, QuantityValueID),
  !.
quantity_value_id(QuantityValue, QuantityValueID):-
  quantity_value_id1(QuantityValue, QuantityValueID).

quantity_value_id1(QuantityValue, QuantityValueID):-
  quantity_value(QuantityValue),
  rdf_datatype(QuantityValue, value:has_id, integer, QuantityValueID, ccm).

%% quantity_value_index(
%%   ?QuantityValue:quantity_value,
%%   ?Index:number
%% ) is nondet.
% Pairs of quantity values and their numeric index.
%
% @param QuantityValue A quantity value.
% @param Index A numeric quantity value index.

quantity_value_index(QuantityValue, Index):-
  var(QuantityValue),
  !,
  % The generative case takes every quantity value in turn, and then
  % retrieves the indexes deterministically.
  quantity_value(QuantityValue),
  quantity_value_index1(QuantityValue, Index).
quantity_value_index(QuantityValue, Index):-
  quantity_value_index1(QuantityValue, Index),
  % A quantity value has exactly one index, so this case is deterministic.
  !.

quantity_value_index1(QuantityValue, Index):-
  rdf_datatype(QuantityValue, value:has_index, integer, Index, ccm).

%% quantity_value_label(
%%   ?QuantityValue:quantity_value,
%%   ?QuantityValueLabel:atom
%% ) is nondet.
% Pairs of quantity values and their labels.
%
% @param QuantityValue A quantity value.
% @param QuantityValueLabel The atomic natural language label of a quantity value.

quantity_value_label(QuantityValue, Label):-
  nonvar(QuantityValue),
  !,
  quantity_value_label1(QuantityValue, Label),
  % A quantity value has exactly one label.
  !.
quantity_value_label(QuantityValue, Label):-
  quantity_value(QuantityValue),
  quantity_value_label1(QuantityValue, Label).

quantity_value_label1(QuantityValue, Label):-
  rdfs_label(QuantityValue, Label).

quantity_value_smaller_than_quantity_value(
  LowerQuantityValue,
  HigherQuantityValue
):-
  quantity_value_smaller_than_quantity_value(
    LowerQuantityValue,
    _Distance,
    HigherQuantityValue
  ).

quantity_value_smaller_than_quantity_value(
  LowerQuantityValue,
  Distance,
  HigherQuantityValue
):-
  quantity_value_greater_than_quantity_value(
    HigherQuantityValue,
    Distance,
    LowerQuantityValue
  ).

quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName):-
  quantity_value_id(QuantityValue, QuantityValueID),
  format(atom(QuantityValueDOTName), 'qv_~w', [QuantityValueID]).

quantity_values(QuantityValues):-
  setoff(
    QuantityValue,
    quantity_value(QuantityValue),
    QuantityValues
  ).

zero_quantity_value(ZeroLandmark):-
  rdfs_individual_of(ZeroLandmark, value:zero_quantity_value).

zero_quantity_value(Quantity, ZeroLandmark):-
  quantity_quantity_value(Quantity, ZeroLandmark),
  zero_quantity_value(ZeroLandmark).



% QUANTITY VALUE DEFINITIONS %

interval_quantity_value_definition(IntervalQuantityValueDefinition):-
  rdfs_subclass_of(
    IntervalQuantityValueDefinition,
    value:interval_quantity_value
  ).

negative_quantity_value_definition(NegativeQuantityValueDefinition):-
  rdfs_subclass_of(
    NegativeQuantityValueDefinition,
    value:negative_quantity_value
  ).

point_quantity_value_definition(PointQuantityValueDefinition):-
  rdfs_subclass_of(PointQuantityValueDefinition, value:point_quantity_value).

positive_quantity_value_definition(PositiveQuantityValueDefinition):-
  rdfs_subclass_of(
    PositiveQuantityValueDefinition,
    value:positive_quantity_value
  ).

quantity_value_definition(QuantityValueDefinition):-
  rdfs_subclass_of(QuantityValueDefinition, value:quantity_value).

quantity_value_definition_id(
  QuantityValueDefinition,
  QuantityValueDefinitionID
):-
  nonvar(QuantityValueDefinition),
  !,
  quantity_value_definition_id1(
    QuantityValueDefinition,
    QuantityValueDefinitionID
  ),
  !.
quantity_value_definition_id(
  QuantityValueDefinition,
  QuantityValueDefinitionID
):-
  quantity_value_definition_id1(
    QuantityValueDefinition,
    QuantityValueDefinitionID
  ).

quantity_value_definition_id1(
  QuantityValueDefinition,
  QuantityValueDefinitionID
):-
  rdf_datatype(QuantityValueDefinition, value:has_id, integer, QuantityValueDefinitionID, ccm).

quantity_value_definition_label(
  QuantityValueDefinition,
  QuantityValueDefinitionLabel
):-
  quantity_value_definition(QuantityValueDefinition),
  rdfs_label(QuantityValueDefinition, QuantityValueDefinitionLabel).

quantity_value_definition_quantity_value(
  QuantityValueDefinition,
  QuantityValue
):-
  quantity_value_definition(QuantityValueDefinition),
  rdf(QuantityValue, rdf:type, QuantityValueDefinition, ccm).

quantity_value_definition_to_dot_name(
  QuantityValueDefinition,
  QuantityValueDefinitionDOTName
):-
  quantity_value_definition_id(
    QuantityValueDefinition,
    QuantityValueDefinitionID
  ),
  format(
    atom(QuantityValueDefinitionDOTName),
    'qvd_~w',
    [QuantityValueDefinitionID]
  ).

quantity_value_definitions(QuantityValueDefinitions):-
  setoff(
    QuantityValueDefinition,
    quantity_value_definition(QuantityValueDefinition),
    QuantityValueDefinitions
  ).

zero_quantity_value_definition(ZeroQuantityValueDefinition):-
  rdfs_subclass_of(ZeroQuantityValueDefinition, value:zero_quantity_value).



% RELATIONS %

relation_category(Relation, RelationCategory):-
  rdf_global_id(expression:calculus, RelationCategory),
  rdfs_subproperty_of(Relation, RelationCategory).
relation_category(Relation, RelationCategory):-
  rdf_global_id(expression:causality, RelationCategory),
  rdfs_subproperty_of(Relation, RelationCategory).
relation_category(Relation, RelationCategory):-
  rdf_global_id(expression:conditional, RelationCategory),
  rdfs_subproperty_of(Relation, RelationCategory).
relation_category(Relation, RelationCategory):-
  rdf_global_id(expression:correspondence, RelationCategory),
  rdfs_subproperty_of(Relation, RelationCategory).
relation_category(Relation, RelationCategory):-
  rdf_global_id(expression:inequality, RelationCategory),
  rdfs_subproperty_of(Relation, RelationCategory).



% SIGNS %

equal_to(Sign):-
  rdfs_subclass_of(Sign, expression:equal_to).

equal_to_expression(Expression):-
  equal_to(Sign),
  expression_sign(Expression, Sign).

expression_sign(Expression, Sign):-
  inequality_expression(Expression),
  !,
  inequality_expression_sign(Expression, Sign).
expression_sign(Expression, Sign):-
  quantity_causality_expression(Expression),
  quantity_causality_expression_sign(Expression, Sign).

greater_than(Sign):-
  rdfs_subclass_of(Sign, expression:greater_than).

greater_than_expression(Expression):-
  greater_than(Sign),
  expression_sign(Expression, Sign).

multiply_signs([Sign], Sign):-
  !.
multiply_signs([Sign1, Sign2 | Signs], MultipliedSign):-
  multiply_signs(Sign1, Sign2, Sign3),
  multiply_signs([Sign3 | Signs], MultipliedSign).

multiply_signs(expression:equal_to, Sign, Sign):-
  !.
multiply_signs(Sign, expression:equal_to, Sign):-
  !.
multiply_signs(
  expression:smaller_than,
  expression:smaller_than,
  expression:greater_than
):-
  !.
multiply_signs(Sign, Sign, Sign):-
  !.
multiply_signs(
  expression:greater_than,
  expression:smaller_than,
  expression:smaller_than
):-
  !.
multiply_signs(
  expression:smaller_than,
  expression:greater_than,
  expression:smaller_than
).

sign(Sign):-
  equal_to(Sign).
sign(Sign):-
  greater_than(Sign).
sign(Sign):-
  smaller_than(Sign).

smaller_than(Sign):-
  rdfs_subclass_of(Sign, expression:smaller_than).

smaller_than_expression(Expression):-
  smaller_than(Sign),
  expression_sign(Expression, Sign).



% SPACES %

%% space_to_quantities(+Space:space, -Quantities:ord_set(quantity)) is det.
% Returns the quantities for the given space.
%
% @param Space A space.
% @param Quantities An ordered set of quantities.

space_to_quantities(Space, Quantities):-
  setoff(
    Quantity,
    quantity_space(Quantity, Space),
    Quantities
  ).



% STATES %

%% state_to_quantities(+State:state, -Quantities:ord_set(quantity)) is det.
% Returns the quantities for the given state.
%
% @param State A state.
% @param Quantities An ordered set of quantities.

state_to_quantities(State, Quantities):-
  space_to_quantities(State, Quantities),
  state(State).
