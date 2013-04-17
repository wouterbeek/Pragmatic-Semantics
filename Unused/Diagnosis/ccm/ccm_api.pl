:- module(
  ccm_api,
  [
% COMPONENTS
    competitive_component/1, % ?CompetitiveComponent:component
    component/1, % ?Component:component
    component/5, % ?Type:component_definition
                 % +Inputs:list(resource/point),
                 % +Supports:list(resource/point),
                 % +Outputs:list(resource/point),
                 % ?Component:component
    component_abbreviation/2, % ?Component:component
                              % ?ComponentAbbreviation:atom
    component_cardinality/2, % ?Component:component
                             % ?Cardinality:integer
    component_definition_component/2, % ?Type:component_definition
                                      % ?Component:component
    component_expression/2, % ?Component:component
                            % ?Expression:expression
    component_expression/3, % ?Component:component
                            % ?Relation:uri
                            % ?Expression:expression
    component_expression/4, % +Component:component
                            % +Relation:relation
                            % +Environment:environment
                            % -Expression:expression
    component_id/2, % ?Component:component
                    % ?ComponentID:number
    component_input_point/2, % ?Component:component
                             % ?InputPoint:point
    component_input_point/3, % ?Component:component
                             % ?Relation:uri
                             % ?InputPoint:point
    component_input_point_cloud/2, % ?Component:component
                                   % ?InputPointCloud:point_cloud
    component_input_point_cloud/3, % ?Component:component
                                   % ?Relation:relation
                                   % ?InputPointCloud:point_cloud
    component_label/2, % +Component:component
                       % -ComponentLabel:atom
    component_output_point/2, % ?Component:component
                              % ?OutputPoint:point
    component_output_point/3, % ?Component:component
                              % ?Relation:uri
                              % ?OutputPoint:point
    component_output_point_cloud/2, % ?Component:component
                                    % ?OutputPointCloud:point_loud
    component_output_point_cloud/3, % ?Component:component
                                    % ?Relation:uri
                                    % ?OutputPointCloud:point_loud
    component_point/2, % ?Component:component
                       % ?Point:point
    component_point/3, % ?Component:component
                       % ?Relation:uri
                       % ?Point:point
    component_point/4, % +Component:component
                       % +Relation:relation
                       % +Environment:environment
                       % -Point:point
    component_point_cloud/2, % ?Component:component
                             % ?PointCloud:point_cloud
    component_point_cloud/3, % ?Component:component
                             % ?Relation:uri
                             % ?PointCloud:point_cloud
    component_space/3, % ?Component:component
                       % ?Relation:uri
                       % ?Space:space
    component_space_delta/2, % ?Component:component
                             % ?SpaceDelta:integer
    component_space_delta/3, % ?FromSpace:space
                             % ?Component:component
                             % ?ToSpace:space
    component_support_component/2, % ?Component:component
                                   % ?SupportComponent:component
    component_support_expression/2, % ?Component:component
                                    % ?SupportExpression:expression
    component_support_point/2, % ?Component:component
                               % ?SupportPoint:point
    component_support_point/3, % ?Component:component
                               % ?Relation:uri
                               % ?SupportPoint:point
    component_support_point_cloud/2, % ?Component:component
                                     % ?SupportPointCloud:point_cloud
    component_support_point_cloud/3, % ?Component:component
                                     % ?Relation:uri
                                     % ?SupportPointCloud:point_cloud
    component_to_ccm_label/2, % +Component:component
                           % -ComponentCCMLabel:atom
    component_to_dot_name/2, % +Component:component
                             % -ComponentDOTName:atom
    component_to_input_points/2, % +Component:component
                                 % -InputPoints:ord_set(point)
    component_to_input_point_clouds/2, % +Component:component
                                       % -InputPointClouds:list(point_cloud)
    component_to_output_points/2, % +Component:component
                                  % -OutputPoints:ord_set(point)
    component_to_output_point_clouds/2, % +Component:component
                                        % -OutputPointClouds:list(point_loud)
    component_to_point_clouds/2, % +Component:component
                                 % -PointClouds:list(point_cloud)
    component_to_point_specifications/2, % +Component:component
                                         % -PointSpecifications:list(resource)
    component_to_points/2, % +Component:component
                           % -Points:ord_set(point)
    component_to_support_points/2, % +Component:component
                                   % -SupportPoints:ord_set(point_cloud)
    component_to_support_point_clouds/2, % +Component:component
                                         % -SupportPointClouds:list(point_cloud)
    component_transition/2, % ?FromComponent:component
                            % ?ToComponent:component
    components/1, % ?Components:ord_set(component)
    components_to_points/2, % +Components:ord_set(component)
                            % -Points:ord_set(point)
    disabled_component/1, % ?Component:component
    retrieval_component/1, % ?Component:component
    subsumed_component/1, % ?SubsumedComponent:component
    termination_component/1, % ?Component:component
    transition_component/1, % ?Component:component
    within_state_component/1, % ?Component:component

% COMPONENT CLOUDS
    active_component_cloud/1, % ?ComponentCloud:component_cloud
    aggregate_component_cloud/1, % ?Aggregate:component_cloud
    bidirectional_component_cloud/1, % ?Bidirectional:component_cloud
    competitive_component_cloud/1,
    component_cloud/1, % ?ComponentCloud:component_cloud
    component_cloud/5, % ?Type:component_definition
                       % +Inputs:list(resource/point_cloud),
                       % +Supports:list(resource/point_cloud),
                       % +Outputs:list(resource/point_cloud),
                       % ?ComponentCloud:component_cloud
    component_cloud_abbreviation/2, % ?ComponentCloud:component_cloud
                                    % ?ComponentCloudAbbreviation:atom
    component_cloud_cardinality/2, % ?Component:component
                                   % ?Cardinality:integer
    component_cloud_chain/3, % +ComponentCloud:component_cloud
                             % -ComponentCloudChain:list(component_cloud)
                             % -ComponentChains:list(list(component))
    component_cloud_chain/4, % +ComponentCloud:component_cloud
                             % +ComponentDefinition:component_definition
                             % -ComponentCloudChain:list(component_cloud)
                             % -ComponentChains:list(list(component))
    component_cloud_chain_to_component_chains/2, % +ComponentCloudChain:list(component_cloud)
                                                 % -ComponentChains:list(list(component))
    component_cloud_component/2, % ?ComponentCloud:component_cloud
                                 % ?Component:component
    component_cloud_component_relation/2, % ?ComponentCloudRelation:uri
                                          % ?ComponentRelation:uri
    component_cloud_id/2, % ?ComponentCloud:component_cloud
                          % ?ID:integer
    component_cloud_input_point/2, % ?ComponentCloud:component_cloud
                                   % ?InputPoint:point
    component_cloud_input_point/3, % ?ComponentCloud:component_cloud
                                   % ?InputRelation:uri
                                   % ?InputPoint:point
    component_cloud_input_point_cloud/2, % ?ComponentCloud:component_cloud
                                         % ?InputPointCloud:point_cloud
    component_cloud_input_point_cloud/3, % ?ComponentCloud:component_cloud
                                         % ?InputRelation:uri
                                         % ?InputPointCloud:point_cloud
    component_cloud_label/2, % ?ComponentCloud:component_cloud
                             % ?Label:atom
    component_cloud_output_point/2, % ?ComponentCloud:component_cloud
                                    % ?OutputPoint:point
    component_cloud_output_point/3, % ?ComponentCloud:component_cloud
                                    % ?OutputRelation:uri
                                    % ?OutputPoint:point
    component_cloud_output_point_cloud/2, % ?ComponentCloud:component_cloud
                                          % ?OutputPointCloud:point_cloud
    component_cloud_output_point_cloud/3, % ?ComponentCloud:component_cloud
                                          % ?OutputRelation:uri
                                          % ?OutputPointCloud:point_cloud
    component_cloud_point/2, % ?ComponentCloud:component_cloud
                             % ?Point:point
    component_cloud_point/3, % ?ComponentCloud:component_cloud
                             % ?Relation:relation
                             % ?Point:point
    component_cloud_point_cloud/2, % ?ComponentCloud:component_cloud
                                   % ?PointCloud:point_cloud
    component_cloud_point_cloud/3, % ?ComponentCloud:component_cloud
                                   % ?Relation:uri
                                   % ?PointCloud:point_cloud
    component_cloud_space/2, % ?ComponentCloud:component_cloud
                             % ?Space:space
    component_cloud_space/3, % ?ComponentCloud:component_cloud
                             % ?Relation:uri
                             % ?Space:space
    component_cloud_space_delta/3, % ?FromSpace:space
                                   % ?ComponentCloud:component_cloud
                                   % ?ToSpace:space
    component_cloud_subsumes_component_cloud/2, % ?Subsuming:component_cloud
                                                % ?Subsumed:component_cloud
    component_cloud_support_component_cloud/2, % ?ComponentCloud:component_cloud
                                               % ?Support:component_cloud
    component_cloud_support_expression/2, % ?ComponentCloud:component_cloud
                                          % ?Support:expression
    component_cloud_support_point/2, % ?ComponentCloud:component_cloud
                                     % ?SupportPoint:point
    component_cloud_support_point/3, % ?ComponentCloud:component_cloud
                                     % ?SupportRelation:uri
                                     % ?SupportPoint:point
    component_cloud_support_point_cloud/2, % ?ComponentCloud:component_cloud
                                           % ?SupportPointCloud:point_cloud
    component_cloud_support_point_cloud/3, % ?ComponentCloud:component_cloud
                                           % ?SupportRelation:uri
                                           % ?SupportPointCloud:point_cloud
    component_cloud_to_behavior_rule/2, % +ComponentCloud:component_cloud
                                        % -BehaviorRule:atom
    component_cloud_to_ccm_label/2, % +ComponentCloud:component_cloud
                                    % -CCMLabel:atom
    component_cloud_to_components/2, % +ComponentCloud:component_cloud
                                     % -Components:ord_set(component)
    component_cloud_to_dot_name/2, % +ComponentCloud:component_cloud
                                   % -ComponentCloudDOTName:atom
    component_cloud_to_input_point_clouds/2, % +ComponentCloud:component_cloud
                                             % -InputPointClouds:list(point_cloud)
    component_cloud_to_input_spaces/2, % +ComponentCloud:component_cloud
                                       % -InputSpaces:ord_set(space)
    component_cloud_to_output_point_clouds/2, % +ComponentCloud:component_cloud
                                              % -OutputPointClouds:list(point_cloud)
    component_cloud_to_output_spaces/2, % +ComponentCloud:component_cloud
                                        % -OutputSpaces:ord_set(space)
    component_cloud_to_point_clouds/2, % +ComponentCloud:component_cloud
                                       % -PointClouds:list(point_cloud)
    component_cloud_to_point_specifications/2, % +ComponentCloud:component_cloud
                                               % -PointSpecifications:list(resource)
    component_cloud_to_spaces/2, % +ComponentCloud:component_cloud
                                 % -Spaces:ord_set(space)
    component_cloud_to_spaces/3, % +ComponentCloud:component_cloud
                                 % +Relation:uri
                                 % -Spaces:ord_set(space)
    component_cloud_to_subsumed_component_clouds/2, % ?ComponentCloud:component_cloud
                                                    % ?SubsumedComponentClouds:ord_set(component_cloud)
    component_cloud_to_support_component_clouds/2, % +ComponentCloud:component_cloud
                                                   % -SupportComponentClouds:ord_set(component_cloud)
    component_cloud_to_support_point_clouds/2, % +ComponentCloud:component_cloud
                                               % -SupportPointClouds:list(point_cloud)
    component_cloud_to_support_spaces/2, % +ComponentCloud:component_cloud
                                         % -SupportSpaces:ord_set(space)
    component_cloud_transition/2, % ?FromComponentCloud:component_cloud
                                  % ?ToComponentCloud:component_cloud
    component_clouds/1, % -ComponentClouds:ord_set(component_cloud)
    component_clouds_to_point_clouds/2, % +ComponentClouds:ord_set(component_cloud)
                                        % -PointClouds:ord_set(point_cloud)
    component_definition_component_cloud/2, % ?ComponentDefinition:component_definition
                                            % ?ComponentCloud:component_cloud
    continuity_component_cloud/1, % ?Continuity:component_cloud
    disabled_component_cloud/1, % ?Disabled:component_cloud
    find_component_cloud/5, % +Type:component_definition
                            % +Inputs:list(atom/point)
                            % +Supports:list(atom/point)
                            % +Outputs:list(atom/point)
                            % -ComponentCloud:component_cloud
    first_component_cloud/2, % ?ComponentDefinition:component_definition
                             % ?FirstComponentCloud:component_cloud
    hierarchical_component_cloud/1, % ?Hierarchical:component_cloud
    retrieval_component_cloud/1, % ?Retrieval:component_cloud
    subsumed_component_cloud/1, % ?Subsumed:component_cloud
    transition_component_cloud/1, % ?Transition:component_cloud
    termination_component_cloud/1, % ?Transition:component_cloud
    unpackable_component_cloud/1, % ?Unpackable:component_cloud
    within_state_component_cloud/1, % ?WithinState:component_cloud

% COMPONENT DEFINITIONS
    aggregate_component_definition/1, % ?ComponentDefinition:component_definition
    bidirectional_component_definition/1, % ?ComponentDefinition:component_definition
    can_instantiate_component_definition/1, % ?ComponentDefinition:component_definition
    component_definition/1, % ?ComponentDefinition:component_definition
    component_definition_abbreviation/2, % ?ComponentDefinition:component_definition
                                         % ?ComponentDefinitionAbbreviation:atom
    component_definition_cardinality/2, % ?ComponentDefinition:component_definition
                                        % ?Cardinality:number
    component_definition_id/2, % ?ComponentDefinition:component_definition
                               % ?ComponentDefinitionID:number
    component_definition_label/2, % +ComponentDefinition:component_definition
                                  % -ComponentDefinitionLabel:atom
    component_definition_point_specification/2, % ?ComponentDefinition:component_definition,
                                                % ?PointSpecification:uri
    component_definition_point_specification/3, % ?ComponentDefinition:component_definition,
                                                % ?Index:number
                                                % ?PointSpecification:uri
    component_definition_point_specification/4, % ?ComponentDefinition:component_definition,
                                                % ?Index:number
                                                % ?RelationName:atom
                                                % ?PointSpecification:uri
    component_definition_space_delta/2, % ?ComponentDefinition:component_definition
                                        % ?SpaceDelta:integer
    component_definition_space_delta/3, % ?FromSpace:space
                                        % ?ComponentDefinition:component_definition
                                        % ?ToSpace:space
    component_definition_to_behavior_rule/2, % +ComponentDefinition:component_definition
                                             % -BehaviorRule:atom
    component_definition_to_dot_name/2, % +ComponentDefinition:component_definition
                                        % -ComponentDefinitionDOTName:atom
    component_definition_to_point_relation/2, % +ComponentDefinition:component_definition
                                              % -PointRelation:uri
    component_definition_to_point_relations/2, % +ComponentDefinition:component_definition
                                               % -PointRelations:list(resource)
    component_definitions/1, % -ComponentDefinitions:ord_set(component_definition)
    continuity_component_definition/1, % ?ComponentDefinition:component_definition
    disabled_component_definition/1, % ?DisabledComponentDefinition:component_definition
    instantiable_component_definition/1, % ?ComponentDefinition:component_definition
    point_specification_expression_definition/2, % ?PointSpecification:uri
                                                 % ?ExpressionDefinition:expression_definition
    point_specification_index/2,
    point_specification_point/3, % +Component:component
                                 % +PointSpecification:uri
                                 % -Point:point
    point_specification_point/4, % +Component:component
                                 % +PointSpecification:uri
                                 % +Environment:environment
                                 % -Point:point
    point_specification_relation/2,
    point_specification_relation_name/2,
    retrieval_component_definition/1, % ?ComponentDefinition:component_definition
    space_transition_component_definition/1, % ?ComponentDefinition:component_definition
    state_transition_component_definition/1, % ?ComponentDefinition:component_definition
    termination_component_definition/1, % ?ComponentDefinition:component_definition
    within_state_component_definition/1, % ?ComponentDefinition:component_definition

% EXPRESSIONS
    aggregate_expression/1, % ?Expression:expression
    aggregate_expression/2, % +Expression:expression
                            % -Aggregation:boolean
    bidirectional_expression/1, % ?Expression:expression
    closure/1, % +Space:space
    conditional_expression/1, % ?Expression:expression
    expression/1, % ?Expression:expression
    expression/3, % ?Property:uri
                  % ?Argument:uri
                  % ?Expression:expression
    expression/4, % ?ExpressionDefinition:expression_definition
                  % ?FromArgument:uri
                  % ?ToArgument:uri
                  % ?Expression:expression
    expression_argument/2, % ?Expression:expression
                           % ?Argument:uri
    expression_id/2, % ?Expression:expression
                     % ?ExpressionID:number
    expression_from_argument/2, % ?Expression:expression
                                % ?FromArgument:uri
    expression_point_cloud/2, % ?Expression:expression
                              % ?PointCloud:point_cloud
    expression_space/2, % ?Expression:expression
                        % ?Space:space
    expression_subsumes_expression/2, % ?SubsumingExpression:expression
                                      % ?SubsumedExpression:expression
    expression_to_argument/2, % ?Expression:expression
                              % ?ToArgument:uri
    expression_to_dot_name/2, % +Expression:expression
                              % -ExpressionDOTName:atom
    expression_to_subsumed_expressions/2, % ?Expression:expression
                                          % ?SubsumedExpressions:list(expression)
    expressions/1, % -Expressions:ord_set(expression)
    find_alternative_expression/3, % +Expression:expression
                                   % -SubsententialPart:uri
                                   % -AlternativeExpression:expression
    find_expression_definition/2, % +ExpressionDefinitions:list(expression_definition)
                                  % -ExpressionDefinition:expression_definition
    find_or_add_alternative_expression/3, % +Expression:expression
                                          % +AlternativeSubsententialExpression:uri
                                          % -AlternativeExpression:expression
    inverted_expression/1, % ?InvertedExpression:expression
    subsumed_expression/1, % ?SubsumedExpression:expression
    support_expression/1, % ?SupportExpression:expression
    unidirectional_expression/1, % ?UnidirectionalExpression:expression
    uninverted_expression/1, % ?UninvertedExpression:expression
    within_state_expression/1, % ?Expression:expression

% EXPRESSION DEFINITIONS
    bidirectional_expression_definition/1, % ?BidirectionalExpressionDefinition:expression_definition
    can_instantiate_expression_definition/1, % ?UnidirectionalExpressionDefinition:expression_definition
    expression_definition/1, % ?ExpressionDefinition:expression_definition
    expression_definition_abbreviation/2, % ?ExpressionDefinition:expression_definition
                                          % ?Abbreviation:atom
    expression_definition_expression/2, % ?ExpressionDefinition:expression_definition
                                        % ?Expression:expression
    expression_definition_id/2, % +ExpressionDefinition:expression_definition
                                % -ExpressionDefinitionID:number
    expression_definition_label/2, % ?ExpressionDefinition:expression_definition
                                   % ?ExpressionDefinitionLabel:atom
    expression_definition_name/2, % ?ExpressionDefinition:expression_definition
                                  % ?ExpressionDefinitionName:atom
    expression_definition_to_dot_name/2, % +ExpressionDefinition:expression_definition
                                         % -ExpressionDefinitionDOTName:atom
    expression_definition_to_expressions/2, % +ExpressionDefinition:expression_definition
                                            % -Expressions:ord_set(expression)
    expression_definitions/1, % -ExpressionDefinitions:ord_set(expression_definition)
    instantiable_expression_definitions/1, % -InstantiableExpressionDefinitions:ord_set(expression_definition)
    inverted_expression_definition/1, % ?InvertedExpressionDefinition:expression_definition
    support_expression_definition/1, % ?SupportExpressionDefinition:expression_definition
    unidirectional_expression_definition/1, % ?UnidirectionalExpressionDefinition:expression_definition
    uninverted_expression_definition/1, % ?UninvertedExpressionDefinition:expression_definition

% POINTS
    isolated_point/1, % ?IsolatedPoint:point
    isolated_points/1, % -IsolatedPoints:ord_set(point)
    lonely_point/1, % ?Lonely:point
    point/1, % ?Point:point
    point/2, % ?Expression:expression
             % ?Point:point
    point/3, % ?Expression:expression
             % ?Space:space
             % ?Point:point
    point/6, % ?Space:space
             % ?FromArgument:uri
             % ?Relation:expression_definition
             % ?ToArgument:uri
             % ?Expression:expression
             % ?Point:point
    point_id/2, % ?Point:point
                % ?PointBareID:number
    point_index/4, % ?ComponentCloud:component_cloud
                   % ?Point:point
                   % ?Type:atom
                   % ?Index:integer
    point_input_component/2, % ?Point:point
                             % ?Component:component
    point_output_component/2, % ?Point:point
                              % ?Component:component
    point_to_ccm_label/2, % +Point:point
                          % -PointCCMLabel:atom
    point_to_label/2, % +Point:point
                      % -PointLabel:atom
    point_to_components/2, % +Point:point
                           % -Components:ord_set(component)
    point_to_dot_name/2, % +Point:point
                         % -PointDOTName:atom
    point_to_input_components/2, % +Point:point
                                 % -InputComponents:ord_set(component)
    point_to_output_components/2, % +Point:point
                                  % -OutputComponents:ord_set(component)
    points/1, % -Points:ord_set(point)
    support_point/1, % ?SupportPoint:point

% POINT CLOUDS
    isolated_point_cloud/1, % ?IsolatedPointCloud:point_cloud
    isolated_point_clouds/1, % -IsolatedPointClouds:ord_set(point_cloud)
    lonely_point_cloud/1, % ?Lonely:point_cloud
    point_cloud/1, % ?PointCloud:point_cloud
    point_cloud/2, % ?Expression:expression
                   % ?PointCloud:point_cloud
    point_cloud/3, % ?InputComponents:ord_set(component)
                   % ?OutputComponents:ord_set(component)
                   % ?PointCloud:point_cloud
    point_cloud/4, % ?Expression:expression
                   % ?Space:space
                   % ?Point:point
                   % ?PointCloud:point_cloud
    point_cloud/7, % ?Expression:expression
                   % ?FromArgument:uri
                   % ?Relation:expression_definition
                   % ?ToArgument:uri
                   % ?Space:space
                   % ?Point:point
                   % ?PointCloud:point_cloud
    point_cloud_id/2, % ?PointCloud:point_cloud
                      % ?PointCloudID:number
    point_cloud_index/4, % +ComponentCloud:component_cloud
                         % +PointCloud:point_cloud
                         % ?Type:atom
                         % ?Index:integer
    point_cloud_input_component/2, % ?PointCloud:point_cloud
                                   % ?InputComponent:component
    point_cloud_input_component_cloud/2, % ?PointCloud:point_cloud
                                         % ?InputComponentCloud:component_cloud
    point_cloud_label/2, % +PointCloud:point_cloud
                         % -PointCloudLabel:atom
    point_cloud_output_component/2, % ?PointCloud:point_cloud
                                    % ?OutputComponent:component
    point_cloud_output_component_cloud/2, % ?PointCloud:point_cloud
                                          % ?OutputComponentCloud:component_cloud
    point_cloud_to_ccm_label/2, % +PointCloud:point_cloud
                                % -PointCloudCCMLabel:atom
    point_cloud_to_dot_name/2, % +PointCloud:point_cloud
                               % -PointCloudDOTName:atom
    point_cloud_to_component_clouds/2, % +PointCloud:point_cloud
                                       % -ComponentClouds:ord_set(component_cloud)
    point_cloud_to_input_components/2, % +PointCloud:point_cloud
                                       % -InputComponents:ord_set(component)
    point_cloud_to_output_components/2, % +PointCloud:point_cloud
                                        % -OutputComponents:ord_set(component)
    point_cloud_to_points/2, % +PointCloud:point_cloud
                             % -Points:ord_set(point)
    point_cloud_to_spaces/2, % +PointCloud:point_cloud
                             % -Spaces:ord_set(space)
    point_clouds/1, % -Points:ord_set(point)
    point_clouds_to_component_clouds/2, % +OldPointClouds:ord_set(point_clouds)
                                        % -ComponentClouds:ord_set(component_cloud)
    point_clouds_to_component_clouds/3, % +OldPointClouds:ord_set(point_clouds)
                                        % +NumberOfComponentClouds:integer
                                        % -RelevantComponentClouds:ord_set(component_cloud)
    support_point_cloud/1, % ?SupportPointCloud:point_cloud

% SPACES
    between_space/3, % +FromSpace:space
                     % -InBetweenSpace:space
                     % +ToSpace:space
    global_space/1, % ?GlobalSpace:space
    initial_state_transtition/1, % ?InitialTransition:space_transition
    input_space/1, % ?InputSpace:space
    space/1, % ?Space:space
    space_component/2, % ?Space:space
                       % ?Component:component
    space_delta/3, % +FromSpace:space
                   % -SpaceDelta:integer
                   % +ToSpace:space
    space_expression/2, % ?Space:space
                        % ?Expression:expression
    space_id/2, % ?Space:space
                % ?SpaceID:number
    space_label/2, % ?Space:space
                   % ?Label:atom
    space_point/2, % ?Space:space
                   % ?Point:point
    space_point_cloud/2, % ?Space:space
                         % ?PointCloud:point_cloud
    space_next_to_space/2, % ?Space1:space
                           % ?Space2:space
    space_to_ccm_label/2, % +Space:space
                          % -CCMLabel:atom
    space_to_dot_name/2, % ?Space:space
                         % ?SpaceCCMName:atom
    space_to_expressions/2, % +Space:space
                            % -Expressions:ord_set(expression)
    space_to_point_clouds/2, % +Space:space
                             % -PointClouds:ord_set(point_cloud)
    space_to_points/2, % +Space:space
                       % -Points:ord_set(point)
    spaces/1, % -Spaces:ord_set(space)

% SPACE TRANSITIONS
    space_transition/1, % +Space:space
    space_transition/2, % +FromSpace:space
                        % +ToSpace:space
    space_transition_from_space/2, % ?SpaceTransition:space_transition
                                   % ?FromSpace:space
    space_transition_id/2, % ?SpaceTransition:transition
                              % ?SpaceTransitionID:number
    space_transition_label/2, % ?SpaceTransition:transition
                              % ?SpaceTransitionLabel:atom
    space_transition_to_component/2, % +SpaceTransition:space_transition
                                     % -Component:component
    space_transition_to_dot_name/2, % ?SpaceTransition:transition
                                    % ?SpaceTransitionDOTName:atom
    space_transition_to_space/2, % ?SpaceTransition:space_transition
                                 % ?ToSpace:space

% STATES
    start_state/1, % ?StartState:state
    start_states/1, % -StartStates:ord_set(state)
    state/1, % ?State:state
    state_delta/3, % +FromState:state
                   % -StateDelta:integer
                   % +ToState:state
    state_id/2, % ?State:state
                % ?StateID:number
    state_label/2, % ?State:state
                   % ?StateLabel:atom
    state_name/2, % ?State:state
                  % ?StateName:atom
    state_point/2, % ?State:state
                   % ?Point:point
    state_status/2, % ?State:state
                    % ?Status:atom
    state_to_ccm_label/2, % +State:state
                          % -StateCCMLabel:atom
    state_to_component/2, % +State:state
                          % -Component:component
    state_to_dot_name/2, % +State:state
                         % -StateDOTName:atom
    state_to_from_states/2, % +State:state
                            % -FromStates:ord_set(state)
    state_to_to_states/2, % +State:state
                          % -ToStates:ord_set(state)
    states/1, % -States:ord_set(state)

% STATE TRANSITIONS
    state_transition/1, % ?Resource:state_transition
    state_transition/2, % ?FromState:state
                        % ?ToState:state
    state_transition/3, % ?FromState:state
                        % ?ToState:state
                        % ?StateTransition:state_transition
    state_transition_by_names/3, % +FromStateName:number
                                 % +ToStateName:number
                                 % -StateTransition:state_transition
    state_transition_from_state/2, % ?StateTransition:state_transition
                                   % ?FromState:state
    state_transition_id/2, % +StateTransition:transition
                           % -StateTransitionID:atom
    state_transition_label/2, % +StateTransition:transition
                              % -StateTransitionLabel:atom
    state_transition_to_ccm_label/2, % +StateTransition:state_transition
                                     % -StateTransitionCCMLabel:atom
    state_transition_to_dot_name/2, % +StateTransition:state_transition
                                    % -StateTransitionDOTName:atom
    state_transition_to_state/2, % ?StateTransition:state_transition
                                 % ?ToState:state
    state_transitions/1, % -ord_set(state_transition)

% BELIEF
    known_or_unknown/3 % ?Diagnosis:diagnosis
                       % ?Environment:environment
                       % ?Point:point
  ]
).

/** <module> CCM API v2

The rewrite fo the CCM API so that all methods are instantiable in the
following ways: (+,+), (+,-), (-,+), (-,-).

@author Wouter Beek
@version 2012/04-2012/08
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_db)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_conflict)).
:- use_module(ccm(ccm_export)).
:- use_module(ccm(ccm_verb)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(atom_ext)).
:- use_module(generic(list_ext)).
:- use_module(generic(math_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qr(qr_api)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).



% COMPONENTS %

competitive_component(CompetitiveComponent):-
  rdfs_individual_of(CompetitiveComponent, component:competitive).

%% component(?Component:component) is nondet.
% Components, i.e. component instances.
%
% @param Component A component.

component(Component):-
  rdfs_individual_of(Component, component:component),
  \+(component_cloud(Component)).

%% component(
%%   ?Type:component_definition,
%%   +Inputs:list(resource/point),
%%   +Supports:list(resource/point),
%%   +Outputs:list(resource/point),
%%   ?Component:component
%% ) is nondet.

component(Type, Inputs, Supports, Outputs, Component):-
  component_definition_component(Type, Component),

  % Component / point relations.
  findall(
    InputRelation/Input,
    component_input_point(Component, InputRelation, Input),
    Inputs
  ),
  findall(
    SupportRelation/Support,
    component_support_point(Component, SupportRelation, Support),
    Supports
  ),
  findall(
    OutputRelation/Output,
    component_output_point(Component, OutputRelation, Output),
    Outputs
  ),

  % Check spaces.
  first(Inputs, _InputRelation/Input),
  space_point(FromSpace, Input),
  first(Outputs, _OutputRelation/Output),
  space_point(ToSpace, Output),
  component_space_delta(FromSpace, Component, ToSpace).

%% component_abbreviation(
%%   ?Component:component,
%%   ?ComponentAbbreviation:atom
%% ) is nondet.
% Pairs of components and their abbreviations.
% We do not require a component to have an abbreviation.
% We impose no restriction on the number of abbreviations a component
% may have.
%
% @param Component A component.
% @param ComponentAbbreviation An atmoic abbreviation of a component.

component_abbreviation(Component, ComponentAbbreviation):-
  component_definition_component(ComponentDefinition, Component),
  component_definition_abbreviation(
    ComponentDefinition,
    ComponentAbbreviation
  ).

%% component_cardinality(
%%   ?Component:component,
%%   ?Cardinality:integer
%% ) is nondet.
% Pairs of components and their cardinality.
%
% @param Component A base or aggregate component.
% @param Cardinality The number of base components represented by the given
%        base or aggregate component.

component_cardinality(Component, Cardinality):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_cardinality(ComponentCloud, Cardinality).

%% component_definition_component(
%%   ?ComponentDefinition:component_definition,
%%   ?Component:component
%% ) is nondet.
% Pairs of component definitions and components that are instances thereof.
% We make no assumptions as to how many component definitions
% a component may have.
% We make no assumptions as to how many components a component
% definition may have.
%
% @param ComponentDefinition A component definition.
% @param Component A component.

component_definition_component(ComponentDefinition, Component):-
  nonvar(Component),
  !,
  % The component definition is the same for each component
  % in a component cloud.
  once(component_cloud_component(ComponentCloud, Component)),
  component_definition_component_cloud(ComponentDefinition, ComponentCloud).
component_definition_component(ComponentDefinition, Component):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_cloud_component(ComponentCloud, Component).

component_expression(Component, Expression):-
  component_point_cloud(Component, PointCloud),
  expression_point_cloud(Expression, PointCloud).

component_expression(Component, Relation, Expression):-
  component_point_cloud(Component, Relation, PointCloud),
  expression_point_cloud(Expression, PointCloud).

%% component_expression(
%%    +Component:component,
%%    +Relation:uri,
%%    +Environment:environment,
%%    -Expression:expression
%% ) is nondet.
% Returns an expression that has the given relation with the given component
% under the given component assumptions.
%
% @param Component A component.
% @param Relation A component-to-point relation.
% @param Environment An environment.
% @param Expression An expression.

component_expression(Component, Relation, Environment, Expression):-
  component_point(Component, Relation, Environment, Point),
  point(Expression, Point).

%% component_id(?Component:component, ?ComponentID:number) is nondet.
% A component and its numeric identifier.
%
% @param Component A component.
% @param ComponentID A numeric component identifier.

component_id(Component, ComponentID):-
  nonvar(Component),
  !,
  component_id_(Component, ComponentID),
  % A component has exactly one identifier.
  !.
component_id(Component, ComponentID):-
  component_id_(Component, ComponentID).

component_id_(Component, ComponentID):-
  rdf_datatype(Component, component:has_id, integer, ComponentID, ccm).

component_input_point(Component, InputPoint):-
  component_input_point(Component, _Relation, InputPoint).

component_input_point(Component, Relation, InputPoint):-
  component_point(Component, Relation, InputPoint),
  rdfs_subproperty_of(Relation, component:has_input).

component_input_point_cloud(Component, InputPointCloud):-
  component_input_point_cloud(Component, _Relation, InputPointCloud).

component_input_point_cloud(Component, Relation, InputPointCloud):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_input_point_cloud(
    ComponentCloud,
    Relation,
    InputPointCloud
  ).

%% component_label(?Component:component, ?ComponentLabel:atom) is nondet.
% Pairs of components and their natural language labels.
%
% @param Component A component.
% @param LanguageLabel The atom natural language label of a component.

component_label(Component, ComponentLabel):-
  nonvar(Component),
  !,
  component_label_(Component, ComponentLabel),
  % A component has exactly one component label.
  !.
component_label(Component, ComponentLabel):-
  component_label_(Component, ComponentLabel).

component_label_(Component, ComponentLabel):-
  rdfs_label(Component, ComponentLabel).

component_output_point(Component, OutputPoint):-
  component_output_point(Component, _Relation, OutputPoint).

component_output_point(Component, Relation, OutputPoint):-
  component_point(Component, Relation, OutputPoint),
  rdfs_subproperty_of(Relation, component:has_output).

%% component_output_point_cloud(
%%   +Component:component,
%%   -OutputPointCloud:point_cloud
%% ) is nondet.
% Succeeds for a pair of component and one of its direct output point clouds.
%
% @param Component A component.
% @param OutputPointCloud A point cloud.

component_output_point_cloud(Component, OutputPointCloud):-
  component_output_point_cloud(Component, _Relation, OutputPointCloud).

component_output_point_cloud(Component, Relation, OutputPointCloud):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_output_point_cloud(
    ComponentCloud,
    Relation,
    OutputPointCloud
  ).

component_point(Component, Point):-
  component_point(Component, _Relation, Point).

component_point(Component, Relation, Point):-
  rdfs(Component, component:has_point, Point, ccm),
  rdf(Component, Relation, Point, ccm).

%% component_point(
%%    +Component:component,
%%    +Relation:uri,
%%    +Environment:environment,
%%    -Point:point
%% ) is det.
% Returns a point that has the given relation with the given component.
%
% @param Component A component.
% @param Relation A component-to-point relation.
% @param Environment An environment.
% @param Point A point.

component_point(Component, Relation, Environment, Point):-
  component_point(Component, Relation, Point_),
  alternative_points(Point_, Point),
  environment(ATMS, Environment),
  diagnosis_atms(Diagnosis, ATMS),
  known_or_unknown(Diagnosis, Environment, Point).

%% component_point_cloud(
%%   ?Component:component,
%%   ?PointCloud:point_cloud
%% ) is nondet.
% Pairs of components and point clouds.
%
% @param Component A component.
% @param PointCloud A point cloud.

component_point_cloud(Component, PointCloud):-
  component_point_cloud(Component, _Relation, PointCloud).

%% component_point_cloud(
%%   ?Component:component,
%%   ?Relation:uri,
%%   ?PointCloud:point_cloud
%% ) is nondet.
% Returns that specific point cloud that is directly related
% to the given component via a relation of the given type.
%
% @param Component A component.
% @param Relation A relation between components and point clouds.
% @param PointCloud A point cloud.

component_point_cloud(Component, ComponentRelation, PointCloud):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_component_relation(
    ComponentCloudRelation,
    ComponentRelation
  ),
  component_cloud_point_cloud(ComponentCloud, ComponentCloudRelation, PointCloud).

%% component_space(
%%   ?Component:component,
%%   ?Relation:uri,
%%   ?Space:space
%% ) is nondet.

component_space(Component, Relation, Space):-
  nonvar(Component),
  nonvar(Relation),
  !,
  component_space_(Component, Relation, Space),
  !.
component_space(Component, Relation, Space):-
  component_space_(Component, Relation, Space).

component_space_(Component, Relation, Space):-
  component_point(Component, Relation, Point),
  space_point(Space, Point).

component_space_delta(Component, SpaceDelta):-
  component_definition_component(ComponentDefinition, Component),
  component_definition_space_delta(ComponentDefinition, SpaceDelta).

component_space_delta(FromSpace, Component, ToSpace):-
  component_definition_component(ComponentDefinition, Component),
  component_definition_space_delta(FromSpace, ComponentDefinition, ToSpace).

%% component_support_component(
%%    ?Component:component,
%%    ?SupportComponent:component
%% ) is nondet.
% Returns the retrieval components that have support input for the given
% component.
%
% @param Component A component.
% @param SupportComponent A retrieval component.

component_support_component(Component, SupportComponent):-
  component_support_point_cloud(Component, SupportPointCloud),
  point_cloud_input_component(SupportPointCloud, SupportComponent).

%% component_support_expression(
%%   ?Component:component,
%%   ?SupportExpression:support_expression
%% ) is nondet.

component_support_expression(Component, SupportExpression):-
  nonvar(SupportExpression),
  !,
  expression_point_cloud(SupportExpression, SupportPointCloud),
  component_support_point_cloud(Component, SupportPointCloud).
component_support_expression(Component, SupportExpression):-
  component_support_point_cloud(Component, SupportPointCloud),
  expression_point_cloud(SupportExpression, SupportPointCloud).

component_support_point(Component, SupportPoint):-
  component_support_point(Component, _Relation, SupportPoint).

component_support_point(Component, Relation, SupportPoint):-
  component_point(Component, Relation, SupportPoint),
  rdfs_subproperty_of(Relation, component:has_support).

%% component_support_point_cloud(
%%   ?Component:component,
%%   ?SupportPointCloud:point_cloud
%% ) is nondet.
% Pairs of components and support point clouds.
%
% @param Component A component.
% @param SupportPointCloud A support point cloud.

component_support_point_cloud(Component, SupportPointCloud):-
  component_support_point_cloud(Component, _Relation, SupportPointCloud).

component_support_point_cloud(Component, Relation, SupportPointCloud):-
  component_cloud_component(ComponentCloud, Component),
  component_cloud_support_point_cloud(
    ComponentCloud,
    Relation,
    SupportPointCloud
  ).

component_to_ccm_label(Component, ComponentLabel):-
  component_abbreviation(Component, ComponentAbbreviation),!,
  component_id(Component, ComponentID),
  format(atom(ComponentLabel), '~w:~w', [ComponentAbbreviation, ComponentID]).

component_to_dot_name(Component, ComponentDOTName):-
  component_id(Component, ComponentID),
  format(atom(ComponentDOTName), 'c_~w', [ComponentID]).

%% component_to_input_points(
%%   +Component:component,
%%   -InputPoints:ord_set
%% ) is det.
% Returns the input points of the given component.
%
% @param Component A component.
% @param InputPoints An ordered set of points.

component_to_input_points(Component, InputPoints):-
  setoff(
    InputPoint,
    component_input_point(Component, InputPoint),
    InputPoints
  ).

%% component_to_input_point_clouds(
%%   +Component:component,
%%   -InputPointClouds:list(point_cloud)
%% ) is det.
% Returns the input point clouds of the given component.
%
% @param Component A component.
% @param InputPointClouds A list of point clouds.

component_to_input_point_clouds(Component, InputPointClouds):-
  component_to_point_specifications(Component, PointSpecifications),
  findall(
    InputPointCloud,
    (
      member(PointSpecification, PointSpecifications),
      point_specification_relation(PointSpecification, InputRelation),
      rdfs_subproperty_of(InputRelation, component:has_input),
      once(component_point_cloud(Component, InputRelation, InputPointCloud))
    ),
    InputPointClouds
  ).

%% component_to_output_points(
%%   +Component:component,
%%   -OutputPoints:ord_set(point)
%% ) is det.
% Returns the output points.
%
% @param Component A component.
% @param OutputPoints An ordered set of points.

component_to_output_points(Component, OutputPoints):-
  setoff(
    OutputPoint,
    component_output_point(Component, OutputPoint),
    OutputPoints
  ).

%% component_to_output_point_clouds(
%%   +Component:component,
%%   -OutputPointClouds:list(resource/point_cloud)
%% ) is nondet.
% Component to one of its direct output point cloud.
%
% @param Component A component.
% @param OutputPointClouds A list of point clouds.

component_to_output_point_clouds(Component, OutputPointClouds):-
  component_to_point_specifications(Component, PointSpecifications),
  findall(
    OutputPointCloud,
    (
      member(PointSpecification, PointSpecifications),
      point_specification_relation(PointSpecification, OutputRelation),
      rdfs_subproperty_of(OutputRelation, component:has_output),
      once(component_point_cloud(Component, OutputRelation, OutputPointCloud))
    ),
    OutputPointClouds
  ).

%% component_to_point_clouds(
%%   +Component:component,
%%   -PointClouds:list(point_cloud)
%% ) is det.
% Returns the point clouds that are directly related to the given
% component.
%
% @param Component A component.
% @param PointClouds A list of point clouds.

component_to_point_clouds(Component, PointClouds):-
  component_to_point_specifications(Component, PointSpecifications),
  findall(
    PointCloud,
    (
      member(PointSpecification, PointSpecifications),
      point_specification_relation(PointSpecification, Relation),
      once(component_point_cloud(Component, Relation, PointCloud))
    ),
    PointClouds
  ).

component_to_point_specifications(Component, PointSpecifications):-
  once(component_definition_component(ComponentDefinition, Component)),
  % Do not use setoff here, the order in which the point specifications
  % occur in the component definition is meaningful.
  % Also, do not use rdfs, since that will include point
  % specifications that have been redefined in a lower class.
  findall(
    PointSpecification,
    component_definition_point_specification(
      ComponentDefinition,
      PointSpecification
    ),
    PointSpecifications
  ).

component_to_points(Component, Points):-
  setoff(
    Point,
    component_point(Component, Point),
    Points
  ).

%% component_to_support_points(
%%   +Component,
%%   -SupportPoints:ord_set(point)
%% ) is det.
% Returns the support points of the given component.
%
% @param Component A component.
% @param SupportPoints An ordered set of points.

component_to_support_points(Component, SupportPoints):-
  setoff(
    SupportPoint,
    component_support_point(Component, SupportPoint),
    SupportPoints
  ).

%% component_to_support_point_clouds(
%%   +Component,
%%   -SupportPointClouds:list(resource/point_cloud)
%% ) is det.
% Returns the support point clouds of the given component.
%
% @param Component A component.
% @param SupportPointClouds A list of point clouds.

component_to_support_point_clouds(Component, SupportPointClouds):-
  component_to_point_specifications(Component, PointSpecifications),
  findall(
    SupportPointCloud,
    (
      member(PointSpecification, PointSpecifications),
      point_specification_relation(PointSpecification, SupportRelation),
      rdfs_subproperty_of(SupportRelation, component:has_support),
      once(
        component_point_cloud(Component, SupportRelation, SupportPointCloud)
      )
    ),
    SupportPointClouds
  ).

component_transition(FromComponent, ToComponent):-
  component_output_point(FromComponent, InBetweenPoint),
  (
    component_input_point(ToComponent, InBetweenPoint)
  ;
    component_support_point(ToComponent, InBetweenPoint)
  ).

%% components(-Components:ord_set(component)) is det.
% Returns all components.
%
% @param Components An ordered set of components.

components(Components):-
  setoff(
    Component,
    component(Component),
    Components
  ).

%% components_to_points(
%%   +Components:ord_set(component),
%%   -Points:ord_set(point)
%% ) is det.
% Returns the points that are in between the given =Components=.
%
% @param Components An ordered set of components.
% @param Points An ordered set of points.

components_to_points(Components, Points):-
  setoff(
    Point,
    (
      member(Component, Components),
      component_point(Component, Point)
    ),
    Points
  ).

%% disabled_component(?Disabled:component) is nondet.
% Disabled components are subsuming components that have been unpacked.
%
% @param Disabled A component.

disabled_component(Disabled):-
  nonvar(Disabled),
  !,
  component_cloud_component(Disabled_, Disabled),
  disabled_component_cloud(Disabled_).
disabled_component(Disabled):-
  disabled_component_cloud(Disabled_),
  component_cloud_component(Disabled_, Disabled).

%% retrieval_component(?RetrievalComponent:component) is nondet.
% Retrieval components.
%
% @param RetrievalComponent A component.

retrieval_component(Retrieval):-
  component_cloud_component(Retrieval_, Retrieval),
  retrieval_component_cloud(Retrieval_).

subsumed_component(Subsumed):-
  component_cloud_component(Subsumed_, Subsumed),
  subsumed_component_cloud(Subsumed_).

%% termination_component(?Termination:component) is nondet.
% Termination components.
%
% @param Termination A component.

termination_component(Termination):-
  component_cloud_component(Termination_, Termination),
  termination_component_cloud(Termination_).

%% transition_component(?TransitionComponent:component) is semidet.
% Transition components.
%
% @param TransitionComponent A component.

transition_component(Transition):-
  component_cloud_component(Transition_, Transition),
  transition_component_cloud(Transition_).

%% within_state_component(?WithinState:component) is nondet.
% Within-state components.
%
% @param WithinState A component.

within_state_component(WithinState):-
  component_cloud_component(WithinState_, WithinState),
  within_state_component_cloud(WithinState_).



% COMPONENTS CLOUDS %

active_component_cloud(ComponentCloud):-
  \+(subsumed_component_cloud(ComponentCloud)),
  \+(disabled_component_cloud(ComponentCloud)).

aggregate_component_cloud(Aggregate):-
  rdfs_individual_of(Aggregate, component:aggregate).

%% bidirectional_component_cloud(?Bidirectional:component_cloud) is nondet.
% Bidirectional component clouds.
%
% @param Bidirectional A component cloud.

bidirectional_component_cloud(Bidirectional):-
  rdfs_individual_of(Bidirectional, component:bidirectional).

competitive_component_cloud(CompetitiveComponentCloud):-
  rdfs_individual_of(CompetitiveComponentCloud, component:competitive).

component_cloud(ComponentCloud):-
  rdfs_individual_of(ComponentCloud, component_cloud:component_cloud).

%% component_cloud(
%%   ?Type:component_definition,
%%   ?Inputs:list(resource/point_cloud),
%%   ?Supports:list(resource/point_cloud),
%%   ?Outputs:list(resource/point_cloud),
%%   ?ComponentCloud:component_cloud
%% ) is nondet.

component_cloud(Type, Inputs, Supports, Outputs, ComponentCloud):-
  component_definition_component_cloud(Type, ComponentCloud),

  % Component cloud / point cloud relations.
  findall(
    InputComponentCloudRelation/InputPointCloud,
    (
      component_definition_point_specification(
        Type,
        Index,
        InputComponentRelation,
        _InputPointSpecification
      ),
      rdfs_subproperty_of(InputComponentRelation, component:has_input),
      component_cloud_component_relation(
        InputComponentCloudRelation,
        InputComponentRelation
      ),
      rdf(ComponentCloud, InputComponentCloudRelation, InputPointCloud, ccm)
    ),
    Inputs
  ),
  findall(
    SupportComponentCloudRelation/SupportPointCloud,
    (
      component_definition_point_specification(
        Type,
        Index,
        SupportComponentRelation,
        _SupportPointSpecification
      ),
      rdfs_subproperty_of(SupportComponentRelation, component:has_support),
      component_cloud_component_relation(
        SupportComponentCloudRelation,
        SupportComponentRelation
      ),
      rdf(
        ComponentCloud,
        SupportComponentCloudRelation,
        SupportPointCloud,
        ccm
      )
    ),
    Supports
  ),
  findall(
    OutputComponentCloudRelation/OutputPointCloud,
    (
      component_definition_point_specification(
        Type,
        Index,
        OutputComponentRelation,
        _OutputPointSpecification
      ),
      rdfs_subproperty_of(OutputComponentRelation, component:has_output),
      component_cloud_component_relation(
        OutputComponentCloudRelation,
        OutputComponentRelation
      ),
      rdf(ComponentCloud, OutputComponentCloudRelation, OutputPointCloud, ccm)
    ),
    Outputs
  ),

  % Check spaces.
  first(Inputs, _InputRelation/Input),
  space_point_cloud(FromSpace, Input),
  first(Outputs, _OutputRelation/Output),
  space_point_cloud(ToSpace, Output),
  component_cloud_space_delta(FromSpace, ComponentCloud, ToSpace).

component_cloud_abbreviation(ComponentCloud, ComponentCloudAbbreviation):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_definition_abbreviation(
    ComponentDefinition,
    ComponentCloudAbbreviation
  ).

component_cloud_cardinality(ComponentCloud, Cardinality):-
  aggregate_component_cloud(ComponentCloud),
  !,
  component_cloud_to_subsumed_component_clouds(
    ComponentCloud,
    SubsumedComponentClouds
  ),
  maplist(
    component_cloud_cardinality,
    SubsumedComponentClouds,
    SubsumedCardinalities
  ),
  sum_list(SubsumedCardinalities, Cardinality).
component_cloud_cardinality(_ComponentCloud, 1).

%% component_cloud_chain(
%%   +ComponentCloud:component_cloud,
%%   -ComponentCloudChain:list(component_cloud),
%%   -ComponentChains:list(list(component))
%% ) is nondet.
% Returns a chain of component clouds that starts with the given
% component cloud and that is followed by all component clouds
% that are of the same component definition.
%
% @param ComponentCloud A component cloud that is first in a
%        potential chain of component clouds.
% @param ComponentCloudChain A list of consecutive component clouds
%        that are of the same component definition.
% @param ComponentChains A list of litsts of components.

component_cloud_chain(ComponentCloud, ComponentCloudChain, ComponentChains):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_cloud_chain(
    ComponentCloud,
    ComponentDefinition,
    ComponentCloudChain,
    ComponentChains
  ).

component_cloud_chain(
  ComponentCloud,
  ComponentDefinition,
  ComponentCloudChain,
  ComponentChains
):-
  component_cloud_chain_(
    ComponentCloud,
    ComponentDefinition,
    ComponentCloudChain,
    ComponentChains_
  ),
  component_chains_(ComponentChains_, ComponentChains).

component_cloud_chain_(
  ComponentCloud,
  ComponentDefinition,
  [ComponentCloud | ComponentCloudChain],
  [Components | ComponentChains]
):-
  component_cloud_transition(ComponentCloud, NextComponentCloud),
  component_definition_component_cloud(
    ComponentDefinition,
    NextComponentCloud
  ),
  component_cloud_to_components(ComponentCloud, Components),
  component_cloud_chain_(
    NextComponentCloud,
    ComponentDefinition,
    ComponentCloudChain,
    ComponentChains
  ).
component_cloud_chain_(
  ComponentCloud,
  _ComponentDefinition,
  [ComponentCloud],
  [Components]
):-
  component_cloud_to_components(ComponentCloud, Components).

component_cloud_chain_to_component_chains(
  ComponentCloudChain,
  ComponentChains
):-
  maplist(
    component_cloud_to_components,
    ComponentCloudChain,
    ComponentChains_
  ),
  component_chains_(ComponentChains_, ComponentChains).

component_chains_([], []).
component_chains_([ComponentChain_ | ComponentChains_], ComponentChains):-
  length(ComponentChain_, Length),
  numlist(1, Length, Indices),
  findall(
    ComponentChain,
    (
      member(Index, Indices),
      findall(
        Component,
        (
          member(Components_, [ComponentChain_ | ComponentChains_]),
          nth1(Index, Components_, Component)
        ),
        ComponentChain
      )
    ),
    ComponentChains
  ).

% A component has exactly one component cloud.
component_cloud_component(ComponentCloud, Component):-
  nonvar(Component),
  !,
  component_cloud_component_(ComponentCloud, Component),
  !.
component_cloud_component(ComponentCloud, Component):-
  component_cloud_component_(ComponentCloud, Component).

component_cloud_component_(ComponentCloud, Component):-
  rdf(ComponentCloud, component_cloud:has_component, Component, ccm).

component_cloud_component_relation(
  ComponentCloudRelation,
  ComponentRelation
):-
  nonvar(ComponentCloudRelation),
  !,
  rdf_global_id(component_cloud:Relation, ComponentCloudRelation),
  rdf_global_id(component:Relation, ComponentRelation).
component_cloud_component_relation(
  ComponentCloudRelation,
  ComponentRelation
):-
  nonvar(ComponentRelation),
  !,
  rdf_global_id(component:Relation, ComponentRelation),
  rdf_global_id(component_cloud:Relation, ComponentCloudRelation).

component_cloud_id(ComponentCloud, ID):-
  nonvar(ComponentCloud),
  !,
  component_cloud_id_(ComponentCloud, ID),
  !.
component_cloud_id(ComponentCloud, ID):-
  nonvar(ID),
  !,
  component_cloud_id_(ComponentCloud, ID),
  !.
component_cloud_id(ComponentCloud, ID):-
  component_cloud_id_(ComponentCloud, ID).

component_cloud_id_(ComponentCloud, ID):-
  rdf_datatype(ComponentCloud, component_cloud:has_id, integer, ID, ccm).

component_cloud_input_point(ComponentCloud, InputPoint):-
  component_cloud_input_point(ComponentCloud, _InputRelation, InputPoint).

component_cloud_input_point(ComponentCloud, InputRelation, InputPoint):-
  component_cloud_point(ComponentCloud, InputRelation, InputPoint),
  rdfs_subproperty_of(InputRelation, component:has_input).

component_cloud_input_point_cloud(ComponentCloud, InputPointCloud):-
  component_cloud_input_point_cloud(
    ComponentCloud,
    _InputRelation,
    InputPointCloud
  ).

component_cloud_input_point_cloud(
  ComponentCloud,
  InputRelation,
  InputPointCloud
):-
  component_cloud_point_cloud(
    ComponentCloud,
    InputRelation,
    InputPointCloud
  ),
  rdfs_subproperty_of(InputRelation, component_cloud:has_input).

component_cloud_label(ComponentCloud, Label):-
  once(component_cloud_component(ComponentCloud, Component)),
  component_label(Component, Label).

component_cloud_output_point(ComponentCloud, OutputPoint):-
  component_cloud_output_point(ComponentCloud, _OutputRelation, OutputPoint).

component_cloud_output_point(ComponentCloud, OutputRelation, OutputPoint):-
  component_cloud_point(ComponentCloud, OutputRelation, OutputPoint),
  rdfs_subproperty_of(OutputRelation, component:has_output).

component_cloud_output_point_cloud(ComponentCloud, OutputPointCloud):-
  component_cloud_output_point_cloud(
    ComponentCloud,
    _OutputRelation,
    OutputPointCloud
  ).

component_cloud_output_point_cloud(
  ComponentCloud,
  OutputRelation,
  OutputPointCloud
):-
  component_cloud_point_cloud(
    ComponentCloud,
    OutputRelation,
    OutputPointCloud
  ),
  rdfs_subproperty_of(OutputRelation, component_cloud:has_output).

component_cloud_point(ComponentCloud, Point):-
  component_cloud_point(ComponentCloud, _Relation, Point).

component_cloud_point(ComponentCloud, Relation, Point):-
  component_cloud_component(ComponentCloud, Component),
  component_point(Component, Relation, Point).

component_cloud_point_cloud(ComponentCloud, PointCloud):-
  component_cloud_point_cloud(ComponentCloud, _Relation, PointCloud).

component_cloud_point_cloud(ComponentCloud, Relation, PointCloud):-
  nonvar(ComponentCloud),
  nonvar(Relation),
  !,
  component_cloud_point_cloud_(ComponentCloud, Relation, PointCloud),
  !.
component_cloud_point_cloud(ComponentCloud, Relation, PointCloud):-
  component_cloud_point_cloud_(ComponentCloud, Relation, PointCloud).

component_cloud_point_cloud_(ComponentCloud, Relation, PointCloud):-
  rdfs(ComponentCloud, component_cloud:has_point_cloud, PointCloud, ccm),
  rdf(ComponentCloud, Relation, PointCloud, ccm).

component_cloud_space(ComponentCloud, Space):-
  component_cloud_space(ComponentCloud, _Relation, Space).

component_cloud_space(ComponentCloud, Relation, Space):-
  component_cloud_point(ComponentCloud, Relation, Point),
  space_point(Space, Point).

component_cloud_space_delta(FromSpace, ComponentCloud, ToSpace):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_definition_space_delta(FromSpace, ComponentDefinition, ToSpace).

component_cloud_subsumes_component_cloud(Subsuming, Subsumed):-
  rdf(Subsuming, component_cloud:subsumes, Subsumed, ccm).

component_cloud_support_component_cloud(
  ComponentCloud,
  SupportComponentCloud
):-
  component_cloud_support_point_cloud(ComponentCloud, SupportPointCloud),
  component_cloud_output_point_cloud(
    SupportComponentCloud,
    SupportPointCloud
  ).

component_cloud_support_expression(ComponentCloud, SupportExpression):-
  component_cloud_support_point(ComponentCloud, SupportPoint),
  point(SupportExpression, SupportPoint).

component_cloud_support_point(ComponentCloud, SupportPoint):-
  component_cloud_support_point(
    ComponentCloud,
    _SupportRelation,
    SupportPoint
  ).

component_cloud_support_point(ComponentCloud, SupportRelation, SupportPoint):-
  component_cloud_point(ComponentCloud, SupportRelation, SupportPoint),
  rdfs_subproperty_of(SupportRelation, component:has_support).

component_cloud_support_point_cloud(ComponentCloud, SupportPointCloud):-
  component_cloud_support_point_cloud(
    ComponentCloud,
    _SupportRelation,
    SupportPointCloud
  ).

component_cloud_support_point_cloud(
  ComponentCloud,
  SupportRelation,
  SupportPointCloud
):-
  component_cloud_point_cloud(
    ComponentCloud,
    SupportRelation,
    SupportPointCloud
  ),
  rdfs_subproperty_of(SupportRelation, component_cloud:has_support).

%% component_cloud_to_behavior_rule(
%%   +ComponentCloud:component_cloud,
%%   -BehaviorRule:atom
%% ) is semidet.
% Returns the behavior rule for the given component cloud, if any.
%
% @param ComponentCloud A component cloud.
% @param BehaviorRule The atomic name of a behavior rule.

component_cloud_to_behavior_rule(ComponentCloud, BehaviorRule):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  component_definition_to_behavior_rule(ComponentDefinition, BehaviorRule),
  % A component has at most one behavior rule. We take the behavior rule
  % of the closest component definition (in the hierarchy) that has one
  % specified.
  !.

component_cloud_to_point_specifications(ComponentCloud, PointSpecifications):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  % Do not use setoff here, the order in which the point specifications
  % occur in the component definition is meaningful.
  % Also, do not use rdfs, since that will include point
  % specifications that have been redefined in a lower class.
  findall(
    PointSpecification,
    component_definition_point_specification(
      ComponentDefinition,
      PointSpecification
    ),
    PointSpecifications
  ).

component_cloud_to_ccm_label(ComponentCloud, ComponentCloudLabel):-
  component_cloud_abbreviation(ComponentCloud, ComponentCloudAbbreviation),
  component_cloud_id(ComponentCloud, ComponentCloudID),
  format(
    atom(ComponentCloudLabel),
    '~w:~w',
    [ComponentCloudAbbreviation, ComponentCloudID]
  ).

component_cloud_to_components(ComponentCloud, Components):-
  setoff(
    Component,
    component_cloud_component(ComponentCloud, Component),
    Components
  ).

component_cloud_to_dot_name(ComponentCloud, ComponentCloudDOTName):-
  component_cloud_id(ComponentCloud, ComponentCloudID),
  format(atom(ComponentCloudDOTName), 'cc_~w', [ComponentCloudID]).

component_cloud_to_input_point_clouds(ComponentCloud, InputPointClouds):-
  findall(
    InputPointCloud,
    component_cloud_input_point_cloud(ComponentCloud, InputPointCloud),
    InputPointClouds
  ).

component_cloud_to_input_spaces(ComponentCloud, InputSpaces):-
  setoff(
    InputSpace,
    (
      component_cloud_input_point(ComponentCloud, InputPoint),
      space_point(InputSpace, InputPoint)
    ),
    InputSpaces
  ).

component_cloud_to_output_point_clouds(ComponentCloud, OutputPointClouds):-
  findall(
    OutputPointCloud,
    component_cloud_output_point_cloud(ComponentCloud, OutputPointCloud),
    OutputPointClouds
  ).

component_cloud_to_output_spaces(ComponentCloud, OutputSpaces):-
  setoff(
    OutputSpace,
    (
      component_cloud_output_point(ComponentCloud, OutputPoint),
      space_point(OutputSpace, OutputPoint)
    ),
    OutputSpaces
  ).

component_cloud_to_point_clouds(ComponentCloud, PointClouds):-
  findall(
    PointCloud,
    component_cloud_point_cloud(ComponentCloud, PointCloud),
    PointClouds
  ).

component_cloud_to_spaces(ComponentCloud, Spaces):-
  setoff(
    Space,
    (
      component_cloud_point(ComponentCloud, Point),
      space_point(Space, Point)
    ),
    Spaces
  ).

component_cloud_to_spaces(ComponentCloud, Relation, Spaces):-
  setoff(
    Space,
    (
      component_cloud_point(ComponentCloud, Relation, Point),
      space_point(Space, Point)
    ),
    Spaces
  ).

component_cloud_to_subsumed_component_clouds(
  SubsumingComponentCloud,
  SubsumedComponentClouds
):-
  setoff(
    SubsumedComponentCloud,
    component_cloud_subsumes_component_cloud(
      SubsumingComponentCloud,
      SubsumedComponentCloud
    ),
    SubsumedComponentClouds
  ).

component_cloud_to_support_component_clouds(
  ComponentCloud,
  SupportComponentClouds
):-
  setoff(
    SupportComponentCloud,
    component_cloud_support_component_cloud(
      ComponentCloud,
      SupportComponentCloud
    ),
    SupportComponentClouds
  ).

component_cloud_to_support_point_clouds(ComponentCloud, SupportPointClouds):-
  findall(
    SupportPointCloud,
    component_cloud_support_point_cloud(ComponentCloud, SupportPointCloud),
    SupportPointClouds
  ).

component_cloud_to_support_spaces(ComponentCloud, SupportSpaces):-
  setoff(
    SupportSpace,
    (
      component_cloud_input_point(ComponentCloud, SupportPoint),
      space_point(SupportSpace, SupportPoint)
    ),
    SupportSpaces
  ).

component_cloud_transition(FromComponentCloud, ToComponentCloud):-
  component_cloud_output_point_cloud(FromComponentCloud, InBetweenPointCloud),
  (
    component_cloud_input_point_cloud(ToComponentCloud, InBetweenPointCloud)
  ;
    component_cloud_support_point_cloud(ToComponentCloud, InBetweenPointCloud)
  ),
  FromComponentCloud \== ToComponentCloud.

component_clouds(ComponentClouds):-
  setoff(
    ComponentCloud,
    component_cloud(ComponentCloud),
    ComponentClouds
  ).

component_clouds_to_point_clouds(ComponentClouds, PointClouds):-
  setoff(
    PointCloud,
    (
      member(ComponentCloud, ComponentClouds),
      component_cloud_point_cloud(ComponentCloud, PointCloud)
    ),
    PointClouds
  ).

% A component cloud has exactly one (direct) component definition.
component_definition_component_cloud(ComponentDefinition, ComponentCloud):-
  nonvar(ComponentCloud),
  !,
  component_definition_component_cloud_(ComponentDefinition, ComponentCloud),
  !.
component_definition_component_cloud(ComponentDefinition, ComponentCloud):-
  component_definition(ComponentDefinition),
  component_definition_component_cloud_(ComponentDefinition, ComponentCloud).

component_definition_component_cloud_(ComponentDefinition, ComponentCloud):-
  rdfs_individual_of(ComponentCloud, ComponentDefinition).

%% continuity_component_cloud(?Continuity:component_cloud) is nondet.
% Continuity component clouds.
%
% @param Continuity A component cloud.

continuity_component_cloud(Continuity):-
  rdfs_individual_of(Continuity, component:continuity).

disabled_component_cloud(Disabled):-
  rdfs_individual_of(Disabled, component_cloud:disabled).

find_component_cloud(Type, Inputs, Supports, Outputs, ComponentCloud):-
  component_definition_component_cloud(Type, ComponentCloud),

  % Component cloud / point cloud relations.
  forall(
    member(InputComponentCloudRelation_/InputPointCloud, Inputs),
    (
      component_definition_point_specification(
        Type,
        Index,
        InputComponentRelation,
        _InputPointSpecification
      ),
      rdfs_subproperty_of(InputComponentRelation, component:has_input),
      component_cloud_component_relation(
        InputComponentCloudRelation,
        InputComponentRelation
      ),
      rdf(ComponentCloud, InputComponentCloudRelation, InputPointCloud, ccm),
      rdfs_subproperty_of(
        InputComponentCloudRelation_,
        InputComponentCloudRelation
      )
    )
  ),
  forall(
    member(SupportComponentCloudRelation_/SupportPointCloud, Supports),
    (
      component_definition_point_specification(
        Type,
        Index,
        SupportComponentRelation,
        _SupportPointSpecification
      ),
      rdfs_subproperty_of(SupportComponentRelation, component:has_support),
      component_cloud_component_relation(
        SupportComponentCloudRelation,
        SupportComponentRelation
      ),
      rdf(
        ComponentCloud,
        SupportComponentCloudRelation,
        SupportPointCloud,
        ccm
      ),
      rdfs_subproperty_of(
        SupportComponentCloudRelation_,
        SupportComponentCloudRelation
      )
    )
  ),
  forall(
    member(OutputComponentCloudRelation_/OutputPointCloud, Outputs),
    (
      component_definition_point_specification(
        Type,
        Index,
        OutputComponentRelation,
        _OutputPointSpecification
      ),
      rdfs_subproperty_of(OutputComponentRelation, component:has_output),
      component_cloud_component_relation(
        OutputComponentCloudRelation,
        OutputComponentRelation
      ),
      rdf(ComponentCloud, OutputComponentCloudRelation, OutputPointCloud, ccm),
      rdfs_subproperty_of(
        OutputComponentCloudRelation_,
        OutputComponentCloudRelation
      )
    )
  ),

  % Check spaces.
  first(Inputs, _InputRelation/Input),
  space_point_cloud(FromSpace, Input),
  first(Outputs, _OutputRelation/Output),
  space_point_cloud(ToSpace, Output),
  component_cloud_space_delta(FromSpace, ComponentCloud, ToSpace).

%% first_component_cloud(
%%   ?ComponentDefinition:component_definition,
%%   ?ComponentCloud:component_cloud
%% ) is nondet.
% Pairs of component definitions and component clouds that are the first
% in a potential chain.
%
% @param ComponentDefinition A component definition.
% @param ComponentCloud A component cloud.

first_component_cloud(ComponentDefinition, ComponentCloud):-
  component_definition_component_cloud(ComponentDefinition, ComponentCloud),
  \+((
    component_cloud_transition(PreviousComponent, ComponentCloud),
    component_definition_component(ComponentDefinition, PreviousComponent)
  )).

hierarchical_component_cloud(Hierarchical):-
  rdfs_individual_of(Hierarchical, component:hierarchical_aggregate).

retrieval_component_cloud(Retrieval):-
  rdfs_individual_of(Retrieval, component:retrieval).

%% subsumed_component_cloud(?Subsumed:component_cloud) is nondet.
% Subsumed component clouds.
%
% @param Subsumed A component cloud.

subsumed_component_cloud(Subsumed):-
  nonvar(Subsumed),
  !,
  subsumed_component_cloud_(Subsumed),
  !.
subsumed_component_cloud(Subsumed):-
  subsumed_component_cloud_(Subsumed).

subsumed_component_cloud_(Subsumed):-
  rdf(_Subsuming, component_cloud:subsumes, Subsumed, ccm).

termination_component_cloud(Termination):-
  rdfs_individual_of(Termination, component:termination).

transition_component_cloud(Transition):-
  rdfs_individual_of(Transition, component:space_transition).

unpackable_component_cloud(UnpackableComponentCloud):-
  hierarchical_component_cloud(UnpackableComponentCloud),
  \+(disabled_component_cloud(UnpackableComponentCloud)).

within_state_component_cloud(WithinState):-
  rdfs_individual_of(WithinState, component:within_state).



% COMPONENT DEFINITIONS %

aggregate_component_definition(AggregateComponentDefinition):-
  rdfs_subclass_of(AggregateComponentDefinition, component:aggregate).

bidirectional_component_definition(BidirectionalComponentDefinition):-
  rdfs_subclass_of(BidirectionalComponentDefinition, component:bidirectional).

can_instantiate_component_definition(ComponentDefinition):-
  rdf_datatype(ComponentDefinition, component:can_instantiate, boolean, true, ccm).

%% component_definition(
%%   ?ComponentDefinition:component_definition
%% ) is nondet.
% A component definition.
%
% @param ComponentDefinition A component definition.

component_definition(ComponentDefinition):-
  rdfs_subclass_of(ComponentDefinition, component_cloud:component_cloud).

%% component_definition_abbreviation(
%%  ?ComponentDefinition:component_definition,
%%  ?ComponentDefinitionAbbreviation:atom
%% ) is nondet.
% Pairs of component definitions and their abbreviations.
% We do not require a component definition to have an abbreviation.
% We impose no restriction on the number of abbreviations a component
% definition may have.
%
% @param ComponentDefinition A component definition.
% @param ComponentDefinitionAbbreviation An atmoic abbreviation
%        of a component definition.

component_definition_abbreviation(
  ComponentDefinition,
  ComponentDefinitionAbbreviation
):-
  rdf_literal(
    ComponentDefinition,
    component:has_abbreviation,
    ComponentDefinitionAbbreviation,
    ccm
  ).

component_definition_cardinality(ComponentDefinition, Cardinality):-
  findall(
    PointSpecification,
    component_definition_point_specification(
      ComponentDefinition,
      PointSpecification
    ),
    PointSpecifications
  ),
  length(PointSpecifications, Cardinality).

%% component_definition_id(
%%   ?ComponentDefintion:component_definition,
%%   ?ComponentDefinitionID:number
%% ) is nondet.
% Pairs of component definitions and their identifier.
%
% @param ComponentDefinition A component definition.
% @param ComponentDefinitionID The numeric identifier for
%        a component definition.

component_definition_id(ComponentDefinition, ComponentDefinitionID):-
  component_definition_label(ComponentDefinition, ComponentDefinitionLabel),
  spaces_to_underscores(ComponentDefinitionLabel, ComponentDefinitionID).

%% component_definition_label(
%%    ?ComponentDefinition:component_definition,
%%    ?ComponentDefinitionLabel:atom
%% ) is nondet.
% Pairs of component definitions and their natural language labels.
%
% @param ComponentDefinition A component definition.
% @param ComponentDefinitionLabel The atomic natural language label
%        of a component definition.

component_definition_label(ComponentDefinition, ComponentDefinitionLabel):-
  rdfs_label(ComponentDefinition, ComponentDefinitionLabel).

%% component_definition_point_specification(
%%   ?ComponentDefinition:component_definition,
%%   ?PointSpecification:uri
%% ) is nondet.
% Pairs of component definitions and point specifications.
%
% @param ComponentDefinition A component definition.
% @param PointSpecification A point specification.

component_definition_point_specification(
  ComponentDefinition,
  PointSpecification
):-
  rdf(
    ComponentDefinition,
    component:has_point_specification,
    PointSpecification,
    ccm
  ).

%% component_definition_point_specification(
%%   ?ComponentDefinition:component_definition,
%%   ?Index:number,
%%   ?PointSpecification:uri
%% ) is nondet.
% Pairs of component definitions and point specifications.
%
% @param ComponentDefinition A component definition.
% @param Index The index of the point specification with respect to its
%        component definition.
% @param PointSpecification A point specification.

component_definition_point_specification(
  ComponentDefinition,
  Index,
  PointSpecification
):-
  nonvar(ComponentDefinition),
  nonvar(Index),
  !,
  component_definition_point_specification_(
    ComponentDefinition,
    Index,
    PointSpecification
  ),
  !.
component_definition_point_specification(
  ComponentDefinition,
  Index,
  PointSpecification
):-
  component_definition_point_specification_(
    ComponentDefinition,
    Index,
    PointSpecification
  ).

component_definition_point_specification_(
  ComponentDefinition,
  Index,
  PointSpecification
):-
  component_definition_point_specification(
    ComponentDefinition,
    PointSpecification
  ),
  point_specification_index(PointSpecification, Index).

%% component_definition_point_specification(
%%   ?ComponentDefinition:component_definition,
%%   ?Index:number,
%%   ?Relation:uri,
%%   ?PointSpecification:uri
%% ) is nondet.
% Pairs of component definitions and point specifications.
%
% @param ComponentDefinition A component definition.
% @param Index The index of the point specification with respect to its
%        component definition.
% @param Relation The relation definition between a component
%        instantiation of the component definition and a point cloud
%        instantiation of the point specification.
% @param PointSpecification A point specification.

component_definition_point_specification(
  ComponentDefinition,
  Index,
  Relation,
  PointSpecification
):-
  component_definition_point_specification(
    ComponentDefinition,
    Index,
    PointSpecification
  ),
  point_specification_relation(PointSpecification, Relation).

component_definition_space_delta(ComponentDefinition, Delta):-
  nonvar(ComponentDefinition),
  !,
  component_definition_to_space_delta(ComponentDefinition, Delta),
  !.
component_definition_space_delta(ComponentDefinition, Delta):-
  component_definition_space_delta_(ComponentDefinition, Delta).

component_definition_to_space_delta(ComponentDefinition, Delta):-
  component_definition_space_delta_(ComponentDefinition, Delta),
  !.
component_definition_to_space_delta(_ComponentDefinition, 0).

component_definition_space_delta_(ComponentDefinition, Delta):-
  rdf_datatype(ComponentDefinition, component:has_space_delta, integer, Delta, ccm).

component_definition_space_delta(FromSpace, ComponentDefinition, ToSpace):-
  component_definition_space_delta(ComponentDefinition, SpaceDelta),
  space_delta(FromSpace, SpaceDelta, ToSpace).

%% component_definition_to_behavior_rule(
%%   +ComponentDefinition:component_definition,
%%   -BehaviorRule:atom
%% ) is semidet.
% Returns the behavior rule for the given component definition, if any.
%
% @param Component Definition A component definition.
% @param BehaviorRule The atomic name of a behavior rule.

component_definition_to_behavior_rule(ComponentDefinition, BehaviorRule):-
  rdf_literal(
    ComponentDefinition,
    component:has_behavior_rule,
    BehaviorRule,
    ccm
  ),
  % [CONVENTION] A component definition has at most one behavior rule.
  !.

component_definition_to_dot_name(
  ComponentDefinition,
  ComponentDefinitionDOTName
):-
  component_definition_id(ComponentDefinition, ComponentDefinitionID),
  format(atom(ComponentDefinitionDOTName), 'cd_~w', [ComponentDefinitionID]).

component_definition_to_point_relation(ComponentDefinition, PointRelation):-
  component_definition_point_specification(
    ComponentDefinition,
    PointSpecification
  ),
  point_specification_relation(PointSpecification, PointRelation).

component_definition_to_point_relations(ComponentDefinition, PointRelations):-
  findall(
    PointRelation,
    component_definition_to_point_relation(
      ComponentDefinition,
      PointRelation
    ),
    PointRelations
  ).

%% component_definitions(
%%   -ComponentDefintions:ord_set(component_definition)
%% ) is det.
% Returns the ordered set of all component definitions.
%
% @param ComponentDefinitions The ordered set of component definitions.

component_definitions(ComponentDefinitions):-
  setoff(
    ComponentDefinition,
    component_definition(ComponentDefinition),
    ComponentDefinitions
  ).

%% continuity_component_definition(
%%   ?ContinuityComponentDefinition:component_definition
%% ) is nondet.
% Succeeds if the given component definition is a continuity component
% definition.
%
% @param ContinuityComponentDefinition A component definition.

continuity_component_definition(ContinuityComponentDefinition):-
  rdfs_subclass_of(ContinuityComponentDefinition, component:continuity).

disabled_component_definition(DisabledComponentDefinition):-
  rdfs_subclass_of(DisabledComponentDefinition, component:disabled).

%% instantiable_component_definition(
%%   ?InstantiableComponentDefinition:component_definition
%% ) is nondet.
% Instantiable component definitions, i.e. component definitions
% that allow concrete component instances to be made.
%
% @param InstantiableComponentDefinition A component defintion.

instantiable_component_definition(ComponentDefinition):-
  component_definition(ComponentDefinition),
  can_instantiate_component_definition(ComponentDefinition).

% A point specification has exactly one point expression definition.
point_specification_expression_definition(
  PointSpecification,
  ExpressionDefinition
):-
  nonvar(PointSpecification),
  !,
  point_specification_expression_definition_(
    PointSpecification,
    ExpressionDefinition
  ),
  !.
point_specification_expression_definition(
  PointSpecification,
  ExpressionDefinition
):-
  point_specification_expression_definition_(
    PointSpecification,
    ExpressionDefinition
  ).

point_specification_expression_definition_(
  PointSpecification,
  ExpressionDefinition
):-
  rdf(
    PointSpecification,
    component:has_point_expression_definition,
    ExpressionDefinition,
    ccm
  ).

point_specification_index(PointSpecification, Index):-
  nonvar(PointSpecification),
  !,
  point_specification_index_(PointSpecification, Index),
  !.
point_specification_index(PointSpecification, Index):-
  point_specification_index_(PointSpecification, Index).

point_specification_index_(PointSpecification, Index):-
  rdf_datatype(PointSpecification, component:has_point_id, integer, Index, ccm).

%% point_specification_point(
%%   +Component:component,
%%   +PointSpecification:uri,
%%   -Point:point
%% ) is nondet.
% Returns a point that has the given relation to the given component.
% Remember that the combination of component and component/point-relation
% determines a point cloud, but a point cloud may have multiple points.
%
% @param Component A component.
% @param Point Specification A component/point-relation.
% @param Point A point.

point_specification_point(Component, PointSpecification, Point):-
  point_specification_relation(PointSpecification, PointRelation),
  component_point(Component, PointRelation, Point).

point_specification_point(Component, PointSpecification, Environment, Point):-
  point_specification_relation(PointSpecification, PointRelation),
  component_point(Component, PointRelation, Environment, Point).

%% point_specification_relation(
%%   ?PointSpecification:uri,
%%   ?Relation:uri
%% ) is det.
% Returns the component/point-relation for the given =PointSpecification=.
% Every point specification has exactly one component/point-relation.
%
% @param PointSpecification
% @param Relation

point_specification_relation(PointSpecification, Relation):-
  nonvar(PointSpecification),
  !,
  point_specification_relation_(PointSpecification, Relation),
  !.
point_specification_relation(PointSpecification, Relation):-
  point_specification_relation_(PointSpecification, Relation).

point_specification_relation_(PointSpecification, Relation):-
  rdf(PointSpecification, component:has_point_relation, Relation, ccm).

point_specification_relation_name(PointSpecification, RelationName):-
  nonvar(PointSpecification),
  !,
  point_specification_relation_name_(PointSpecification, RelationName),
  !.
point_specification_relation_name(PointSpecification, RelationName):-
  point_specification_relation_name_(PointSpecification, RelationName).

point_specification_relation_name_(PointSpecification, RelationName):-
  rdf_literal(
    PointSpecification,
    component:has_point_name,
    RelationName,
    ccm
  ).

%% retrieval_component_definition(
%%   ?RetrievalComponentDefinition:component_definition
%% ) is nondet.
% Succeeds if the given component is a retrieval component.
%
% @param RetrievalComponentDefinition A component definition.

retrieval_component_definition(RetrievalComponentDefinition):-
  rdfs_subclass_of(RetrievalComponentDefinition, component:retrieval).

space_transition_component_definition(ComponentDefinition):-
  rdfs_subclass_of(ComponentDefinition, component:space_transition).

state_transition_component_definition(ComponentDefinition):-
  rdfs_subclass_of(ComponentDefinition, component:state_transition).

%% termination_component_definition(
%%   ?TerminationComponentDefinition:component_definition
%% ) is nondet.
% Succeeds if the given component definition is a termination component
% definition.
%
% @param TerminationComponentDefinition A component definition.

termination_component_definition(TerminationComponentDefinition):-
  rdfs_subclass_of(TerminationComponentDefinition, component:termination).

%% within_state_component_definition(
%%   ?WithinStateComponentDefinition:component_definition
%% ) is nondet.
% Succeeds if the given component definition is a within state component
% definition.
%
% @param WithinStateComponentDefinition A component definition.

within_state_component_definition(WithinStateComponentDefinition):-
  rdfs_subclass_of(WithinStateComponentDefinition, component:within_state).



% EXPRESSIONS %

%% aggregate_expression(?Expression:expression) is nondet.

aggregate_expression(Expression):-
  rdfs_individual_of(Expression, expression:aggregate).

%% aggregate_expression(+Expression:expression, -Aggregate:boolean) is det.

aggregate_expression(Expression, true):-
  aggregate_expression(Expression),
  !.
aggregate_expression(_Expression, false).

%% bidirectional_expression(?Expression:expression) is nondet.
% An bidirectional expression.
%
% @param Expression An expression.

bidirectional_expression(Expression):-
  rdfs_individual_of(Expression, expression:bidirectional),
  % QP-expressions may be bidirectional, e.g. magnitude value expressions,
  % so we need to check for the right expression definition types.
  (
    rdfs_individual_of(Expression, expression:cc)
  ;
    rdfs_individual_of(Expression, expression:pp)
  ;
    rdfs_individual_of(Expression, expression:qq)
  ).

%% closure(+Space:space) is det.
% Esablish closure in the given space.
% A closure is reached by applying a conditional point onto the
% non-conditional points in the same space. This is done consequentially,
% until all applicable conditionals have been applied.

closure(Space):-
  findall(
    Consequence,
    (
      point(
        _Conditional,
        Condition,
        expression:conditional,
        Consequence,
        Space,
        _ConditionalPoint
      ),
      point(Condition, Space, _ConditionPoint),
      \+(point(Consequence, Space, _ConsequencePoint))
    ),
    Consequences
  ),
  (
    Consequences == []
  ->
    true
  ;
    maplist(find_or_add_point(Space), Consequences, _ConsequencePoints),
    closure(Space)
  ).

conditional_expression(Expression):-
  rdfs_individual_of(Expression, expression:conditional).

%% expression(?Expression:expression) is nondet.
% Expressions.
%
% @param Expression An expression.

expression(Expression):-
  rdfs_individual_of(Expression, expression:expression).

expression(Property, Argument, Expression):-
  (
    nonvar(Property),
    nonvar(Argument)
  ;
    nonvar(Expression)
  ),
  !,
  expression_(Property, Argument, Expression).
expression(Property, Argument, Expression):-
  expression_(Property, Argument, Expression).

expression_(Property, Argument, Expression):-
  expression_definition(Property),
  rdfs_individual_of(Expression, Property),
  expression_argument(Expression, Argument).

%% expression(
%%   ?FromArgument:uri,
%%   ?Relation:expression_definition,
%%   ?ToArgument:uri,
%%   ?Expression:expression
%% ) is nondet.
% Expressions
%
% @param FromArgument A resource.
% @param Relation An expression definition.
% @param ToArgument A resource.
% @param Expression An expression.

expression(FromArgument, Relation, ToArgument, Expression):-
  (
    nonvar(Relation),
    nonvar(FromArgument),
    nonvar(ToArgument)
  ;
    nonvar(Expression)
  ),
  !,
  expression_(FromArgument, Relation, ToArgument, Expression),
  !.
expression(FromArgument, Relation, ToArgument, Expression):-
  expression_(FromArgument, Relation, ToArgument, Expression).

expression_(FromArgument, Relation, ToArgument, Expression):-
  expression_definition(Relation),
  rdfs_individual_of(Expression, Relation),
  expression_from_argument(Expression, FromArgument),
  expression_to_argument(Expression, ToArgument).

expression_argument(Expression, Argument):-
  expression(Expression),
  rdf(Expression, expression:has_argument, Argument, ccm).

%% expression_id(?Expression:expression, ?ExpressionID:number) is nondet.
% Pairs of experessions and expression identifiers.
%
% @param Expression An expression.
% @param ExpressionID A numeric identifier of an expression.

expression_id(Expression, ExpressionID):-
  var(Expression),
  var(ExpressionID),
  !,
  expression_id_(Expression, ExpressionID).
expression_id(Expression, ExpressionID):-
  expression_id_(Expression, ExpressionID),
  % There is a one-to-one mapping between expressions and their
  % identifiers.
  !.

expression_id_(Expression, ExpressionID):-
  rdf_datatype(Expression, expression:has_id, integer, ExpressionID, ccm).

%% expression_from_argument(
%%   ?Expression:expression,
%%   ?FromArgument:uri
%% ) is det.
% The relation between an expression and its from argument.
%
% @param Expression An expression.
% @param FromArgument A URI.

expression_from_argument(Expression, FromArgument):-
  expression(Expression),
  rdf(Expression, expression:has_from_argument, FromArgument, ccm).

%% expression_point_cloud(
%%   ?Expression:expression,
%%   ?PointCloud:point_cloud
%% ) is nondet.
% Pairs of expressions and point clouds.
%
% @param Expression An expression.
% @param PointCloud A point cloud.

expression_point_cloud(Expression, PointCloud):-
  point_cloud(Expression, _Space, _Point, PointCloud).

%% expression_space(?Expression:expression, ?Space:space) is nondet.
% Pairs of expressions and spaces.
%
% @param Expression An expression.
% @param Space A space.

expression_space(Expression, Space):-
  point(Expression, Space, _Point).

expression_subsumes_expression(SubsumingExpression, SubsumedExpression):-
  component_cloud_support_expression(
    SubsumingComponentCloud,
    SubsumingExpression
  ),
  component_cloud_subsumes_component_cloud(
    SubsumingComponentCloud,
    SubsumedComponentCloud
  ),
  component_cloud_support_expression(
    SubsumedComponentCloud,
    SubsumedExpression
  ).

%% expression_to_argument(
%%   ?Expression:expression,
%%   ?ToArgument:uri
%% ) is nondet.
% The relation between an expression and its to argument.
%
% @param Expression An expression.
% @param ToArgument A URI.

expression_to_argument(Expression, ToArgument):-
  expression(Expression),
  rdf(Expression, expression:has_to_argument, ToArgument, ccm).

expression_to_dot_name(Expression, ExpressionDOTName):-
  expression_id(Expression, ExpressionID),
  format(atom(ExpressionDOTName), 'x_~w', [ExpressionID]).

expression_to_subsumed_expressions(Expression, SubsumedExpressions):-
  findall(
    SubsumedExpression,
    expression_subsumes_expression(Expression, SubsumedExpression),
    SubsumedExpressions
  ).

%% expressions(-Expressions:ord_set(expression)) is det.
% Returns the ordered set of all expressions.
%
% @param Expressions The ordered set of expressions.

expressions(Expressions):-
  setoff(
    Expression,
    expression(Expression),
    Expressions
  ).

%% find_alternative_expression(
%%   +Expression:expression,
%%   -SubsententialPart:uri,
%%   -AlternativeExpression:expression
%% ) is semidet.
% Returns an alternative expression, i.e. one that only differs
% from the given expression in the returned subsentential part.
%
% @param Expression An expression
% @param SubsententialPart Either a quantity value or a relation (either
%        an inequality or a support relation).
% @param AlternativeExpression An expression.

find_alternative_expression(
  Expression,
  DerivativeValue,
  AlternativeExpression
):-
  find_alternative_expression_(
    Expression,
    DerivativeValue,
    AlternativeExpression
  ),
  AlternativeExpression \== Expression.

find_alternative_expression_(
  Expression,
  DerivativeValue,
  AlternativeExpression
):-
  derivative_quantity_value_expression(Expression),
  !,
  % The quantity did not change.
  from_quantity_expression(FromQuantity, Expression),!,
  expression(
    FromQuantity,
    expression:qp_derivative_equal_to,
    DerivativeValue,
    AlternativeExpression
  ).
find_alternative_expression_(
  Expression,
  MagnitudeValue,
  AlternativeExpression
):-
  magnitude_quantity_value_expression(Expression),
  !,
  % The quantity did not change.
  from_quantity_expression(FromQuantity, Expression),
  expression(
    FromQuantity,
    expression:qp_magnitude_equal_to,
    MagnitudeValue,
    AlternativeExpression
  ).
find_alternative_expression_(Expression, Relation, AlternativeExpression):-
  % The arguments did not change.
  expression_from_argument(Expression, FromArgument),!,
  expression_to_argument(Expression, ToArgument),!,
  expression(FromArgument, Relation, ToArgument, AlternativeExpression).

%% find_or_add_alternative_expression(
%%   +Expression:expression,
%%   +AlternativeSubsententialExpression:uri
%%   -AlternativeExpression:expression
%% ) is det.
% Retruns the newly created or existing alternative expression that
% deviates from the given expression only in the given subsentential
% expression.
% Since the subsentential element is given, we can potentially add
% an expression.
%
% @param Expresssion An expression.
% @param AlternativeSubsententialExpression A resource.
% @param AlternativeExpression An expression.

find_or_add_alternative_expression(
  Expression,
  SubsententialExpression,
  AlternativeExpression
):-
  find_alternative_expression(
    Expression,
    SubsententialExpression,
    AlternativeExpression
  ),
  !.
find_or_add_alternative_expression(
  Expression,
  SubsententialExpression,
  AlternativeExpression
):-
  find_or_add_alternative_expression_(
    Expression,
    SubsententialExpression,
    AlternativeExpression
  ).

find_or_add_alternative_expression_(
  Expression,
  DerivativeValue,
  AlternativeExpression
):-
  derivative_quantity_value_expression(Expression),
  !,
  % The quantity did not change.
  from_quantity_expression(FromQuantity, Expression),!,
  find_or_add_expression(
    FromQuantity,
    expression:qp_derivative_equal_to,
    DerivativeValue,
    AlternativeExpression
  ).
find_or_add_alternative_expression_(
  Expression,
  MagnitudeValue,
  AlternativeExpression
):-
  magnitude_quantity_value_expression(Expression),
  !,
  % The quantity did not change.
  from_quantity_expression(FromQuantity, Expression), !,
  find_or_add_expression(
    FromQuantity,
    expression:qp_magnitude_equal_to,
    MagnitudeValue,
    AlternativeExpression
  ).
find_or_add_alternative_expression_(Expression, Relation, AlternativeExpression):-
  % The arguments did not change.
  expression_from_argument(Expression, FromArgument),!,
  expression_to_argument(Expression, ToArgument),!,
  find_or_add_expression(FromArgument, Relation, ToArgument, AlternativeExpression).

%% find_expression_definition(
%%   +SuperExpressionDefinitions:list(expression_definition),
%%   -SubExpressionDefinition:expression_definition
%% ) is nondet.
% Returns an expression definitions that is a subset of the intersection
% of the given expression definitions.

find_expression_definition(
  SuperExpressionDefinitions,
  SubExpressionDefinition
):-
  % rdf_meta does not work on lists, so...
  rdf_global_ids(SuperExpressionDefinitions, SuperExpressionDefinitions_),
  find_expression_definition_(
    SuperExpressionDefinitions_,
    SubExpressionDefinition
  ).

find_expression_definition_(
  [SuperExpressionDefinition | SuperExpressionDefinitions],
  SubExpressionDefinition
):-
  % We need a subexpression to start with. Taking an arbitrary
  % subdefinition of the head superdefinitions is more efficient
  % than taking a random expression definition.
  rdfs_subclass_of(SubExpressionDefinition, SuperExpressionDefinition),
  forall(
    member(SuperExpressionDefinition, SuperExpressionDefinitions),
    rdfs_subclass_of(SubExpressionDefinition, SuperExpressionDefinition)
  ).

inverted_expression(Expression):-
  rdfs_individual_of(Expression, expression:inverted).

subsumed_expression(Expression):-
  expression_subsumes_expression(_SubsumingExpression, Expression).

%% support_expression(?Expression:expression) is nondet.
% A support expression.

support_expression(Expression):-
  rdfs_individual_of(Expression, expression:support).

%% unidirectional_expression(?Expression:expression) is nondet.
% A directed relation.

unidirectional_expression(Expression):-
  \+(bidirectional_expression(Expression)).

uninverted_expression(Expression):-
  \+(inverted_expression(Expression)).

%% within_state_expression(?Expression:expression) is nondet.
% A within state expression.

within_state_expression(Expression):-
  rdfs_individual_of(Expression, expression:within_state).



% EXPRESSION DEFINITIONS %

%% bidirectional_expression_definition(
%%   ?BidirectionalExpressionDefinition:expression_definition
%% ) is nondet.
% An bidirectional expression definition.
%
% @param BidirectionalExpressionDefinition An expression definition.

bidirectional_expression_definition(BidirectionalExpressionDefinition):-
  rdfs_individual_of(
    BidirectionalExpressionDefinition,
    expression:bidirectional
  ),

  % QP-expressions may be bidirectional, e.g. magnitude value expressions,
  % so we need to check for the right expression definition types.
  (
    rdfs_individual_of(BidirectionalExpressionDefinition, expression:cc)
  ;
    rdfs_individual_of(BidirectionalExpressionDefinition, expression:pp)
  ;
    rdfs_individual_of(BidirectionalExpressionDefinition, expression:qq)
  ).

%% can_instantiate_expression_definition(
%%   ?ExpressionDefinition:expression_definition
%% ) is nondet.
% Instantiable expression definitions.
%
% @param ?ExpressionDefinition An expression definition.

can_instantiate_expression_definition(ExpressionDefinition):-
  rdf_datatype(ExpressionDefinition, expression:can_instantiate, boolean, true, ccm).

%% expression_definition(
%%   ?ExpressionDefinition:expression_definition
%% ) is semidet.
% Expression definitions.
%
% @param ExpressionDefinition An expression definition.

expression_definition(ExpressionDefinition):-
  rdfs_subclass_of(ExpressionDefinition, expression:expression).

%% expression_definition_abbreviation(
%%   ?ExpressionDefinition:expression_definition,
%%   ?Abbreviation:atom
%% ) is nondet.
% Pairs of expression definitions and their abbreviations.
%
% @param ExpressionDefinition An expression definition.
% @param Abbreviation An atomic abbreviation of an expression definition name.

expression_definition_abbreviation(ExpressionDefinition, Abbreviation):-
  rdf_literal(
    ExpressionDefinition,
    expression:has_abbreviation,
    Abbreviation,
    ccm
  ).

%% expression_definition_expression(
%%   ?ExpressionDefinition:expression_definition,
%%   ?Expression:expression
%% ) is nondet.
% Pairs of expression definitions and their expression instances.
%
% @param ExpressionDefinition An expression definition.
% @param Expression An expression.

expression_definition_expression(ExpressionDefinition, Expression):-
  nonvar(Expression),
  !,
  expression_definition_expression1(ExpressionDefinition, Expression),
  !.
expression_definition_expression(ExpressionDefinition, Expression):-
  var(Expression),
  !,
  expression_definition(ExpressionDefinition),
  expression_definition_expression1(ExpressionDefinition, Expression).

expression_definition_expression1(ExpressionDefinition, Expression):-
  rdfs_individual_of(Expression, ExpressionDefinition).

%% expression_definition_id(
%%   ?ExpressionDefinition:expression_definition,
%%   ?ExpressionDefinitionID:number
%% ) is nondet.
% Pairs of expression definitions and their numeric identifiers.
%
% @param ExpressionDefinition An expression definition.
% @param ExpressionDefinitionID A numeric expression definition identifier.

expression_definition_id(ExpressionDefinition, ExpressionDefinitionID):-
  rdf_global_id(expression:ExpressionDefinitionName, ExpressionDefinition),
  format(atom(ExpressionDefinitionID), 'ed_~w', [ExpressionDefinitionName]).

%% expression_definition_label(
%%   ?ExpressionDefinition:expression_definition,
%%   ?ExpressionDefinitionLabel:atom
%% ) is det.
% Pairs of expression definitions and their natural language labels.
%
% @param ExpressionDefinition An expression definition.
% @param ExpressionDefinitionLabel An atomic natural language
%        label of an expression definition.

expression_definition_label(ExpressionDefinition, ExpressionDefinitionLabel):-
  rdfs_label(ExpressionDefinition, ExpressionDefinitionLabel).

expression_definition_name(ExpressionDefinition, ExpressionDefinitionName):-
  (
    inverted_expression_definition(ExpressionDefinition)
  ->
    Inverted = 'Inv.'
  ;
    Inverted = ''
  ),
  (
    bidirectional_expression_definition(ExpressionDefinition)
  ->
    Direction = ''
  ;
    Direction = 'Dir.'
  ),
  expression_definition_label(
    ExpressionDefinition,
    ExpressionDefinitionLabel
  ),
  format(
    atom(ExpressionDefinitionName),
    '~w~w ~w',
    [Inverted, Direction, ExpressionDefinitionLabel]
  ).

expression_definition_to_dot_name(
  ExpressionDefinition,
  ExpressionDefinitionDOTName
):-
  expression_definition_id(ExpressionDefinition, ExpressionDefinitionID),
  format(
    atom(ExpressionDefinitionDOTName),
    'xd_~w',
    [ExpressionDefinitionID]
  ).

%% expression_definition_to_expressions(
%%   +ExpressionDefinition:expression_definition,
%%   -Expressions:ord_set(expression)
%% ) is det.
% Returns the instance expressions of the given expression definition.
%
% @param ExpressionDefinition An expression definition.
% @param Expressions An ordered set of expressions.

expression_definition_to_expressions(ExpressionDefinition, Expressions):-
  setoff(
    Expression,
    expression_definition_expression(
      ExpressionDefinition,
      Expression
    ),
    Expressions
  ).

%% expression_definitions(
%%   -ExpressionDefinitions:ord_set(expression_definition)
%% ) is det.
% Returns the ordered set of all expression definitions.
%
% @param ExpressionDefinitions An ordered set of expression definitions.
% @tbd Add a variant that only returns instantiable expression definitions.

expression_definitions(ExpressionDefinitions):-
  setoff(
    ExpressionDefinition,
    expression_definition(ExpressionDefinition),
    ExpressionDefinitions
  ).

instantiable_expression_definitions(InstantiableExpressionDefinitions):-
  expression_definitions(ExpressionDefinitions),
  include(
    can_instantiate_expression_definition,
    ExpressionDefinitions,
    InstantiableExpressionDefinitions
  ).

inverted_expression_definition(InvertedExpressionDefinition):-
  rdfs_subclass_of(InvertedExpressionDefinition, expression:inverted).

%% support_expression_definition(
%%   ?SupportExpressionDefinition:expression_definition
%% ) is semidet.
% Succeeds if the given expression definition is a support expression
% definition.
%
% @param SupportExpressionDefinition A support expression definition.

support_expression_definition(SupportExpressionDefinition):-
  rdfs_subclass_of(SupportExpressionDefinition, expression:support).

%% unidirectional_expression_definition(
%%   ?UnidirectionalExpressionDefinition:expression_definition
%% ) is nondet.
% A directional expression definition.
%
% @param UnidirectionalExpressionDefinition A directional
%        expression definition.

unidirectional_expression_definition(UnidirectionalExpressionDefinition):-
  \+(bidirectional_expression_definition(UnidirectionalExpressionDefinition)).

uninverted_expression_definition(UninvertedExpressionDefinition):-
  rdfs_subclass_of(UninvertedExpressionDefinition, expression:uninverted).



% POINTS %

isolated_point(IsolatedPoint):-
  point_cloud(IsolatedPoint, IsolatedPointCloud),
  isolated_point_cloud(IsolatedPointCloud).

isolated_points(IsolatedPoints):-
  setoff(
    IsolatedPoint,
    isolated_point(IsolatedPoint),
    IsolatedPoints
  ).

lonely_point(Lonely):-
  point(Lonely),
  forall(
    component_point(Neighbor, Lonely),
    transition_component(Neighbor)
  ).

%% point(?Point:point) is nondet.
% A point.
%
% @param Point A point.

point(Point):-
  rdfs_individual_of(Point, point:point).

%% point(?Space:space, ?Expression:expression, ?Point:point) is nondet.
% Triples of spaces, expressions, and points.
%
% @param Space A space.
% @param Expression An expression.
% @param Point A point.

% A point has exactly one expression.
point(Expression, Point):-
  nonvar(Point),
  !,
  point_(Expression, Point),
  !.
point(Expression, Point):-
  point_(Expression, Point).

point_(Expression, Point):-
  rdf(Expression, expression:has_point, Point, ccm).

point(Expression, Space, Point):-
  point(Expression, Point),
  space_point(Space, Point).

point(Expression, FromArgument, Relation, ToArgument, Space, Point):-
  expression(FromArgument, Relation, ToArgument, Expression),
  point(Expression, Space, Point).

%% point_id(?Point:point, ?PointID:integer) is nondet.
% A point and its integer identifier.
%
% @param Point A point.
% @param PointID The numeric identifier of a point.

point_id(Point, PointID):-
  var(Point),
  var(PointID),
  !,
  point_id_(Point, PointID).
% There is a one-to-one mapping between points and their identifiers.
point_id(Point, PointID):-
  point_id_(Point, PointID),
  !.

point_id_(Point, PointID):-
  rdf_datatype(Point, point:has_id, integer, PointID, ccm).

point_index(ComponentCloud, Point, Type, Index):-
  point_cloud(Point, PointCloud),
  point_cloud_index(ComponentCloud, PointCloud, Type, Index).

point_input_component(Point, Component):-
  rdfs(Point, point:has_input_component, Component, ccm).

point_output_component(Point, Component):-
  rdfs(Point, point:has_output_component, Component, ccm).

point_to_ccm_label(Point, PointLabel):-
  point_color(Point, PointColor),
  space_point(Space, Point),
  space_to_ccm_label(Space, SpaceCCMLabel),
  point_id(Point, PointID),
  point(Expression, Point),
  expression_to_ccm_label(Expression, ExpressionLabel),
  format(
    atom(PointLabel),
    '<font color="~w">P~w=(S~w</font><font color="black">:E~w)</font>',
    [PointColor, PointID, SpaceCCMLabel, ExpressionLabel]
  ).

%% point_to_label(+Point:point, -PointLanguageLabel:atom) is det.
% Returns the natural language label of the given point.
%
% @param Point A point.
% @param PointLanguageLabel The atomic natural language label of a point.

point_to_label(Point, PointLabel):-
  once(point_to_sentence(Point, PointLabel)).

%% point_to_components(+Point:point, -Components:ord_set(component)) is det.
% Returns the components that the given point is directly connected with.
%
% @param Point A point.
% @param Components An ordered set of components.

point_to_components(Point, Components):-
  point_to_input_components(Point, InputComponents),
  point_to_output_components(Point, OutputComponents),
  ord_union(InputComponents, OutputComponents, Components).

point_to_dot_name(Point, PointDOTName):-
  point_id(Point, PointID),
  format(atom(PointDOTName), 'p_~w', [PointID]).

%% point_to_input_components(
%%   +Point:point,
%%   -InputComponents:ord_set(component)
%% ) is det.
% Returns the input components for the given point.
%
% @param Point A point.
% @param InputComponents An ordered set of components.

point_to_input_components(Point, InputComponents):-
  setoff(
    InputComponent,
    point_input_component(Point, InputComponent),
    InputComponents
  ).

%% point_to_output_components(
%%   +Point:point,
%%   -OutputComponents:ord_set(component)
%% ) is det.
% Returns the out component of the given point.
%
% @param Point A point.
% @param OutputComponents An ordered set of components.

point_to_output_components(Point, OutputComponents):-
  setoff(
    OutputComponent,
    point_output_component(Point, OutputComponent),
    OutputComponents
  ).

%% points(-Points:ord_set(point)) is det.
% Returns all points.
%
% @param Points An ordered set of points.

points(Points):-
  setoff(
    Point,
    point(Point),
    Points
  ).

support_point(Support):-
  nonvar(Support),
  !,
  point_cloud(Support, SupportPointCloud),
  support_point_cloud(SupportPointCloud).
support_point(Support):-
  support_point_cloud(SupportPointCloud),
  point_cloud(Support, SupportPointCloud).



% POINT CLOUDS %

%% isolated_point(?IsolatedPointCloud:point_cloud) is nondet.
% Isolated point clouds.
% An isolated point cloud has no in- or outcomponents.
%
% @param IsolatedPointCloud A point cloud.

isolated_point_cloud(IsolatedPointCloud):-
  point_cloud([], [], IsolatedPointCloud).

%% isolated_points(-IsolatedPointClouds:ord_set(point_cloud)) is det.
% Retuns all isolated point clouds.
%
% @param IsolatedPointClouds An ordered set of point clouds.

isolated_point_clouds(IsolatedPointClouds):-
  setoff(
    IsolatedPointCloud,
    isolated_point(IsolatedPointCloud),
    IsolatedPointClouds
  ).

%% lonely_point_cloud(?Lonely:point_cloud) is nondet.
% Lonely point clouds.
% A lonely point cloud is one that has no direct component cloud neighbors.
%
% @param Lonely A point cloud.

lonely_point_cloud(Lonely):-
  point_cloud(Lonely),
  forall(
    component_cloud_point_cloud(Neighbor, Lonely),
    transition_component_cloud(Neighbor)
  ).

%% point_cloud(?PointCloud:point_cloud) is nondet.
% A point cloud.
%
% @param PointCloud A point cloud.

point_cloud(PointCloud):-
  rdfs_individual_of(PointCloud, point_cloud:point_cloud).

%% point_cloud(?Point:point, ?PointCloud:point_cloud) is nondet.
% The relation between a point cloud and its points.
%
% @param Point A point.
% @param PointCloud A point cloud.

% A point has exactly one point cloud.
point_cloud(Point, PointCloud):-
  nonvar(Point),
  !,
  point_cloud_(Point, PointCloud),
  !.
point_cloud(Point, PointCloud):-
  point_cloud_(Point, PointCloud).

point_cloud_(Point, PointCloud):-
  rdf(PointCloud, point_cloud:has_point, Point, ccm).

point_cloud(InputComponents, OutputComponents, PointCloud):-
  point_cloud(PointCloud),
  point_cloud_to_input_components(PointCloud, InputComponents),
  point_cloud_to_output_components(PointCloud, OutputComponents).

point_cloud(Expression, Space, Point, PointCloud):-
  nonvar(Space),
  !,
  point(Expression, Space, Point),
  point_cloud(Point, PointCloud).
point_cloud(Expression, Space, Point, PointCloud):-
  point_cloud(Point, PointCloud),
  point(Expression, Space, Point).

point_cloud(
  Expression,
  FromArgument,
  Relation,
  ToArgument,
  Space,
  Point,
  PointCloud
):-
  point_cloud(Point, PointCloud),
  point(Expression, FromArgument, Relation, ToArgument, Space, Point).

%% point_cloud_id(
%%   ?PointCloud:point_cloud,
%%   ?ID:integer
%% ) is nondet.
% A point cloud and its integer identifier.
%
% @param PointCloud A point cloud.
% @param ID A numeric identifier of a point cloud.

% There is a one-to-one mapping between point clouds and their identifiers.
point_cloud_id(PointCloud, ID):-
  var(PointCloud),
  var(ID),
  !,
  point_cloud_id_(PointCloud, ID).
point_cloud_id(PointCloud, ID):-
  point_cloud_id_(PointCloud, ID),
  !.

point_cloud_id_(PointCloud, ID):-
  rdf_datatype(PointCloud, point_cloud:has_id, integer, ID, ccm).

point_cloud_index(ComponentCloud, PointCloud, input, Index):-
  component_cloud_to_input_point_clouds(ComponentCloud, InputPointClouds),
  nth0(Index, InputPointClouds, PointCloud),
  !.
point_cloud_index(ComponentCloud, PointCloud, output, Index):-
  component_cloud_to_output_point_clouds(ComponentCloud, OutputPointClouds),
  nth0(Index, OutputPointClouds, PointCloud),
  !.
point_cloud_index(ComponentCloud, PointCloud, support, Index):-
  component_cloud_to_support_point_clouds(ComponentCloud, SupportPointClouds),
  nth0(Index, SupportPointClouds, PointCloud),
  !.

%% point_cloud_input_component(
%%   ?PointCloud:point_cloud,
%%   ?Component:component
%% ) is nondet.
% Pairs of point clouds and input components.
% The components are input to the point clouds.
%
% @param PointCloud A point cloud.
% @param InputComponent A component.

point_cloud_input_component(PointCloud, InputComponent):-
  component_output_point_cloud(InputComponent, PointCloud).

point_cloud_input_component_cloud(PointCloud, InputComponentCloud):-
  component_cloud_output_point_cloud(InputComponentCloud, PointCloud).

point_cloud_label(PointCloud, PointCloudLabel):-
  rdfs_label(PointCloud, PointCloudLabel).

point_cloud_output_component(PointCloud, OutputComponent):-
  component_input_point_cloud(OutputComponent, PointCloud).
point_cloud_output_component(PointCloud, OutputComponent):-
  component_support_point_cloud(OutputComponent, PointCloud).

point_cloud_output_component_cloud(PointCloud, OutputComponentCloud):-
  component_cloud_input_point_cloud(OutputComponentCloud, PointCloud).
point_cloud_output_component_cloud(PointCloud, OutputComponentCloud):-
  component_cloud_support_point_cloud(OutputComponentCloud, PointCloud).

point_cloud_to_ccm_label(PointCloud, PointCloudLabel):-
  point_cloud_id(PointCloud, PointCloudID),
  point_cloud_to_points(PointCloud, Points),
  maplist(point_to_ccm_label, Points, PointLabels),
  atomic_list_concat(PointLabels, '<br/>', PointsLabel),
  format(
    atom(PointCloudLabel),
    '<font color="black">PC~w</font><br/>~w',
    [PointCloudID, PointsLabel]
  ).

%% point_cloud_to_component_clouds(
%%   +PointCloud:point_cloud,
%%   -ComponentClouds:ord_set(component_cloud)
%% ) is det.
% Returns the component clouds that are directly related to the given
% point cloud.
%
% @param PointCloud A point cloud.
% @param ComponentClouds An ordered set of component clouds.

point_cloud_to_component_clouds(PointCloud, ComponentClouds):-
  setoff(
    ComponentCloud,
    component_cloud_point_cloud(ComponentCloud, PointCloud),
    ComponentClouds
  ).

point_cloud_to_dot_name(PointCloud, PointCloudDOTName):-
  point_cloud_id(PointCloud, PointCloudID),
  format(atom(PointCloudDOTName), 'pc_~w', [PointCloudID]).

%% point_cloud_to_input_components(
%%   +PointCloud:point_cloud,
%%   -InputComponents:ord_set(component)
%% ) is det.
% Returns the input components of the given point cloud.
%
% @param PointCloud A point cloud.
% @param InputComponents An ordered set of components.

point_cloud_to_input_components(PointCloud, InputComponents):-
  setoff(
    InputComponent,
    point_cloud_input_component(PointCloud, InputComponent),
    InputComponents
  ).

point_cloud_to_output_components(PointCloud, OutputComponents):-
  setoff(
    OutComponent,
    point_cloud_output_component(PointCloud, OutComponent),
    OutputComponents
  ).

%% point_cloud_to_points(
%%   +PointCloud:point_cloud,
%%   -Points:ord_set(point)
%% ) is det.
% Returns the points that are in the given =PointCloud=.
%
% @param PointCloud A point cloud.
% @param Points An ordered set of points.

point_cloud_to_points(PointCloud, Points):-
  setoff(
    Point,
    point_cloud(Point, PointCloud),
    Points
  ).

point_cloud_to_spaces(PointCloud, Spaces):-
  setoff(
    Space,
    space_point_cloud(Space, PointCloud),
    Spaces
  ).

%% point_clouds(-PointClouds:ord_set(point_cloud)) is det.
% Returns all point clouds.
%
% @param PointClouds An ordered set of point clouds.

point_clouds(PointClouds):-
  setoff(
    PointCloud,
    point_cloud(PointCloud),
    PointClouds
  ).

point_clouds_to_component_clouds(PointClouds, ComponentClouds):-
  setoff(
    ComponentCloud,
    (
      member(PointCloud, PointClouds),
      component_cloud_point_cloud(ComponentCloud, PointCloud),
      active_component_cloud(ComponentCloud)
    ),
    ComponentClouds
  ).

point_clouds_to_component_clouds(
  PointClouds,
  NumberOfComponentClouds,
  RelevantComponentClouds
):-
  point_clouds_to_component_clouds(
    PointClouds,
    NumberOfComponentClouds,
    [],
    [],
    RelevantComponentClouds
  ).

%% point_clouds_to_component_clouds(
%%   +OldPointClouds:ord_set(point_clouds),
%%   +NumberOfComponentClouds:integer,
%%   +AllComponentClouds:ord_set(component_cloud),
%%   +LastAddedComponentClouds:ord_set(component_cloud),
%%   -RelevantComponentClouds:ord_set(component_cloud)
%% ) is det.
% Scopes the component clouds, so that exactly the given number of
% component clouds are within the scope (provided this number is not
% larger than the number of unscoped component clouds).
% Scoping is perfomed in a smart way starting out with the expectations
% the learner gave given as input.
%
% @param Expectations An ordered set of expectation points.
% @param NumberOfComponentClouds A nonnegative integer.
% @param AllComponentClouds An ordered set of component clouds. These are all
%        the component clouds that we have until now.
% @param LastAddedComponentClouds An ordered set of component clouds. These
%        are the component clouds that we added in the last step of recursion.
% @param RelevantComponentClouds An ordered set of component clouds. This is
%        the solution for this predicate.

point_clouds_to_component_clouds(
  _OldPointClouds,
  NumberOfComponentClouds,
  AllComponentClouds,
  LastAddedComponentClouds,
  RelevantComponentClouds
):-
  length(AllComponentClouds, AllComponentCloudsLength),
  AllComponentCloudsLength >= NumberOfComponentClouds,
  !,
  RemoveNumber is AllComponentCloudsLength - NumberOfComponentClouds,
  length(LastAddedComponentClouds, AllNumber),
  AddNumber is AllNumber - RemoveNumber,
  length(VarList, AddNumber),
  append(VarList, RemoveComponentClouds, LastAddedComponentClouds),
  ord_subtract(
    AllComponentClouds,
    RemoveComponentClouds,
    RelevantComponentClouds
  ).
point_clouds_to_component_clouds(
  OldPointClouds,
  NumberOfComponentClouds,
  OldAllComponentClouds,
  _LastAddedComponents,
  RelevantComponentClouds
):-
  % From the old point clouds to all directly connected component clouds.
  maplist(point_cloud_to_component_clouds, OldPointClouds, ComponentCloudss),
  ord_union([OldAllComponentClouds | ComponentCloudss], NewComponentClouds_),
  exclude(
    disabled_component_cloud,
    NewComponentClouds_,
    NewAllComponentClouds
  ),

  % Single out those component clouds that were added in the last
  % recursion step.
  ord_subtract(
    NewAllComponentClouds,
    OldAllComponentClouds,
    NewLastAddedComponentClouds
  ),

  % If no component clouds were added in the last recursion step, then
  % the predicate is done and all component clouds are returned.
  % Otherwise, the directly connected point clouds are retrieved, and
  % we enter another recusrive call.
  (
    NewLastAddedComponentClouds == []
  ->
    RelevantComponentClouds = OldAllComponentClouds
  ;
    maplist(
      component_cloud_to_point_clouds,
      NewAllComponentClouds,
      NewPointCloudss
    ),
    ord_union([OldPointClouds | NewPointCloudss], NewPointClouds),
    point_clouds_to_component_clouds(
      NewPointClouds,
      NumberOfComponentClouds,
      NewAllComponentClouds,
      NewLastAddedComponentClouds,
      RelevantComponentClouds
    )
  ).

%% support_point_cloud(?Support:point_cloud) is nondet.
% Succeeds if the given point is a support point.
% Support points are points that have a retrieval component as
% their input component.
%
% @param Support A support point cloud.

support_point_cloud(Support):-
  nonvar(Support),
  !,
  component_cloud_point_cloud(Retrieval, Support),
  retrieval_component_cloud(Retrieval).
support_point_cloud(Support):-
  retrieval_component_cloud(Retrieval),
  component_cloud_point_cloud(Retrieval, Support).



% SPACES %

between_space(Space, Space, Space):-
  !.
between_space(FromSpace, InBetweenSpace, ToSpace):-
  space_transition(FromSpace, InBetweenSpace),
  space_transition(InBetweenSpace, ToSpace),
  !.
between_space(FromSpace, InBetweenSpace, ToSpace):-
  space_transition(FromSpace, NewFromSpace),
  space_transition(NewToSpace, ToSpace),
  between_space(NewFromSpace, InBetweenSpace, NewToSpace).

%% global_space(?GlobalSpace:space) is semidet.
% The global space.
%
% @param GlobalSpace A space.

global_space(GlobalSpace):-
  rdf_global_id(space:global, GlobalSpace).

initial_state_transtition(InitialTransition):-
  input_space(InputSpace),
  space_transition(InputSpace, InitialTransition).

%% input_space(?InputSpace:space) is semidet.
% The input space.
%
% @param InputSpace A space.

input_space(InputSpace):-
  rdf_global_id(space:input, InputSpace).

%% space(?Space:space) is nondet.
% Spaces.
%
% @param Space A space.

space(Space):-
  rdfs_individual_of(Space, space:space).

%% space_component(?Space:space, ?Component:component) is nondet.
% Pairs of spaces and component clouds.
%
% @param Space A space.
% @param Component A component.

space_component(Space, Component):-
  % A component may have multiple support points, but they must
  % be in the same space.
  once(component_support_point(Component, SupportPoint)),
  space_point(Space, SupportPoint).

%% space_delta(
%%   +FromSpace:space,
%%   -SpaceDelta:integer,
%%   +ToSpace:space
%% ) is semidet.
% Returns the distance between the given spaces in terms of
% space transitions.
%
% @param FromState A state.
% @param SpaceDelta An integer.
% @param ToState A state.

space_delta(State, 0, State):-
  !.
space_delta(FromState, 2, ToState):-
  state_transition(FromState, ToState),
  !.
space_delta(FromSpace, 1, ToSpace):-
  space_transition(FromSpace, ToSpace),
  !.
space_delta(FromSpace, Delta, ToSpace):-
  space_transition(FromSpace, NewFromSpace),
  space_transition(NewToSpace, ToSpace),
  space_delta(NewFromSpace, NewDelta, NewToSpace),
  Delta is NewDelta + 2.

space_expression(Space, Expression):-
  point(Expression, Space, _Point).

%% space_id(?Space:space, ?SpaceID:number) is det.
% Pairs of spaces and their identifiers.
%
% @param Space A space.
% @param SpaceID The numeric identifier of a space.

space_id(Space, SpaceID):-
  var(Space),
  var(SpaceID),
  space_id1(Space, SpaceID).
space_id(Space, SpaceID):-
  space_id1(Space, SpaceID),
  % Spaces and their identifiers have a one-to-one mapping.
  !.

space_id1(Space, SpaceID):-
  rdf_datatype(Space, space:has_id, integer, SpaceID, ccm).

%% space_label(?Space:space, ?SpaceLabel:atom) is det.
% Pair of spaces and their natural language labels.
%
% @param Space A space.
% @param SpaceLabel The atomic natural language label of the space.

space_label(Space, SpaceLabel):-
  nonvar(Space),
  !,
  space_label_(Space, SpaceLabel),
  % A space has exactly one label.
  !.
space_label(Space, SpaceLabel):-
  space_label_(Space, SpaceLabel).

space_label_(Space, SpaceLabel):-
  rdfs_label(Space, SpaceLabel).

%% space_point(?Space:space, ?Point:point) is nondet.
% Pairs of spaces and points.
%
% @param Space A space.
% @param Point A point.

space_point(Space, Point):-
  nonvar(Point),
  !,
  space_point_(Space, Point),
  !.
space_point(Space, Point):-
  space_point_(Space, Point).

space_point_(Space, Point):-
  rdf(Space, space:has_point, Point, ccm).

space_point_cloud(Space, PointCloud):-
  point_cloud(_Expression, Space, _Point, PointCloud).

%% space_next_to_space(+Space1:space, -Space2:Space) is nondet.
% Pairs of directly connected spaces.
%
% @param Space1 A space.
% @param Space2 A space.

space_next_to_space(ToSpace, FromSpace):-
  space_transition(FromSpace, ToSpace).
space_next_to_space(FromSpace, ToSpace):-
  space_transition(FromSpace, ToSpace).

space_to_ccm_label(Space, SpaceCCMLabel):-
  state(Space),
  !,
  state_to_ccm_label(Space, SpaceCCMLabel).
% State transitions must come before space transitions,
% since all state transitions are space transitions.
space_to_ccm_label(Space, SpaceCCMLabel):-
  state_transition(Space),
  !,
  state_transition_to_ccm_label(Space, SpaceCCMLabel).
space_to_ccm_label(Space, SpaceCCMLabel):-
  space_transition(Space),
  !,
  space_transition_to_ccm_label(Space, SpaceCCMLabel).
space_to_ccm_label(Space, g):-
  global_space(Space),
  !.
space_to_ccm_label(Space, i):-
  input_space(Space),
  !.
space_to_ccm_label(Space, SpaceCCMLabel):-
  space_label(Space, SpaceCCMLabel).

space_to_dot_name(Space, SpaceDOTName):-
  state(Space),
  !,
  state_to_dot_name(Space, SpaceDOTName).
space_to_dot_name(Space, SpaceDOTName):-
  state_transition(Space),
  !,
  state_transition_to_dot_name(Space, SpaceDOTName).
space_to_dot_name(Space, SpaceDOTName):-
  space_transition(Space),
  !,
  space_transition_to_dot_name(Space, SpaceDOTName).
space_to_dot_name(Space, SpaceDOTName):-
  space(Space),
  space_id(Space, SpaceID),
  format(atom(SpaceDOTName), 'sp_~w', [SpaceID]).

%% space_to_expressions(
%%   +Space:space,
%%   -Expressions:ord_set(expression)
%% ) is det.
% Returns the expression that are used in the given space.
%
% @param Space A space.
% @param Expressions An ordered set of expressions.

space_to_expressions(Space, Expressions):-
  setoff(
    Expression,
    expression_space(Space, Expression),
    Expressions
  ).

%% space_to_point_clouds(
%%   +Space:space,
%%   -PointClouds:ord_set(point_cloud)
%% ) is det.
% Returns the point clouds for the given space.
%
% @param Space A space.
% @param PointClouds An ordered set of point clouds.

space_to_point_clouds(Space, PointClouds):-
  setoff(
    PointCloud,
    space_point_cloud(Space, PointCloud),
    PointClouds
  ).

%% space_to_points(+Space:space, -Points:ord_set(point)) is det.
% Returns the points in the given space.
%
% @param Space A space.
% @param Points An ordered set of points.

space_to_points(Space, Points):-
  setoff(
    Point,
    space_point(Space, Point),
    Points
  ).

%% spaces(-Spaces:ord_set(space)) is det.
% Returns all spaces.
%
% @param Spaces The ordered set of spaces.

spaces(Spaces):-
  setoff(
    Space,
    space(Space),
    Spaces
  ).



% SPACE TRANSITIONS %

%% space_transition(?SpaceTransition:transition) is nondet.
% Transition spaces.
%
% @param Space A space transition.

space_transition(SpaceTransition):-
  rdfs_individual_of(SpaceTransition, space:space_transition).

%% space_transition(?FromSpace:space, ?ToSpace:space) is nondet.
% Ordered pairs of transitioning spaces.
%
% @param FromSpace A space.
% @param ToSpace A space.

space_transition(FromSpace, ToSpace):-
  nonvar(FromSpace),
  nonvar(ToSpace),
  !,
  space_transition_(FromSpace, ToSpace),
  % Two spaces can have at most one space transition.
  !.
space_transition(FromSpace, ToSpace):-
  space_transition_(FromSpace, ToSpace).

space_transition_(FromSpace, ToSpace):-
  rdfs(FromSpace, space:has_to_space, ToSpace, ccm).

%% space_transition_from_space(
%%   +SpaceTransition:space_transition,
%%   -FromSpace:space
%% ) is det.
% Returns the from space of the given transition.
%
% @param SpaceTransition A space transition.
% @param FromSpace A space.

space_transition_from_space(SpaceTransition, FromSpace):-
  nonvar(SpaceTransition),
  !,
  space_transition_from_space_(SpaceTransition, FromSpace),
  % A space transition has exactly one from space.
  !.
space_transition_from_space(SpaceTransition, FromSpace):-
  space_transition_from_space_(SpaceTransition, FromSpace).

space_transition_from_space_(SpaceTransition, FromSpace):-
  rdf(FromSpace, space:has_to_space, SpaceTransition, ccm).

space_transition_id(SpaceTransition, SpaceTransitionID):-
  space_id(SpaceTransition, SpaceTransitionID).

%% space_transition_label(+SpaceTransition:transition, -Label:atom) is det.
% Returns the natural language label of the given space transition.
%
% @param SpaceTransition The URI of a space transition.
% @param Label The atomic natural language label of a space transition.

space_transition_label(SpaceTransition, Label):-
  space_label(SpaceTransition, Label).

space_transition_to_ccm_label(SpaceTransition, SpaceTransitionCCMLabel):-
  space_transition_from_space(SpaceTransition, FromSpace),
  space_to_ccm_label(FromSpace, FromSpaceCCMLabel),
  space_transition_to_space(SpaceTransition, ToSpace),
  space_to_ccm_label(ToSpace, ToSpaceCCMLabel),
  format(
    atom(SpaceTransitionCCMLabel),
    '~w->~w',
    [FromSpaceCCMLabel, ToSpaceCCMLabel]
  ).

%% space_transition_to_component(
%%   +SpaceTransition:space_transition,
%%   -Component:component
%% ) is nondet.
% Pairs of space transitions and components.
%
% @param SpaceTransition A space transition.
% @param Component A component.

space_transition_to_component(SpaceTransition, Component):-
  space_transition(SpaceTransition),
  space_component(SpaceTransition, Component).

space_transition_to_dot_name(SpaceTransition, SpaceTransitionDOTName):-
  space_transition_id(SpaceTransition, SpaceTransitionID),
  format(atom(SpaceTransitionDOTName), 'spt_~w', [SpaceTransitionID]).

%% space_transition_to_space(
%%   +SpaceTransition:space_transition,
%%   -ToSpace:space
%% ) is det.
% Returns the from space of the given transition.
%
% @param SpaceTransition A space transition.
% @param ToSpace A space.

space_transition_to_space(SpaceTransition, ToSpace):-
  nonvar(SpaceTransition),
  !,
  space_transition_to_space1(SpaceTransition, ToSpace),
  % A space transition has exactly one from space.
  !.
space_transition_to_space(SpaceTransition, ToSpace):-
  space_transition_to_space1(SpaceTransition, ToSpace).

space_transition_to_space1(SpaceTransition, ToSpace):-
  rdf(SpaceTransition, space:has_to_space, ToSpace, ccm).



% STATES %

%% start_state(?StartState:state) is nondet.
% Start state.
% A start state is a state that is directly connected to the input space.
%
% @param StartState A start state.

start_state(StartState):-
  input_space(InputSpace),
  space_transition(InputSpace, InputTransitionSpace),
  space_transition(InputTransitionSpace, StartState).

%% start_states(-StartStates:ord_set(state)) is det.
% Returns the start states.
% A start state is a state that is directly connected to the input space.
%
% @param StartStates An ordered set of states.

start_states(StartStates):-
  setoff(
    StartState,
    start_state(StartState),
    StartStates
  ).

%% state(?Resource:uri) is semidet.
%% state(-State:state) is nondet.
% Returns if the given space is a state.
%
% @param Space The URI of a space.

state(Space):-
  rdfs_individual_of(Space, space:state).

state_name(State, StateName):-
  var(State),
  var(StateName),
  !,
  state_name_(State, StateName).
state_name(State, StateName):-
  state_name_(State, StateName),
  % There is a one-to-one mapping between states and state names.
  !.

state_name_(State, StateName):-
  rdf_datatype(State, space:has_engine_name, integer, StateName, ccm).

state_delta(FromState, StateDelta, ToState):-
  space_delta(FromState, SpaceDelta, ToState),
  StateDelta is SpaceDelta / 2.

%% state_id(?State:state, ?StateID:number) is nondet.
% A state and its numeric identifier.
%
% @param State A state.
% @param StateID A numeric identifier of a state.

state_id(State, StateID):-
  space_id(State, StateID).

%% state_label(+State:state, -StateLabel:atom) is det.
% Returns the natural language label of the given state.
%
% @param State A state.
% @param StateLabel The atom natural language label for a state,

state_label(State, StateLabel):-
  space_label(State, StateLabel).

%% state_point(?State:state, ?Point:point) is nondet.
% Pairs of states and points.
%
% @param State A state.
% @param Point A point.

state_point(State, Point):-
  space_point(State, Point),
  state(State).

%% state_status(?State:state, ?Status:atom) is nondet.
% Pairs of states and their status.
%
% @param State A state
% @param Status The atomic status of the state. Possible values:
%        1. 'closed'
%        2. 'interpreted'
%        3. 'open'
%        4. 'ordered'
%        5. 'terminated'

state_status(State, Status):-
  nonvar(State),
  !,
  state_status_(State, Status),
  !.
state_status(State, Status):-
  state_status_(State, Status).

state_status_(State, Status):-
  rdf_literal(State, space:has_status, Status, ccm).

state_to_ccm_label(State, StateCCMLabel):-
  state_name(State, StateCCMLabel).

%% state_to_component(?State:state, ?Component:component) is nondet.
% Pairs of states and components.
%
% @param State A state.
% @param Component A component.

state_to_component(State, Component):-
  state(State),
  space_component(State, Component).

state_to_dot_name(State, StateDOTName):-
  state_id(State, StateID),
  format(atom(StateDOTName), 'st_~w', [StateID]).

state_to_from_states(State, FromStates):-
  setoff(
    FromState,
    state_transition(FromState, State),
    FromStates
  ).

state_to_to_states(State, ToStates):-
  setoff(
    ToState,
    state_transition(State, ToState),
    ToStates
  ).

%% states(-States:ord_set(state)) is det.
% Returns all states.
%
% @param States An ordered set of states.

states(States):-
  setoff(
    State,
    state(State),
    States
  ).



% STATE TRANSITIONS %

%% state_transition(?StateTransition:space) is nondet.
% State transitions.
%
% @param Space A space.

state_transition(StateTransition):-
  rdfs_individual_of(StateTransition, space:state_transition).

%% state_transition(?FromState:state, ?ToState:state) is nondet.
% Succeeds if there is a state transition from the former to the latter state.
%
% @param FromState A state.
% @param ToState A state.

state_transition(FromState, ToState):-
  rdfs(FromState, space:has_to_state, ToState, ccm).

state_transition(FromState, ToState, StateTransition):-
  nonvar(StateTransition),
  !,
  state_transition1_(FromState, ToState, StateTransition),
  % A state transition has exactly one from state and exactly one to state.
  !.
state_transition(FromState, ToState, StateTransition):-
  nonvar(FromState),
  nonvar(ToState),
  !,
  state_transition1_(FromState, ToState, StateTransition),
  % A from state and a to state determine a state transition, if any.
  !.
state_transition(FromState, ToState, StateTransition):-
  state_transition(StateTransition),
  state_transition1_(FromState, ToState, StateTransition).

state_transition1_(FromState, ToState, StateTransition):-
  rdf(FromState, space:has_to_space, StateTransition, ccm),
  rdf(StateTransition, space:has_to_space, ToState, ccm).

state_transition_by_names(FromStateName, ToStateName, StateTransition):-
  state_name(FromState, FromStateName),
  state_name(ToState, ToStateName),
  state_transition(FromState, StateTransition),
  state_transition(StateTransition, ToState).

%% state_transition_from_state(
%%   +StateTransition:state_transition,
%%   -FromState:state
%% ) is det.
% Returns the from state of the given transition.
%
% @param StateTransition A state transition.
% @param FromState A state.

state_transition_from_state(StateTransition, FromState):-
  nonvar(StateTransition),
  !,
  state_transition_from_state_(StateTransition, FromState),
  % A state transition has exactly one from state.
  !.
state_transition_from_state(StateTransition, FromState):-
  state_transition_from_state_(StateTransition, FromState).

state_transition_from_state_(StateTransition, FromState):-
  rdf(FromState, space:has_to_space, StateTransition, ccm).

state_transition_id(StateTransition, StateTransitionID):-
  space_id(StateTransition, StateTransitionID).

%% state_transition_label(
%%   +StateTransition:transition,
%%   -StateTransitionLabel:atom
%% ) is det.
% Returns the natural language label of the given state transition.
%
% @param StateTransition A state transition.
% @param StateTransitionLabel The atomic natural language label of
%        a state transition.

state_transition_label(StateTransition, StateTransitionLabel):-
  space_label(StateTransition, StateTransitionLabel).

state_transition_to_ccm_label(StateTransition, StateTransitionCCMLabel):-
  state_transition_from_state(StateTransition, FromState),
  state_to_ccm_label(FromState, FromStateCCMLabel),
  state_transition_to_state(StateTransition, ToState),
  state_to_ccm_label(ToState, ToStateCCMLabel),
  format(
    atom(StateTransitionCCMLabel),
    '~w->~w',
    [FromStateCCMLabel, ToStateCCMLabel]
  ).

state_transition_to_dot_name(StateTransition, StateTransitionDOTName):-
  state_transition_id(StateTransition, StateTransitionID),
  format(atom(StateTransitionDOTName), 'stt_~w', [StateTransitionID]).

%% state_transition_to_state(
%%   +StateTransition:state_transition,
%%   -ToState:state
%% ) is det.
% Returns the to state of the given state transition.
%
% @param StateTransition A state transition.
% @param ToState A state.

state_transition_to_state(StateTransition, ToState):-
  nonvar(StateTransition),
  !,
  state_transition_to_state_(StateTransition, ToState),
  % A state transition has exactly one to state.
  !.
state_transition_to_state(StateTransition, ToState):-
  state_transition_to_state_(StateTransition, ToState).

state_transition_to_state_(StateTransition, ToState):-
  rdf(StateTransition, space:has_to_space, ToState, ccm).

state_transitions(StateTransitions):-
  setoff(
    StateTransition,
    state_transition(StateTransition),
    StateTransitions
  ).



% GENERIC %

%% known_or_unknown(
%%   +Diagnosis:diagnosis,
%%   +Environment:environment,
%%   +Point:point
%% ) is semidet.
% Succeeds if the given point is believed by the given diagnosis under
% the given assumptions.
%
% @param Diagnosis A diagnosis.
% @param Environment An environment.
% @param Point A point.

known_or_unknown(Diagnosis, Environment, Point):-
  learner(Diagnosis, Learner),
  does_consider_point(Learner, Point),
  diagnosis_atms(Diagnosis, ATMS),
  node(ATMS, Point, Node),
  is_in_node(Node, Environment).
