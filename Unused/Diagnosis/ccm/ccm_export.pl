:- module(
  ccm_export,
  [
% EXPORTS
    write_all/0,
    write_build_to_dot/0,
    write_causal_graph_to_dot/1, % +Clusters:boolean
    write_ccm_to_dot/0,
    write_ccm_to_dot/1, % +File:file
    write_ccm_to_rdf/0,
    write_component_definitions_to_dot/0,
    write_entity_hierarchy_to_dot/0,
    write_expression_definitions_to_dot/0,
    write_parse_tree/1, % +Node:compound
    write_parse_trees/1, % +Nodes:list(compound)
    write_quantity_spaces_to_dot/0,
    write_relation_definitions_to_dot/0,
    write_spaces_to_dot/0,
    write_to_dot/4, % +Predicate:atom,
                    % +File:file,
                    % +Alias:atom,
                    % +GraphLabel:atom,
    write_to_dot/5, % +Predicate:atom,
                    % +File:file,
                    % +Alias:atom,
                    % +GraphLabel:atom,
                    % +Argument

% COLORS
    component_cloud_color/2, % +ComponentCloud:component_cloud
                             % -Color:atom
    point_color/2, % +Point:point
                   % -Color:atom
    point_cloud_color/2 % +PointCloud:point_cloud
                        % -Color:atom
  ]
).

/** <module> The Component Connection Model export methods.

This module contains the predicates that export the CCM in various ways.
Needless to say, none of these predicates can ever alter the CCM.

Some predicates use the datatype =|triple(Subject, Predicate, Object)|=.

@author Wouter Beek
@version 2011/08-2012/05
*/

:- use_module(atms(atms_api)).
:- use_module(
  ccm(ccm_api),
  except([disabled_component_cloud/1, disabled_component_definition/1])
).
:- use_module(ccm(ccm_label)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(atom_ext)).
:- use_module(generic(file_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qr(qr_api)).
:- use_module(qr(causal_explanation)).
:- use_module(rdfs(rdfs_read)).

:- dynamic disabled_component_definition/1.

:- meta_predicate write_to_dot(0,+,+,+).
:- meta_predicate write_to_dot(1,+,+,+,+).



% DISALED COMPONENT CLOUDS %

%% disabled_component_cloud(?ComponentCloud:component_cloud) is nondet.
% Succeeds if the given component cloud is disabled for scoping.
%
% @param ComponentCloud A component cloud.

disabled_component_cloud(ComponentCloud):-
  disabled_component_definition(ExclusionPredicate),
  Call =.. [ExclusionPredicate, ComponentCloud],
  call(Call).

%% disabled_component_definition(
%%   ?ComponentDefinition:component_definition
%% ) is nondet.
% Component types that are excluded.
%
% @param ComponentDefinition A component definition.

%disabled_component_definition(disabled_component_cloud).
%disabled_component_definition(retrieval_component_cloud).
%disabled_component_definition(scenario_value_component_cloud).
%disabled_component_definition(subsumed_component_cloud).



% FILES %

build_file(build).
causal_graph_file(causal_graph).
ccm_file(ccm).
component_definitions_file(component_definitions).
class_definitions_file(class_definitions).
entity_hierarchy_file(entity_hierarchy).
expression_definitions_file(expression_definitions).
quantity_spaces_file(quantity_spaces).
relation_definitions_file(relation_definitions).
spaces_file(spaces).



% GRAPHS %

export_graph(export).



% GENERIC %

%% write_parse_tree(Node:compound) is det.
% Exports the parse tree that starts at the given node in the
% GraphViz DOT format.
%
% @param Node A compound term representing a nested parse tree.

write_parse_tree(Node):-
  flag(parse_node_id, NodeID, NodeID + 1),
  write_parse_tree1(NodeID, Node),
  flag(parse_node_id, _OldNodeID, 0).

write_parse_tree1(ParentID, Node):-
  Node =.. [ParentNode | ChildNodes],
  node_to_name(ParentNode, ParentName),
  format(
    parse_tree,
    'n_~w [color="black", fontsize="11", label="~w", shape="box"];',
    [ParentID, ParentName]
  ),
  forall(
    member(ChildNode, ChildNodes),
    (
      flag(parse_node_id, ChildID, ChildID + 1),
      format(
        parse_tree,
        '  n_~w -> n_~w;\n',
        [ParentID, ChildID]
      ),
      write_parse_tree1(ChildID, ChildNode)
    )
  ).

%% write_parse_trees(Nodes:list(compound)) is det.
% Exports all parse trees that start at some given node.
%
% @param Nodes A list of compound terms representing nested parse trees.

write_parse_trees(Nodes):-
  forall(
    member(Node, Nodes),
    (
      flag(parse_node_id, NodeID, NodeID + 1),
      write_parse_tree1(NodeID, Node)
    )
  ).

node_to_name(adjective, 'Adj').
node_to_name(determiner, 'Det').
node_to_name(intransitive_verb, 'IV').
node_to_name(noun, 'N').
node_to_name(noun_phrase, 'NP').
node_to_name(proper_noun, 'PN').
node_to_name(sentence, 'S').
node_to_name(transitive_verb, 'TV').
node_to_name(verb, 'V').
node_to_name(verb_phrase, 'VP').
node_to_name(Name, Name).

%% write_to_dot(
%%   +Predicate:atom,
%%   +File:file,
%%   +Alias:atom,
%%   +GraphLabel:atom
%% ) is det.
% Writes the results of the given DOT write predicate to the given file.
%
% @param Predicate The atomic name of a predicate.
% @param File The atomic name of a file.
% @param Alias The atomic name of the used alias. This is also the name
%        of the digraph.
% @param GraphLabel The atomic label that is to be displayed at the bottom
%        of the graph.
% @param Arguments A list of argument flags that affect the DOT export.
%        These arguments are passed to =Predicate= in the order in which
%        they appear in the list.

write_to_dot(Predicate, File, Alias, GraphLabel):-
  flag(File, FileID, FileID + 1),
  format(atom(Name), '~w_~w', [File, FileID]),
  create_file(debug(ccm), Name, graphviz, AbsoluteFile),
  open(
    AbsoluteFile,
    write,
    _Stream,
    [alias(Alias), close_on_abort(true), type(text)]
  ),
  format(Alias, 'digraph ~w {\n', [Alias]),
  call(Predicate),
  write_vertical_space(Alias),
  format(Alias, '  fontsize="11"\n', []),
  format(Alias, '  charset="UTF-8"\n', []),
  format(Alias, '  label="~w"\n', [GraphLabel]),
  format(Alias, '  overlap=false\n', []),
  format(Alias, '}\n', []),
  close(Alias).

%% write_to_dot(
%%   +Predicate:atom,
%%   +File:file,
%%   +Alias:atom,
%%   +GraphLabel:atom,
%%   +Argument
%% ) is det.
%
% @see write_to_dot/4

write_to_dot(Predicate, File, Alias, GraphLabel, Argument):-
  flag(File, FileID, FileID + 1),
  format(atom(Name), '~w_~w', [File, FileID]),
  create_file(debug(ccm), Name, graphviz, AbsoluteFile),
  open(
    AbsoluteFile,
    write,
    _Stream,
    [alias(Alias), close_on_abort(true), type(text)]
  ),
  format(Alias, 'digraph ~w {\n', [Alias]),
  call(Predicate, Argument),
  write_vertical_space(Alias),
  format(Alias, '  fontsize="11"\n', []),
  format(Alias, '  charset="UTF-8"\n', []),
  format(Alias, '  label="~w"\n', [GraphLabel]),
  format(Alias, '  overlap=false\n', []),
  format(Alias, '}\n', []),
  close(Alias).



% EXPORT PREDICATES %

%% write_all is det.
% Writes all CCM information to the respective export files.

write_all:-
  % We write the RDF representation before writing any of the DOT
  % representations, since the RDF representation is unlikely to contain
  % bugs, and is thus a usefull resource for debugging the touchy DOT
  % export predicates.
  write_ccm_to_rdf,
  % Show the temporal clusers by default.
  write_causal_graph_to_dot(fail),
  write_ccm_to_dot(ccm),
  write_component_definitions_to_dot,
  write_entity_hierarchy_to_dot,
  write_expression_definitions_to_dot,
  write_quantity_spaces_to_dot,
  write_relation_definitions_to_dot,
  write_spaces_to_dot.

write_build_to_dot:-
  build_file(File),
  write_to_dot(
    build_dot,
    File,
    build,
    'Build Model'
  ).

%% write_ccm_to_rdf is det.
% Writes the current CCM representation to an RDF file using standard XML
% serialization.

write_ccm_to_rdf:-
  ccm_file(File),
  rdf_save_xml(ccm, xml, File).

write_causal_graph_to_dot(Clusters):-
  causal_graph_file(File),
  write_to_dot(
    causal_graph,
    File,
    causal_graph,
    'The Causal Graph',
    [Clusters]
  ).

write_ccm_to_dot:-
  write_ccm_to_dot(ccm).

write_ccm_to_dot(File):-
  findall(
    ComponentExclusionPredicate,
    disabled_component_definition(ComponentExclusionPredicate),
    ExclusionPredicates
  ),
  write_to_dot(
    ccm_dot,
    File,
    ccm,
    'Component Connection Model',
    ExclusionPredicates
  ).

write_component_definitions_to_dot:-
  component_definitions_file(File),
  write_to_dot(
    component_definition_dot,
    File,
    component_definition,
    'Component Definitions'
  ).

write_entity_hierarchy_to_dot:-
  entity_hierarchy_file(File),
  write_to_dot(
    entity_hierarchy_dot,
    File,
    entity_hierarchy,
    'Entity Hierarchy'
  ).

write_expression_definitions_to_dot:-
  expression_definitions_file(File),
  write_to_dot(
    expression_definitions_dot,
    File,
    expression_definitions,
    'Expression Definitions'
  ).

write_relation_definitions_to_dot:-
  relation_definitions_file(File),
  write_to_dot(
    relation_definitions_dot,
    File,
    relation_definitions,
    'Relation Definitions'
  ).

write_spaces_to_dot:-
  spaces_file(File),
  write_to_dot(
    spaces_dot,
    File,
    spaces,
    'Spaces and Space Transitions'
  ).

write_quantity_spaces_to_dot:-
  quantity_spaces_file(File),
  write_to_dot(
    quantity_spaces_dot,
    File,
    quantity_spaces,
    'Quantity Spaces'
  ).

write_vertical_space(Stream):-
  format(Stream, '\n', []).



% EXPORT BUILD MODEL %

%% build_dot is det.
% Generate the GraphViz output for the build model.

build_dot:-
  % Writes nodes.
  write_attribute_nodes(build),
  write_attribute_value_nodes(build),
  write_entity_definition_nodes(build),
  write_entity_instance_nodes(build),
  write_quantity_nodes(build),
  write_quantity_space_nodes(build),
  write_quantity_value_nodes(build),
  % Write relations.
  write_attribute_relations(build),
  write_attribute_value_relations(build),
  write_configuration_relations(build),
  write_entity_attribute_relations(build),
  write_entity_definition_relations(build),
  write_entity_definition_to_entity_relations(build),
  write_entity_to_quantity_relations(build),
  write_quantity_proportionality_relations(build),
  write_quantity_space_to_quantity_value_relations(build),
  write_quantity_to_quantity_space_relations(build),
  write_quantity_value_relations(build).



% EXPORT CAUSAL GRAPH %

%% causal_graph(+Clusters:boolean) is det.
% Exports the Causal Graph in the DOT format.
%
% @param Either =true= or =fail=, deciding whether to draw the nodes in
%        temporal clusters or not.

causal_graph(Clusters):-
  % Write nodes.
  write_attribute_value_nodes(causal_graph),
  %%%%write_configuration_definition_nodes(causal_graph),
  %%%%write_configuration_definition_relations(causal_graph),
  write_entity_definition_nodes(causal_graph),
  write_entity_instance_nodes(causal_graph),
  write_quantity_definition_nodes(causal_graph),
  write_quantity_nodes(causal_graph),
  write_quantity_space_definition_nodes(causal_graph),
  write_quantity_space_nodes(causal_graph),
  write_quantity_value_definition_nodes(causal_graph),
  write_quantity_value_nodes(causal_graph),

  % Vertical space.
  write_vertical_space(causal_graph),

  % Write the relations between nodes.
  write_configuration_relations(causal_graph),
  write_entity_attribute_relations(causal_graph),
  write_entity_definition_relations(causal_graph),
  write_entity_definition_to_entity_relations(causal_graph),
  write_entity_to_quantity_relations(causal_graph),
  %write_expression_to_argument_relations(causal_graph),
  write_quantity_definition_quantity_space_definition_relations(causal_graph),
  write_quantity_definition_relations(causal_graph),
  write_quantity_definition_to_quantity_relations(causal_graph),
  write_quantity_to_quantity_space_relations(causal_graph),
  write_quantity_space_definition_relations(causal_graph),
  write_quantity_space_definition_to_quantity_space_relations(causal_graph),
  write_quantity_space_definition_to_quantity_value_definition_relations(
    causal_graph
  ),
  write_quantity_value_definition_relations(causal_graph),
  write_quantity_space_to_quantity_value_relations(causal_graph),
  write_quantity_value_relations(causal_graph),

  % Vertical space.
  write_vertical_space(causal_graph),

  write_definitions_cluster(causal_graph, Clusters),
  write_instances_cluster(causal_graph, Clusters).



% WRITE NODES %

write_attribute_nodes(Stream):-
  attributes(Attributes),
  forall(
    member(Attribute, Attributes),
    (
      attribute_to_dot_name(Attribute, AttributeDOTName),
      attribute_label(Attribute, AttributeLabel),
      format(
        Stream,
        '  ~w [color="orange", fontsize="11", label="~w", shape="ellipse"];\n',
        [AttributeDOTName, AttributeLabel]
      )
    )
  ).

write_attribute_value_nodes(Stream):-
  attribute_values(AttributeValues),
  forall(
    member(AttributeValue, AttributeValues),
    (
      attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName),
      attribute_value_label(AttributeValue, AttributeValueLabel),
      format(
        Stream,
        '  ~w [color="orange", fontsize="11", label="~w", shape="ellipse"];\n',
        [AttributeValueDOTName, AttributeValueLabel]
      )
    )
  ).

write_component_nodes(Stream):-
  components(Components),
  forall(
    member(Component, Components),
    (
      component_to_dot_name(Component, ComponentDOTName),
      component_id(Component, ComponentID),
      component_label(Component, ComponentLabel),!,
      format(atom(ComponentLabel1), '~w: ~w', [ComponentID, ComponentLabel]),
      format(
        Stream,
        '  ~w [color="orange", fontsize="11", label="~w", shape="box"];\n',
        [ComponentDOTName, ComponentLabel1]
      )
    )
  ).

write_component_definition_nodes(Stream):-
  component_definitions(ComponentDefinitions),
  forall(
    member(ComponentDefinition, ComponentDefinitions),
    (
      component_definition_to_dot_name(
        ComponentDefinition,
        ComponentDefinitionDOTName
      ),
      component_definition_label(
        ComponentDefinition,
        ComponentDefinitionLabel
      ),
      format(
        Stream,
        '  ~w [color="darkorange", fontsize="11", label="~w", shape="box"];\n',
        [ComponentDefinitionDOTName, ComponentDefinitionLabel]
      )
    )
  ).

write_configuration_definition_nodes(Stream):-
  configuration_definitions(ConfigurationDefinitions),
  forall(
    member(ConfigurationDefinition, ConfigurationDefinitions),
    (
      configuration_definition_to_dot_name(
        ConfigurationDefinition,
        ConfigurationDefinitionDOTName
      ),
      configuration_definition_label(
        ConfigurationDefinition,
        ConfigurationDefinitionLabel
      ),
      format(
        Stream,
        '  ~w [color="grey", fontsize="11", label="~w", shape="box"];\n',
        [ConfigurationDefinitionDOTName, ConfigurationDefinitionLabel]
      )
    )
  ).

write_entity_definition_nodes(Stream):-
  entity_definitions(EntityDefinitions),
  forall(
    member(EntityDefinition, EntityDefinitions),
    (
      entity_definition_to_dot_name(EntityDefinition, EntityDefinitionDOTName),
      entity_definition_label(EntityDefinition, EntityDefinitionLabel),
      format(
        Stream,
        '  ~w [color="darkgreen", fontsize="11", label="~w", shape="box"];\n',
        [EntityDefinitionDOTName, EntityDefinitionLabel]
      )
    )
  ).

write_entity_instance_nodes(Stream):-
  entities(Entities),
  forall(
    member(Entity, Entities),
    (
      entity_to_dot_name(Entity, EntityDOTName),
      entity_label(Entity, EntityLabel),
      format(
        Stream,
        '  ~w [color="green", fontsize="11", label="~w", shape="ellipse"];\n',
        [EntityDOTName, EntityLabel]
      )
    )
  ).

write_explanation_nodes(Stream):-
  explanations(Explanations),
  forall(
    member(Explanation, Explanations),
    (
      explanation_label(Explanation, ExplanationLabel),
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      format(
        Stream,
        '  ~w [color="black", fontsize="11", label="~w", shape="box"];\n',
        [ExplanationDOTName, ExplanationLabel]
      )
    )
  ).

write_explanation_offset_nodes(Stream):-
  explanation_offsets(ExplanationOffsets),
  forall(
    member(ExplanationOffset, ExplanationOffsets),
    (
      explanation_offset_to_dot_name(ExplanationOffset, ExplanationOffsetDOTName),
      explanation_offset_label(ExplanationOffset, ExplanationOffsetLabel),
      format(
        Stream,
        '  ~w [color="black", fontsize="11", label="~w", shape="box"];\n',
        [ExplanationOffsetDOTName, ExplanationOffsetLabel]
      )
    )
  ).

write_expression_definition_nodes(Stream):-
  expression_definition(ExpressionDefinition),
  write_expression_definition_nodes(Stream, ExpressionDefinition).

write_expression_definition_nodes(Stream, Filter):-
  forall(
    rdfs_subclass_of(ExpressionDefinition, Filter),
    (
      expression_definition_to_dot_name(
        ExpressionDefinition,
        ExpressionDefinitionDOTName
      ),
      expression_definition_name(
        ExpressionDefinition,
        ExpressionDefinitionName
      ),
      format(
        Stream,
        '  ~w [color="black", fontsize="11", label="~w", shape="box"];\n',
        [ExpressionDefinitionDOTName, ExpressionDefinitionName]
      )
    )
  ).

write_expression_nodes(Stream):-
  expressions(Expressions),
  forall(
    member(Expression, Expressions),
    (
      expression_to_dot_name(Expression, ExpressionDOTName),
      expression_id(Expression, ExpressionID),
      expression_to_ccm_label(Expression, ExpressionLabel),
      format(
        atom(ExpressionLabel_),
        '~w: ~w',
        [ExpressionID, ExpressionLabel]
      ),
      format(
        Stream,
        '  ~w [color="green", fontsize="11", label="~w", shape="ellipse"];\n',
        [ExpressionDOTName, ExpressionLabel_]
      )
    )
  ).

write_point_nodes(Stream):-
  points(Points),
  forall(
    member(Point, Points),
    (
      point_to_label(Point, PointLabel1),
      format(atom(PointLabel), '~w', [PointLabel1]),
      point_to_dot_name(Point, PointDOTName),
      format(
        Stream,
        '  ~w [color="red", fontsize="11", label="~w", shape="box"];\n',
        [PointDOTName, PointLabel]
      )
    )
  ).

write_point_cloud_nodes(Stream):-
  point_clouds(PointClouds),
  forall(
    member(PointCloud, PointClouds),
    (
      point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
      point_cloud_label(PointCloud, PointCloudLabel1),
      point_cloud_to_points(PointCloud, Points),
      maplist(point_to_label, Points, PointLabels),
      atomic_list_concat(PointLabels, '\\n', PointsLabel),
      format(
        atom(PointCloudLabel),
        '~w:\\n~w',
        [PointCloudLabel1, PointsLabel]
      ),
      format(
        Stream,
        '  ~w [color="red", fontsize="11", label="~w", shape="box"];\n',
        [PointCloudDOTName, PointCloudLabel]
      )
    )
  ).

write_quantity_definition_nodes(Stream):-
  quantity_definitions(QuantityDefinitions),
  forall(
    member(QuantityDefinition, QuantityDefinitions),
    (
      quantity_definition_to_dot_name(
        QuantityDefinition,
        QuantityDefinitionDOTName
      ),
      quantity_definition_label(QuantityDefinition, QuantityDefinitionLabel),
      format(
        Stream,
        '  ~w [color="purple4", fontsize="11", label="~w", shape="box"];\n',
        [QuantityDefinitionDOTName, QuantityDefinitionLabel]
      )
    )
  ).

write_quantity_nodes(Stream):-
  quantities(Quantities),
  forall(
    member(Quantity, Quantities),
    (
      quantity_to_dot_name(Quantity, QuantityDOTName),
      quantity_id(Quantity, QuantityID),
      quantity_name(Quantity, QuantityName),
      format(atom(QuantityLabel), '~w: ~w', [QuantityID, QuantityName]),
      format(
        Stream,
        '  ~w [color="purple", fontsize="11", label="~w", shape="ellipse"];\n',
        [QuantityDOTName, QuantityLabel]
      )
    )
  ).

write_quantity_space_definition_nodes(Stream):-
  quantity_space_definitions(QuantitySpaceDefinitions),
  forall(
    member(QuantitySpaceDefinition, QuantitySpaceDefinitions),
    (
      quantity_space_definition_to_dot_name(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionDOTName
      ),
      quantity_space_definition_label(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionLabel
      ),
      format(
        Stream,
        '  ~w [color="purple4", fontsize="11", label="~w", shape="box"];\n',
        [QuantitySpaceDefinitionDOTName, QuantitySpaceDefinitionLabel]
      )
    )
  ).

write_quantity_space_nodes(Stream):-
  quantity_spaces(QuantitySpaces),
  forall(
    member(QuantitySpace, QuantitySpaces),
    (
      quantity_space_to_dot_name(QuantitySpace, QuantitySpaceDOTName),
      quantity_space_id(QuantitySpace, QuantitySpaceID),
      quantity_space_label(QuantitySpace, QuantitySpaceLabel),
      format(
        Stream,
        '  ~w [color="purple", fontsize="11", label="~w:~w", shape="ellipse"];\n',
        [QuantitySpaceDOTName, QuantitySpaceID, QuantitySpaceLabel]
      )
    )
  ).

write_quantity_value_definition_nodes(Stream):-
  quantity_value_definitions(QuantityValueDefinitions),
  forall(
    member(QuantityValueDefinition, QuantityValueDefinitions),
    (
      quantity_value_definition_to_dot_name(
        QuantityValueDefinition,
        QuantityValueDefinitionDOTName
      ),
      quantity_value_definition_label(
        QuantityValueDefinition,
        QuantityValueDefinitionLabel
      ),!,
      (
        quantity_space_definition_quantity_value_definition(
          QuantitySpaceDefinition,
          QuantityValueDefinition
        )
      ->
        quantity_space_definition_label(
          QuantitySpaceDefinition,
          QuantitySpaceDefinitionLabel
        ),!,
        format(
          Stream,
          '  ~w [color="purple4", fontsize="11", label="~w (~w)", shape="box"];\n',
          [
            QuantityValueDefinitionDOTName,
            QuantityValueDefinitionLabel,
            QuantitySpaceDefinitionLabel
          ]
        )
      ;
        format(
          Stream,
          '  ~w [color="purple4", fontsize="11", label="~w", shape="box"];\n',
          [QuantityValueDefinitionDOTName, QuantityValueDefinitionLabel]
        )
      )
    )
  ).

write_quantity_value_nodes(Stream):-
  quantity_values(QuantityValues),
  forall(
    member(QuantityValue, QuantityValues),
    (
      quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName),
      quantity_value_id(QuantityValue, QuantityValueID),
      quantity_value_label(QuantityValue, QuantityValueLabel),!,
      format(
        atom(QuantityValueLabel1),
        '~w: ~w',
        [QuantityValueID, QuantityValueLabel]
      ),
      format(
        Stream,
        '  ~w [color="purple", fontsize="11", label="~w", shape="ellipse"];\n',
        [QuantityValueDOTName, QuantityValueLabel1]
      )
    )
  ).

write_request_nodes(Stream):-
  requests(Requests),
  forall(
    member(Request, Requests),
    (
      request_to_dot_name(Request, RequestDOTName),
      request_label(Request, RequestLabel),
      format(
        Stream,
        '  ~w [color="black", fontsize="11", label="~w", shape="box"];\n',
        [RequestDOTName, RequestLabel]
      )
    )
  ).

write_space_nodes(Stream):-
  forall(
    space(Space),
    (
      space_to_dot_name(Space, SpaceDOTName),
      space_id(Space, SpaceID),
      space_to_ccm_label(Space, SpaceCCMLabel),
      format(atom(SpaceLabel), '~w: ~w', [SpaceID, SpaceCCMLabel]),
      format(
        Stream,
        '  ~w [color="black", fontsize="11", label="~w", shape="circle"];\n',
        [SpaceDOTName, SpaceLabel]
      )
    )
  ).



% WRITE RELATIONS %

write_attribute_relations(Stream):-
  forall(
    entity_attribute_value(Entity, AttributeValue),
    (
      entity_to_dot_name(Entity, EntityDOTName),
      attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [EntityDOTName, AttributeValueDOTName]
      )
    )
  ).

write_attribute_value_relations(Stream):-
  forall(
    rdf(Attribute, attribute:has_possible_value, AttributeValue, ccm),
    (
      attribute_to_dot_name(Attribute, AttributeDOTName),
      attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="orange"];\n',
        [AttributeDOTName, AttributeValueDOTName]
      )
    )
  ).

write_component_definition_relations(Stream):-
  component_definitions(ComponentDefinitions),
  findall(
    ChildComponentDefintion/ParentComponentDefinition,
    (
      rdf(
        ChildComponentDefintion,
        rdfs:subClassOf,
        ParentComponentDefinition,
        ccm
      ),
      % Make sure we do not get too high in the component
      % definition hierarchy.
      member(ParentComponentDefinition, ComponentDefinitions)
    ),
    ComponentPairs
  ),
  forall(
    member(
      ChildComponentDefintion/ParentComponentDefinition,
      ComponentPairs
    ),
    (
      component_definition_to_dot_name(ChildComponentDefintion, CCDDOTName),
      component_definition_to_dot_name(ParentComponentDefinition, PCDDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="darkorange"];\n',
        [CCDDOTName, PCDDOTName]
      )
    )
  ).

write_component_to_point_cloud_relations(Stream):-
  setoff(
    [PointCloud, InputComponent],
    component_output_point_cloud(InputComponent, PointCloud),
    PointCloudInputComponentPairs
  ),
  forall(
    member([PointCloud, InputComponent], PointCloudInputComponentPairs),
    (
      component_to_dot_name(InputComponent, InputComponentDOTName),
      point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [InputComponentDOTName, PointCloudDOTName]
      )
    )
  ).

write_configuration_definition_relations(Stream):-
  forall(
    configuration_direct_parent_child(
      ParentConfigurationDefinition,
      ChildConfigurationDefinition
    ),
    (
      configuration_definition_to_dot_name(
        ParentConfigurationDefinition,
        ParentConfigurationDefinitionDOTName
      ),
      configuration_definition_to_dot_name(
        ChildConfigurationDefinition,
        ChildConfigurationDefinitionDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="grey"];\n',
        [
          ParentConfigurationDefinitionDOTName,
          ChildConfigurationDefinitionDOTName
        ]
      )
    )
  ).

write_configuration_relations(Stream):-
  forall(
    configuration(FromEntity, Configuration, ToEntity),
    (
      entity_to_dot_name(FromEntity, FromEntityDOTName),
      entity_to_dot_name(ToEntity, ToEntityDOTName),
      configuration_definition_label(Configuration, ConfigurationLabel),
      format(
        Stream,
        '  ~w -> ~w [color="black", fontsize="11", label="~w"];\n',
        [FromEntityDOTName, ToEntityDOTName, ConfigurationLabel]
      )
    )
  ).

write_entity_attribute_relations(Stream):-
  forall(
    (
      attribute(Attribute),
      rdf(Entity, Attribute, AttributeValue)
    ),
    (
      entity_to_dot_name(Entity, EntityDOTName),
      attribute_label(Attribute, AttributeLabel),
      attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black", fontsize="11", label="~w"];\n',
        [EntityDOTName, AttributeValueDOTName, AttributeLabel]
      )
    )
  ).

write_entity_definition_relations(Stream):-
  forall(
    entity_direct_parent_child(ParentEntityDefinition, ChildEntityDefinition),
    (
      entity_definition_to_dot_name(
        ParentEntityDefinition,
        ParentEntityDefinitionDOTName
      ),
      entity_definition_to_dot_name(
        ChildEntityDefinition,
        ChildEntityDefinitionDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="darkgreen"];\n',
        [ParentEntityDefinitionDOTName, ChildEntityDefinitionDOTName]
      )
    )
  ).

write_entity_definition_to_entity_relations(Stream):-
  forall(
    (
      qr_api:entity(Entity),
      rdf(Entity, rdf:type, EntityDefinition)
    ),
    (
      entity_definition_to_dot_name(
        EntityDefinition,
        EntityDefinitionDOTName
      ),
      entity_to_dot_name(Entity, EntityDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="darkgreen"];\n',
        [EntityDefinitionDOTName, EntityDOTName]
      )
    )
  ).

write_entity_to_quantity_relations(Stream):-
  forall(
    entity_quantity(Entity, Quantity),
    (
      entity_to_dot_name(Entity, EntityDOTName),
      quantity_to_dot_name(Quantity, QuantityDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [EntityDOTName, QuantityDOTName]
      )
    )
  ).

write_explanation_offset_to_explanation_relations(Stream):-
  forall(
    explanation_offset_explanation(ExplanationOffset, Explanation),
    (
      request_to_dot_name(ExplanationOffset, ExplanationOffsetDOTName),
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [ExplanationOffsetDOTName, ExplanationDOTName]
      )
    )
  ).

write_explanation_to_component_relations(Stream):-
  forall(
    explanation_component(Explanation, Component),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      component_to_dot_name(Component, ComponentDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [ExplanationDOTName, ComponentDOTName]
      )
    )
  ).

write_explanation_to_point_relations(Stream):-
  forall(
    explanation_point(Explanation, Point),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      point_to_dot_name(Point, PointDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [ExplanationDOTName, PointDOTName]
      )
    )
  ).

write_expression_definition_relations(Stream):-
  expression_definition(ExpressionDefinition),
  write_expression_definition_relations(Stream, ExpressionDefinition).

write_expression_definition_relations(Stream, Filter):-
  setoff(
    ExpressionDefinition1/ExpressionDefinition2,
    (
      rdf(ExpressionDefinition1, rdfs:subClassOf, ExpressionDefinition2, ccm),
      rdfs_subclass_of(ExpressionDefinition2, Filter)
    ),
    ExpressionDefinitionPairs
  ),
  forall(
    member(
      ExpressionDefinition1/ExpressionDefinition2,
      ExpressionDefinitionPairs
    ),
    (
      expression_definition_to_dot_name(
        ExpressionDefinition1,
        ExpressionDefinition1DOTName
      ),
      expression_definition_to_dot_name(
        ExpressionDefinition2,
        ExpressionDefinition2DOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [ExpressionDefinition1DOTName, ExpressionDefinition2DOTName]
      )
    )
  ).

write_expression_to_argument_relations(Stream):-
  expressions(Expressions),
  forall(
    member(Expression, Expressions),
    (
      expression_to_dot_name(Expression, ExpressionDOTName),
      expression_from_argument(Expression, FromArgument),
      rdfs_label(FromArgument, FromArgumentLabel),
      spaces_to_underscores(FromArgumentLabel, FromArgumentDOTName),
      expression_to_argument(Expression, ToArgument),
      rdfs_label(ToArgument, ToArgumentLabel),
      spaces_to_underscores(ToArgumentLabel, ToArgumentDOTName),
      format(
        Stream,
        '  ~w -> ~w;\n',
        [ExpressionDOTName, FromArgumentDOTName]
      ),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [ExpressionDOTName, ToArgumentDOTName]
      )
    )
  ).

write_point_cloud_to_component_relations(Stream):-
  setoff(
    [PointCloud, OutComponent],
    point_cloud_output_component(PointCloud, OutComponent),
    PointCloudOutComponentPairs
  ),
  forall(
    member([PointCloud, OutComponent], PointCloudOutComponentPairs),
    (
      point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
      component_to_dot_name(OutComponent, OutComponentDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [PointCloudDOTName, OutComponentDOTName]
      )
    )
  ).

write_point_cloud_to_point_relations(Stream):-
  forall(
    point_cloud(Point, PointCloud),
    (
      point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
      point_to_dot_name(Point, PointDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [PointCloudDOTName, PointDOTName]
      )
    )
  ).

write_point_to_expression_relations(Stream):-
  forall(
    point(Expression, Point),
    (
      point_to_dot_name(Point, PointDOTName),
      expression_to_dot_name(Expression, ExpressionDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [PointDOTName, ExpressionDOTName]
      )
    )
  ).

write_quantity_definition_quantity_space_definition_relations(Stream):-
  forall(
    quantity_definition_quantity_space_definition(
      QuantityDefinition,
      QuantitySpaceDefinition
    ),
    (
      quantity_definition_to_dot_name(
        QuantityDefinition,
        QuantityDefinitionDOTName
      ),
      quantity_space_definition_to_dot_name(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [QuantityDefinitionDOTName, QuantitySpaceDefinitionDOTName]
      )
    )
  ).

write_quantity_definition_relations(Stream):-
  rdf_global_id(quantity:quantity, RootQuantityDefinition),
  quantity_definition_to_dot_name(
    RootQuantityDefinition,
    RootQuantityDefinitionDOTName
  ),
  forall(
    rdf(QuantityDefinition, rdfs:subClassOf, RootQuantityDefinition, ccm),
    (
      quantity_definition_to_dot_name(
        QuantityDefinition,
        QuantityDefinitionDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [RootQuantityDefinitionDOTName, QuantityDefinitionDOTName]
      )
    )
  ).

write_quantity_definition_to_quantity_relations(Stream):-
  forall(
    (
      quantity(Quantity),
      rdf(Quantity, rdf:type, QuantityDefinition, ccm)
    ),
    (
      quantity_definition_to_dot_name(
        QuantityDefinition,
        QuantityDefinitionDOTName
      ),
      quantity_to_dot_name(Quantity, QuantityDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple"];\n',
        [QuantityDefinitionDOTName, QuantityDOTName]
      )
    )
  ).

write_quantity_proportionality_relations(Stream):-
  forall(
    quantity_proportionality(
      FromQuantity,
      Relation,
      ToQuantity,
      _Expression,
      _PointCloud
    ),
    (
      quantity_to_dot_name(FromQuantity, FromQuantityDOTName),
      quantity_to_dot_name(ToQuantity, ToQuantityDOTName),
      quantity_causality_expression_definition_sign(Relation, Sign),
      expression_definition_abbreviation(Sign, Sign_),
      format(
        Stream,
        '  ~w -> ~w [color="blue", label="P~w"];\n',
        [FromQuantityDOTName, ToQuantityDOTName, Sign_]
      )
    )
  ).

write_quantity_space_definition_relations(Stream):-
  forall(
    (
      quantity_space_definition(Parent),
      rdf(Child, rdfs:subClassOf, Parent, ccm)
    ),
    (
      quantity_space_definition_to_dot_name(Child, ChildDOTName),
      quantity_space_definition_to_dot_name(Parent, ParentDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [ParentDOTName, ChildDOTName]
      )
    )
  ).

write_quantity_space_definition_to_quantity_space_relations(Stream):-
  forall(
    (
      quantity_space_definition(QuantitySpaceDefinition),
      rdf(QuantitySpace, rdf:type, QuantitySpaceDefinition, ccm)
    ),
    (
      quantity_space_definition_to_dot_name(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionDOTName
      ),
      quantity_space_to_dot_name(QuantitySpace, QuantitySpaceDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [QuantitySpaceDefinitionDOTName, QuantitySpaceDOTName]
      )
    )
  ).

write_quantity_space_definition_to_quantity_value_definition_relations(
  Stream
):-
  forall(
    quantity_space_definition_quantity_value_definition(
      QuantitySpaceDefinition,
      QuantityValueDefinition
    ),
    (
      quantity_space_definition_to_dot_name(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionDOTName
      ),
      quantity_value_definition_to_dot_name(
        QuantityValueDefinition,
        QuantityValueDefinitionDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [QuantitySpaceDefinitionDOTName, QuantityValueDefinitionDOTName]
      )
    )
  ).

write_quantity_space_to_quantity_value_relations(Stream):-
  setoff(
    QuantitySpace/QuantityValue,
    quantity_space_quantity_value(QuantitySpace, QuantityValue),
    QuantitySpaceValuePairs
  ),
  forall(
    member(QuantitySpace/QuantityValue, QuantitySpaceValuePairs),
    (
      quantity_space_to_dot_name(QuantitySpace, QuantitySpaceDOTName),
      quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple"];\n',
        [QuantitySpaceDOTName, QuantityValueDOTName]
      )
    )
  ).

write_quantity_to_quantity_space_relations(Stream):-
  quantities(Quantities),
  forall(
    member(Quantity, Quantities),
    (
      quantity_to_dot_name(Quantity, QuantityDOTName),
      quantity_magnitude_quantity_space(Quantity, MagnitudeQuantitySpace),
      quantity_space_to_dot_name(
        MagnitudeQuantitySpace,
        MagnitudeQuantitySpaceDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple"];\n',
        [QuantityDOTName, MagnitudeQuantitySpaceDOTName]
      ),
      quantity_derivative_quantity_space(Quantity, DerivativeQuantitySpace),
      quantity_space_to_dot_name(
        DerivativeQuantitySpace,
        DerivativeQuantitySpaceDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple"];\n',
        [QuantityDOTName, DerivativeQuantitySpaceDOTName]
      )
    )
  ).

write_quantity_value_definition_relations(Stream):-
  forall(
    (
      quantity_value_definition(Parent),
      rdf(Child, rdfs:subClassOf, Parent, ccm)
    ),
    (
      quantity_value_definition_to_dot_name(Parent, ParentDOTName),
      quantity_value_definition_to_dot_name(Child, ChildDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [ParentDOTName, ChildDOTName]
      )
    )
  ).

write_quantity_value_definition_to_quantity_value_relations(Stream):-
  forall(
    quantity_value_definition_quantity_value(
      QuantityValueDefinition,
      QuantityValue
    ),
    (
      quantity_value_definition_to_dot_name(
        QuantityValueDefinition,
        QuantityValueDefinitionDOTName
      ),
      quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="purple4"];\n',
        [QuantityValueDefinitionDOTName, QuantityValueDOTName]
      )
    )
  ).

write_quantity_value_relations(Stream):-
  setoff(
    QuantityValue/HigherQuantityValue,
    quantity_value_quantity_value_strict(QuantityValue, HigherQuantityValue),
    QuantityValuePairs
  ),
  forall(
    member(QuantityValue/HigherQuantityValue, QuantityValuePairs),
    (
      quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName),
      quantity_value_to_dot_name(
        HigherQuantityValue,
        HigherQuantityValueDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="purple"];\n',
        [QuantityValueDOTName, HigherQuantityValueDOTName]
      )
    ;
      true
    )
  ).

write_request_to_explanation_offset_relations(Stream):-
  forall(
    request_explanation_offset(Request, ExplanationOffset),
    (
      request_to_dot_name(Request, RequestDOTName),
      explanation_offset_to_dot_name(
        ExplanationOffset,
        ExplanationOffsetDOTName
      ),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [RequestDOTName, ExplanationOffsetDOTName]
      )
    )
  ).

write_space_relations(Stream):-
  forall(
    space_transition(FromSpace, ToSpace),
    (
      space_to_dot_name(FromSpace, FromSpaceDOTName),
      space_to_dot_name(ToSpace, ToSpaceDOTName),
      format(
        Stream,
        '  ~w -> ~w [color="black"];\n',
        [FromSpaceDOTName, ToSpaceDOTName]
      )
    )
  ).

write_definitions_cluster(Stream, true):-
  write_cluster_begin(Stream, definitions),
  configuration_definitions(ConfigurationDefinitions),
  forall(
    member(ConfigurationDefinition, ConfigurationDefinitions),
    (
      configuration_definition_to_dot_name(
        ConfigurationDefinition,
        ConfigurationDefinitionDOTName
      ),
      format(Stream, '    ~w;\n', [ConfigurationDefinitionDOTName])
    )
  ),
  entity_definitions(EntityDefinitions),
  forall(
    member(EntityDefinition, EntityDefinitions),
    (
      entity_definition_to_dot_name(
        EntityDefinition,
        EntityDefinitionDOTName
      ),
      format(Stream, '    ~w;\n', [EntityDefinitionDOTName])
    )
  ),
  quantity_definitions(QuantityDefinitions),
  forall(
    member(QuantityDefinition, QuantityDefinitions),
    (
      quantity_definition_to_dot_name(
        QuantityDefinition,
        QuantityDefinitionDOTName
      ),
      format(Stream, '    ~w;\n', [QuantityDefinitionDOTName])
    )
  ),
  quantity_space_definitions(QuantitySpaceDefinitions),
  forall(
    member(QuantitySpaceDefinition, QuantitySpaceDefinitions),
    (
      quantity_space_definition_to_dot_name(
        QuantitySpaceDefinition,
        QuantitySpaceDefinitionDOTName
      ),
      format(Stream, '    ~w;\n', [QuantitySpaceDefinitionDOTName])
    )
  ),
  quantity_value_definitions(QuantityValueDefinitions),
  forall(
    member(QuantityValueDefinition, QuantityValueDefinitions),
    (
      quantity_value_definition_to_dot_name(
        QuantityValueDefinition,
        QuantityValueDefinitionDOTName
      ),
      format(Stream, '    ~w;\n', [QuantityValueDefinitionDOTName])
    )
  ),
  write_cluster_end(Stream),
  !.
write_definitions_cluster(_Stream, _Clusters).

write_instances_cluster(Stream, true):-
  write_cluster_begin(Stream, instances),
  attributes(Attributes),
  forall(
    member(Attribute, Attributes),
    (
      attribute_to_dot_name(Attribute, AttributeDOTName),
      format(Stream, '    ~w;\n', [AttributeDOTName])
    )
  ),
  attribute_values(AttributeValues),
  forall(
    member(AttributeValue, AttributeValues),
    (
      attribute_value_to_dot_name(AttributeValue, AttributeValueDOTName),
      format(Stream, '    ~w;\n', [AttributeValueDOTName])
    )
  ),
  entities(Entities),
  forall(
    member(Entity, Entities),
    (
      entity_to_dot_name(Entity, EntityDOTName),
      format(Stream, '    ~w;\n', [EntityDOTName])
    )
  ),
  quantities(Quantities),
  forall(
    member(Quantity, Quantities),
    (
      quantity_to_dot_name(Quantity, QuantityDOTName),
      format(Stream, '    ~w;\n', [QuantityDOTName])
    )
  ),
  quantity_spaces(QuantitySpaces),
  forall(
    member(QuantitySpace, QuantitySpaces),
    (
      quantity_space_to_dot_name(QuantitySpace, QuantitySpaceDOTName),
      format(Stream, '    ~w;\n', [QuantitySpaceDOTName])
    )
  ),
  quantity_values(QuantityValues),
  forall(
    member(QuantityValue, QuantityValues),
    (
      quantity_value_to_dot_name(QuantityValue, QuantityValueDOTName),
      format(Stream, '    ~w;\n', [QuantityValueDOTName])
    )
  ),
  write_cluster_end(Stream),
  !.
write_instances_cluster(_Stream, _Clusters).

write_expressions_cluster(Stream, true):-
  write_cluster_begin(Stream, expressions),

  expressions(Expressions),
  forall(
    member(Expression, Expressions),
    (
      expression_to_dot_name(Expression, ExpressionDOTName),
      format(Stream, '    ~w;\n', [ExpressionDOTName])
    )
  ),

  write_cluster_end(Stream),
  !.
write_expressions_cluster(_Stream, _Clusters).

write_cluster_begin(Stream, ClusterName):-
  format(
    Stream,
    '  subgraph cluster_~w {\n    label="~w"\n',
    [ClusterName, ClusterName]
  ).

write_cluster_end(Stream):-
  format(Stream, '  }\n\n', []).



% EXPORT CCM %

ccm_dot(ExclusionPredicates):-
  % Write the component cloud nodes.
  setoff(
    ComponentCloud,
    (
      component_cloud(ComponentCloud),
      \+((
        member(ExclusionPredicate, ExclusionPredicates),
        Call =.. [ExclusionPredicate, ComponentCloud],
        call(Call)
      ))
    ),
    DrawnComponentClouds
  ),
  maplist(write_component_cloud_node, DrawnComponentClouds),

  % Write the point cloud nodes.
  setoff(
    PointCloud,
    (
      component_cloud_point_cloud(ComponentCloud, PointCloud),
      \+((
        member(ExclusionPredicate, ExclusionPredicates),
        Call =.. [ExclusionPredicate, ComponentCloud],
        call(Call)
      ))
    ),
    DrawnPointClouds
  ),
  maplist(write_point_cloud_node, DrawnPointClouds),

  % Ended writing of GraphViz nodes.
  write_vertical_space(ccm),

  % Write the arcs from component clouds to point clouds.
  setoff(
    ComponentCloud/PointCloud,
    (
      member(PointCloud, DrawnPointClouds),
      component_cloud_output_point_cloud(ComponentCloud, PointCloud)
    ),
    ComponentCloudPointCloudPairs
  ),
  forall(
    member(ComponentCloud/PointCloud, ComponentCloudPointCloudPairs),
    write_component_cloud_to_point_cloud_arc(ComponentCloud, PointCloud)
  ),

  % Write the arcs from point clouds to component clouds.
  setoff(
    PointCloud/ComponentCloud,
    (
      member(PointCloud, DrawnPointClouds),
      point_cloud_output_component_cloud(PointCloud, ComponentCloud)
    ),
    PointCloudComponentCloudPairs
  ),
  forall(
    member(PointCloud/ComponentCloud, PointCloudComponentCloudPairs),
    write_point_cloud_to_component_cloud_arc(PointCloud, ComponentCloud)
  ),

  % Write the subsumption arcs. This occurs under a setting: the exclusion
  % predicated for subsumed components must be absent.
  (
    disabled_component_definition(subsumed_component_cloud)
  ->
    true
  ;
    setoff(
      SubsumingComponentCloud/SubsumedComponentCloud,
      (
        component_cloud_subsumes_component_cloud(
          SubsumingComponentCloud,
          SubsumedComponentCloud
        ),
        % We only draw subsumption relations between components that have
        % been drawn, i.e. that have nodes.
        member(SubsumingComponentCloud, DrawnComponentClouds),
        member(SubsumedComponentCloud, DrawnComponentClouds)
      ),
      Subsumptions
    ),
    forall(
      member(SubsumingComponentCloud/SubsumedComponentCloud, Subsumptions),
      (
        component_cloud_to_dot_name(
          SubsumingComponentCloud,
          SubsumingComponentCloudDOTName
        ),
        component_cloud_to_dot_name(
          SubsumedComponentCloud,
          SubsumedComponentCloudDOTName
        ),
        format(
          ccm,
          '  ~w -> ~w [color="purple", dir="forward", fontsize="11"];\n',
          [SubsumingComponentCloudDOTName, SubsumedComponentCloudDOTName]
        )
      )
    )
  ).

% Normal point cloud, export it.
write_point_cloud_node(PointCloud):-
  point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
  point_cloud_to_ccm_label(PointCloud, PointCloudLabel),
  point_cloud_color(PointCloud, PointCloudColor),
  format(
    ccm,
    '  ~w [color="~w", fontsize="11", label=<~w>, shape="ellipse", style="solid"];\n',
    [PointCloudDOTName, PointCloudColor, PointCloudLabel]
  ),
  !.
write_point_cloud_node(PointCloud):-
  debug(ccm_export, 'Point cloud ~w could not be exported.', [PointCloud]),
  gtrace. %WB

% Do not write point cloud to component cloud arcs for excluded component clouds.
write_point_cloud_to_component_cloud_arc(_PointCloud, ComponentCloud):-
  disabled_component_cloud(ComponentCloud),
  !.
write_point_cloud_to_component_cloud_arc(PointCloud, ComponentCloud):-
  component_cloud_to_dot_name(ComponentCloud, ComponentCloudDOTName),
  point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
  component_cloud_color(ComponentCloud, ComponentCloudColor),
  point_cloud_color(PointCloud, PointCloudColor),
  arc_color(PointCloudColor, ComponentCloudColor, ArcColor),
  point_cloud_index(ComponentCloud, PointCloud, PortType, PointCloudIndex),
  format(
    ccm,
    '  ~w -> ~w:~w_~w [color="~w", fontsize="11"];\n',
    [
      PointCloudDOTName,
      ComponentCloudDOTName,
      PortType,
      PointCloudIndex,
      ArcColor
    ]
  ).

% Do not write component cloud to point cloud arcs for excluded component
% clouds.
write_component_cloud_to_point_cloud_arc(ComponentCloud, _PointCloud):-
  disabled_component_cloud(ComponentCloud),
  !.
write_component_cloud_to_point_cloud_arc(ComponentCloud, PointCloud):-
  component_cloud_to_dot_name(ComponentCloud, ComponentCloudDOTName),
  point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
  component_cloud_color(ComponentCloud, ComponentCloudColor),
  point_cloud_color(PointCloud, PointCloudColor),
  arc_color(ComponentCloudColor, PointCloudColor, ArcColor),
  point_cloud_index(ComponentCloud, PointCloud, output, PointCloudIndex),
  format(
    ccm,
    '  ~w:output_~w -> ~w [color="~w", fontsize="11"];\n',
    [ComponentCloudDOTName, PointCloudIndex, PointCloudDOTName, ArcColor]
  ).

arc_color(grey, _Color, grey).
arc_color(_Color, grey, grey).
arc_color(black, _Color, black).
arc_color(_Color, black, black).
arc_color(Color, Color, Color).

% <table border="0" cellborder="1" cellspacing="0" cellpadding="4">
%   <tr>
%     <td port="input_0">in0<br/>1,2</td>
%     <td port="support_0">s0<br/>1,2</td>
%     <td rowspan="2" port="output_0">out0<br/>2,3</td>
%   </tr>
%   <tr>
%     <td port="input_1">in1</td>
%     <td colspan="1" rowspan="1">MT:0<br/>1/2,2/3</td>
%   </tr>
% </table>

write_component_cloud_node(ComponentCloud):-
  component_cloud_to_ccm_label(ComponentCloud, ComponentCloudCCMLabel),
  % Disabled components may give their disabled component definition
  % before their other/actual component definition.
  component_cloud(
    ComponentDefinition,
    Inputs,
    Supports,
    [OutputRelation/_Output],
    ComponentCloud
  ),
  \+(disabled_component_definition(ComponentDefinition)),!,

  % Table head and tail.
  TableHead = '<table border="0" cellborder="1" cellspacing="0" cellpadding="4">',
  TableTail = '</table>',
  findall(
    InputCell,
    (
      nth0(InputIndex, Inputs, InputRelation/_Input),
      component_cloud_component_relation(InputRelation, InputRelation_),
      component_cloud_to_spaces(ComponentCloud, InputRelation_, InputSpaces),
      maplist(space_to_ccm_label, InputSpaces, InputSpaceCCMLabels),
      atomic_list_concat(InputSpaceCCMLabels, ',', InputSpacesCCMLabel),
      format(
        atom(InputCell),
        '<td port="input_~w">in~w<br/>~w</td>',
        [InputIndex, InputIndex, InputSpacesCCMLabel]
      )
    ),
    [InputCell0 | InputCells]
  ),

  % Output point clouds.
  length(Inputs, NumberOfInputs),
  component_cloud_component_relation(OutputRelation, OutputRelation_),
  component_cloud_to_spaces(ComponentCloud, OutputRelation_, OutputSpaces),
  maplist(space_to_ccm_label, OutputSpaces, OutputSpaceCCMLabels),
  atomic_list_concat(OutputSpaceCCMLabels, ',', OutputSpacesCCMLabel),
  format(
    atom(OutputCell),
    '<td rowspan="~w" port="output_0">out0<br/>~w</td>',
    [NumberOfInputs, OutputSpacesCCMLabel]
  ),

  % Construe the table.
  length(Supports, NumberOfSupports),
  (
    NumberOfInputs == 1,
    NumberOfSupports == 0
  ->
    format(atom(ComponentCell), '<td>~w</td>', [ComponentCloudCCMLabel]),
    atomic_list_concat(
      [TableHead, '<tr>', InputCell0, ComponentCell, OutputCell, '</tr>'],
      TempTable
    )
  ;
    NumberOfInputs == 1,
    NumberOfSupports == 1
  ->
    Supports = [SupportRelation2/_Support2],
    component_cloud_component_relation(SupportRelation2, SupportRelation2_),
    component_cloud_to_spaces(
      ComponentCloud,
      SupportRelation2_,
      SupportSpaces2
    ),
    maplist(space_to_ccm_label, SupportSpaces2, SupportSpaceCCMLabels2),
    atomic_list_concat(SupportSpaceCCMLabels2, ',', SupportSpacesCCMLabel2),
    format(
      atom(ComponentCell),
      '<td port="support_~w">~w<br/>~w</td>',
      [0, ComponentCloudCCMLabel, SupportSpacesCCMLabel2]
    ),
    atomic_list_concat(
      [TableHead, '<tr>', InputCell0, ComponentCell, OutputCell, '</tr>'],
      TempTable
    )
  ;
    NumberOfSupports == 0
  ->
    format(
      atom(ComponentCell),
      '<td rowspan="~w">~w</td>',
      [NumberOfInputs, ComponentCloudCCMLabel]
    ),
    atomic_list_concat(
      ['<tr>', InputCell0, ComponentCell, OutputCell, '</tr>'],
      Row0
    ),
    findall(
      Row,
      (
        member(InputCell, InputCells),
        format(atom(Row), '<tr>~w</tr>', [InputCell])
      ),
      Rows
    ),
    atomic_list_concat([TableHead, Row0 | Rows], TempTable)
  ;
    % Support point clouds.
    findall(
      SupportCell,
      (
        nth0(SupportIndex1, Supports, SupportRelation1/_Support1),
        component_cloud_component_relation(SupportRelation1, SupportRelation1_),
        component_cloud_to_spaces(
          ComponentCloud,
          SupportRelation1_,
          SupportSpaces1
        ),
        maplist(space_to_ccm_label, SupportSpaces1, SupportSpaceCCMLabels1),
        atomic_list_concat(SupportSpaceCCMLabels1, ',', SupportSpacesCCMLabel1),
        format(
          atom(SupportCell),
          '<td port="support_~w">s~w<br/>~w</td>',
          [SupportIndex1, SupportIndex1, SupportSpacesCCMLabel1]
        )
      ),
      SupportCells
    ),
    atomic_list_concat(['<tr>', InputCell0 | SupportCells], TempRow0),
    atomic_list_concat([TempRow0, OutputCell, '</tr>'], Row0),
    InputCells = [InputCell1 | RestInputCells],
    NumberOfInputs_ is NumberOfInputs - 1,
    format(
      atom(ComponentCell),
      '<td colspan="~w" rowspan="~w">~w</td>',
      [NumberOfSupports, NumberOfInputs_, ComponentCloudCCMLabel]
    ),
    atomic_list_concat(['<tr>', InputCell1, ComponentCell, '</tr>'], Row1),
    findall(
      Row,
      (
        member(InputCell, RestInputCells),
        format(atom(Row), '<tr>~w</tr>', [InputCell])
      ),
      Rows
    ),
    atomic_list_concat([TableHead, Row0, Row1 | Rows], TempTable)
  ),
  atomic_list_concat([TempTable, TableTail], Table),

  % Assert the component node.
  component_cloud_to_dot_name(ComponentCloud, ComponentCloudDOTName),
  component_cloud_color(ComponentCloud, ComponentCloudColor),
  format(
    ccm,
    '  ~w [color="~w", fontsize="11", label=<~w>, shape="box", style="solid"];\n',
    [ComponentCloudDOTName, ComponentCloudColor, Table]
  ).
write_component_cloud_node(ComponentCloud):-
  debug(
    ccm_export,
    'Component cloud ~w could not be exported.',
    [ComponentCloud]
  ),
  gtrace. %WB

/*
write_space(Space, ComponentCloudFilter, PointCloudFilter):-
  write_vertical_space(ccm),
  write_space_header(Space),

  % Add the components for the given space.
  component_clouds(AllComponentClouds),
  ord_intersect(AllComponentClouds, ComponentCloudFilter, ComponentClouds),
  forall(
    member(ComponentCloud, ComponentClouds),
    (
      % We only need one exclusion predicate to succeed on a component.
      once(disabled_component_cloud(ComponentCloud))
    ->
      true
    ;
      component_cloud_to_dot_name(ComponentCloud, ComponentCloudDOTName),
      format(ccm, '    ~w;\n', [ComponentCloudDOTName])
    )
  ),

  space_to_point_clouds(Space, AllPointClouds),
  ord_intersect(AllPointClouds, PointCloudFilter, PointClouds),
  forall(
    member(PointCloud, PointClouds),
    (
      % Do not exclude entirely isolated point clouds.
      component_cloud_point_cloud(_ComponentCloud, PointCloud),
      % Exclude point clouds that link to excluded component clouds
      % exclusively.
      forall(
        component_cloud_point_cloud(ComponentCloud, PointCloud),
        disabled_component_cloud(ComponentCloud)
      )
    ->
      true
    ;
      point_cloud_to_dot_name(PointCloud, PointCloudDOTName),
      format(ccm, '    ~w;\n', [PointCloudDOTName])
    )
  ),
  write_space_footer(ccm).

write_space_footer(Stream):-
  format(Stream, '  }\n', []).

%% write_space_header(+Space:space) is det.
% Writes the header of the given space as a DOT subgraph header.
%
% @param Space A space.

write_space_header(GlobalSpace):-
  global_space(GlobalSpace),
  !,
  format(ccm, '  subgraph cluster_space_global {\n    label="Global"\n', []).
write_space_header(InputSpace):-
  input_space(InputSpace),
  !,
  format(ccm, '  subgraph cluster_space_input {\n    label="Input"\n', []).
write_space_header(State):-
  state(State),
  !,
  state_to_dot_name(State, StateDOTName),
  state_label(State, StateLabel),
  format(
    ccm,
    '  subgraph cluster_state_st_~w {\n    label="~w"\n',
    [StateDOTName, StateLabel]
  ).
write_space_header(StateTransition):-
  state_transition(StateTransition),
  !,
  state_transition_to_dot_name(StateTransition, StateTransitionDOTName),
  state_transition_label(StateTransition, StateTransitionLabel),
  format(
    ccm,
    '  subgraph cluster_transition_sttr_~w {\n    label="State transition ~w"\n',
    [StateTransitionDOTName, StateTransitionLabel]
  ).
write_space_header(SpaceTransition):-
  space_transition(SpaceTransition),
  !,
  space_to_dot_name(SpaceTransition, SpaceTransitionDOTName),
  space_transition_label(SpaceTransition, SpaceTransitionLabel),
  format(
    ccm,
    '  subgraph cluster_transition_sp_~w {\n    label="Space transition ~w"\n',
    [SpaceTransitionDOTName, SpaceTransitionLabel]
  ).
write_space_header(Space):-
  space_to_dot_name(Space, SpaceDOTName),
  format(
    ccm,
    '  subgraph cluster_transition_sp_~w {\n    label="Space ~w"\n',
    [SpaceDOTName, SpaceDOTName]
  ).
*/



% EXPORT 3: COMPONENT DEFINITIONS %

component_definition_dot:-
  write_component_definition_nodes(component_definition),
  write_vertical_space(component_definition),
  write_component_definition_relations(component_definition).


% EXPORT 5: EXPRESSION DEFINITIONS %

expression_definitions_dot:-
  inequality_expression_definition(Filter),!,
  write_expression_definition_nodes(expression_definitions, Filter),
  write_vertical_space(expression_definitions),
  write_expression_definition_relations(expression_definitions, Filter).


% EXPORT 6: SPACES %

spaces_dot:-
  write_space_nodes(spaces),
  write_vertical_space(spaces),
  write_space_relations(spaces).


% EXPORT 7: QUANTITY SPACES %

quantity_spaces_dot:-
  % Write the nodes.
  write_quantity_space_definition_nodes(quantity_spaces),
  write_quantity_space_nodes(quantity_spaces),
  write_quantity_value_definition_nodes(quantity_spaces),
  write_quantity_value_nodes(quantity_spaces),

  write_vertical_space(quantity_spaces),

  % Write the relations.
  write_quantity_space_definition_relations(quantity_spaces),
  write_quantity_space_definition_to_quantity_space_relations(
    quantity_spaces
  ),
  write_quantity_space_definition_to_quantity_value_definition_relations(
    quantity_spaces
  ),
  write_quantity_space_to_quantity_value_relations(quantity_spaces),
  write_quantity_value_definition_relations(quantity_spaces),
  write_quantity_value_definition_to_quantity_value_relations(
    quantity_spaces
  ),
  write_quantity_value_relations(quantity_spaces).


% EXPORT 8: RELATION DEFINITIONS %

relation_definitions_dot:-
  findall(
    Property,
    rdfs_subproperty_of(Property, rdf:'Property'),
    Properties1
  ),
  rdf_global_id(rdf:'Property', RootProperty),
  subtract(Properties1, [RootProperty], Properties),
  forall(
    member(Property, Properties),
    (
      rdf_global_id(Alias:Local, Property),
      format(atom(PropertyDOTName), '~w_~w', [Alias, Local]),
      rdfs_label(Property, PropertyLabel),
      format(
        relation_definitions,
        '  ~w [fontsize="11", label="~w", shape="ellipse"];\n',
        [PropertyDOTName, PropertyLabel]
      )
    )
  ),
  format(relation_definitions, '  \n', []),
  findall(
    [FromProperty, ToProperty],
    rdf(FromProperty, rdfs:subPropertyOf, ToProperty, ccm),
    PropertyPairs
  ),
  forall(
    member([FromProperty, ToProperty], PropertyPairs),
    (
      rdf_global_id(FromAlias:FromLocal, FromProperty),
      format(atom(FromPropertyDOTName), '~w_~w', [FromAlias, FromLocal]),
      rdf_global_id(ToAlias:ToLocal, ToProperty),
      format(atom(ToPropertyDOTName), '~w_~w', [ToAlias, ToLocal]),
      format(
        relation_definitions, '
        ~w -> ~w;\n',
        [FromPropertyDOTName, ToPropertyDOTName]
      )
    )
  ).


% EXPORT 9: ENTITY HIERARCHY %

entity_hierarchy_dot:-
  write_entity_definition_nodes(entity_hierarchy),
  write_entity_instance_nodes(entity_hierarchy),
  write_vertical_space(entity_hierarchy),
  write_entity_definition_relations(entity_hierarchy),
  write_entity_definition_to_entity_relations(entity_hierarchy).



% COLORS %

%% component_cloud_color(+ComponentCloud:component_cloud, -Color:atom) is det.
% Returns the color of the given component cloud.
%
% @param ComponentCloud A component cloud.
% @param Color An atomic color name.
%        1. =black= for unconsidered components.
%        2. =blue= for considered components.
%        3. =grey= for subsumed component clouds, out-of-scope component
%           clouds, and previously unpacked or disabled component clouds.

% Disabled.
component_cloud_color(ComponentCloud, grey):-
  ccm_api:disabled_component_cloud(ComponentCloud),
  !.
% Subsumed.
component_cloud_color(ComponentCloud, grey):-
  subsumed_component_cloud(ComponentCloud),
  !.
% Actively consider.
component_cloud_color(ComponentCloud, blue):-
  learner(Learner),
  does_consider_component_cloud(Learner, ComponentCloud),
  !.
% Don't consider.
component_cloud_color(_ComponentCloud, grey).

%% point_color(+Point:point, -Color:atom) is det.
% Returns the color for the given point.
%
% @param Point A point.
% @param Color The atomic name of a color.

point_color(Point, green):-
  is_premise(Point),
  !.
point_color(Point, dodgerblue):-
  is_in_node(Point),
  !.
point_color(Point, red):-
  is_out_node(Point),
  !.
point_color(_Point, black).

%% point_cloud_color(+PointCloud:point_cloud, -Color:atom) is det.
% Returns the color for the given point cloud relative to the given agent.
%
% @param PointCloud A point cloud.
% @param Color The atomic name of a color.

point_cloud_color(PointCloud, blue):-
  diagnosis(Diagnosis),
  learner(Diagnosis, Learner),
  does_consider_point_cloud(Learner, PointCloud),
  !.
point_cloud_color(PointCloud, grey):-
  point_cloud_to_component_clouds(PointCloud, ComponentClouds),
  forall(
    member(ComponentCloud, ComponentClouds),
    (
      subsumed_component_cloud(ComponentCloud)
    ;
      disabled_component_cloud(ComponentCloud)
    ;
      retrieval_component_cloud(ComponentCloud)
    ;
      learner(Learner),
      \+(does_consider_component_cloud(Learner, ComponentCloud))
    )
  ),
  !.
point_cloud_color(_PointCloud, black).
