:- module(ccm_kb, []).

/** <module> CCM KB

This module handles the generic information for the Component Connection
Model.

Specific information for CCMs is asserted in =ccm_db=.

An important part of the static CCM KB are the CCM component definitions. They
are defined in a cross-linked hierarchy.

@author Wouter Beek
@version Aug 2011 - Mrt 2012, Aug 2012 - Sep 2012
*/

:- use_module(ccm(ccm_prob)).
:- use_module(rdf(rdf_build)).



:-
% COMPONENT CLOUDS
  rdf_assert(component_cloud:component_cloud, rdfs:subClassOf, node:nnode, ccm),
  rdfs_assert_label(component_cloud:component_cloud, 'component cloud', ccm),

  rdf_assert(component_cloud:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(component_cloud:relation, 'component cloud relation', ccm),

  rdf_assert(component_cloud:has_id, rdfs:subPropertyOf, component_cloud:relation, ccm),
  rdfs_assert_label(component_cloud:has_id, 'component cloud has ID', ccm),

  rdf_assert(component_cloud:has_component, rdfs:subPropertyOf, component_cloud:relation, ccm),
  rdfs_assert_label(component_cloud:has_component, 'component cloud has component', ccm),

  rdf_assert(component_cloud:subsumes, rdfs:subPropertyOf, component_cloud:relation, ccm),
  rdfs_assert_label(component_cloud:subsumes, 'aggregate component cloud subsumes component clouds', ccm),

  rdf_assert(component_cloud:has_space_definition, rdfs:subPropertyOf, component_cloud:relation, ccm),
  rdfs_assert_label(component_cloud:has_space_definition, 'component cloud has space definition', ccm),

  rdf_assert(component_cloud:has_input_space_definition, rdfs:subPropertyOf, component_cloud:has_space_definition, ccm),
  rdfs_assert_label(component_cloud:has_input_space_definition, 'component cloud has input space definition', ccm),

  rdf_assert(component_cloud:has_output_space_definition, rdfs:subPropertyOf, component_cloud:has_space_definition, ccm),
  rdfs_assert_label(component_cloud:has_output_space_definition, 'component cloud has output space definition', ccm),

% COMPONENT CLOUD / POINT CLOUD RELATIONS
  rdf_assert(component_cloud:has_point_cloud, rdfs:subPropertyOf, component_cloud:relation, ccm),
  rdfs_assert_label(component_cloud:has_point_cloud, 'component has point', ccm),

  rdf_assert(component_cloud:has_input, rdfs:subPropertyOf, component_cloud:has_point_cloud, ccm),
  rdfs_assert_label(component_cloud:has_input, 'component has input', ccm),

  rdf_assert(component_cloud:has_within_state_input, rdfs:subPropertyOf, component_cloud:has_input, ccm),
  rdfs_assert_label(component_cloud:has_within_state_input, 'component has within state input', ccm),

  rdf_assert(component_cloud:has_derivative_input, rdfs:subPropertyOf, component_cloud:has_within_state_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_input, 'component has derivative input', ccm),

  rdf_assert(component_cloud:has_derivative_input1, rdfs:subPropertyOf, component_cloud:has_derivative_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_input1, 'component has derivative input 1', ccm),

  rdf_assert(component_cloud:has_derivative_input2, rdfs:subPropertyOf, component_cloud:has_derivative_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_input2, 'component has derivative input 2', ccm),

  rdf_assert(component_cloud:has_magnitude_input, rdfs:subPropertyOf, component_cloud:has_within_state_input, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_input, 'component has magnitude input', ccm),

  rdf_assert(component_cloud:has_magnitude_input1, rdfs:subPropertyOf, component_cloud:has_magnitude_input, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_input1, 'component has magnitude input 1', ccm),

  rdf_assert(component_cloud:has_magnitude_input2, rdfs:subPropertyOf, component_cloud:has_magnitude_input, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_input2, 'component has magnitude input 2', ccm),

  rdf_assert(component_cloud:has_support_input, rdfs:subPropertyOf, component_cloud:has_input, ccm),
  rdfs_assert_label(component_cloud:has_support_input, 'component has support input', ccm),

  rdf_assert(component_cloud:has_quantity_space_input, rdfs:subPropertyOf, component_cloud:has_support_input, ccm),
  rdfs_assert_label(component_cloud:has_quantity_space_input, 'component has quantity space input', ccm),

  rdf_assert(component_cloud:has_output, rdfs:subPropertyOf, component_cloud:has_point_cloud, ccm),
  rdfs_assert_label(component_cloud:has_output, 'component has output', ccm),

  rdf_assert(component_cloud:has_within_state_output, rdfs:subPropertyOf, component_cloud:has_output, ccm),
  rdfs_assert_label(component_cloud:has_within_state_output, 'component has within state output', ccm),

  rdf_assert(component_cloud:has_derivative_output, rdfs:subPropertyOf, component_cloud:has_within_state_output, ccm),
  rdfs_assert_label(component_cloud:has_derivative_output, 'component has derivative output', ccm),

  rdf_assert(component_cloud:has_magnitude_output, rdfs:subPropertyOf, component_cloud:has_within_state_output, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_output, 'component has magnitude output', ccm),

  rdf_assert(component_cloud:has_support_output, rdfs:subPropertyOf, component_cloud:has_output, ccm),
  rdfs_assert_label(component_cloud:has_support_output, 'component has support output', ccm),

  rdf_assert(component_cloud:has_quantity_space_output, rdfs:subPropertyOf, component_cloud:has_support_output, ccm),
  rdfs_assert_label(component_cloud:has_quantity_space_output, 'component has quantity space output', ccm),

  rdf_assert(component_cloud:has_support, rdfs:subPropertyOf, component_cloud:has_point_cloud, ccm),
  rdfs_assert_label(component_cloud:has_support, 'component has support', ccm),

  rdf_assert(component_cloud:has_support1, rdfs:subPropertyOf, component_cloud:has_support, ccm),
  rdfs_assert_label(component_cloud:has_support1, 'component has support 1', ccm),

  rdf_assert(component_cloud:has_support2, rdfs:subPropertyOf, component_cloud:has_support, ccm),
  rdfs_assert_label(component_cloud:has_support2, 'component has support 2', ccm),

  rdf_assert(component_cloud:has_inequality_input, rdfs:subPropertyOf, component_cloud:has_input, ccm),
  rdfs_assert_label(component_cloud:has_inequality_input, 'component has inequality input', ccm),

  rdf_assert(component_cloud:has_derivative_inequality_input, rdfs:subPropertyOf, component_cloud:has_inequality_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_inequality_input, 'component has derivative inequality input', ccm),

  rdf_assert(component_cloud:has_derivative_inequality_input1, rdfs:subPropertyOf, component_cloud:has_derivative_inequality_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_inequality_input1, 'component has derivative inequality input 1', ccm),

  rdf_assert(component_cloud:has_derivative_inequality_input2, rdfs:subPropertyOf, component_cloud:has_derivative_inequality_input, ccm),
  rdfs_assert_label(component_cloud:has_derivative_inequality_input2, 'component has derivative inequality input 2', ccm),

  rdf_assert(component_cloud:has_magnitude_inequality_input, rdfs:subPropertyOf, component_cloud:has_inequality_input, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_inequality_input, 'component has magnitude inequality input', ccm),

  rdf_assert(component_cloud:has_inequality_output, rdfs:subPropertyOf, component_cloud:has_output, ccm),
  rdfs_assert_label(component_cloud:has_inequality_output, 'component has inequality output', ccm),

  rdf_assert(component_cloud:has_derivative_inequality_output, rdfs:subPropertyOf, component_cloud:has_inequality_output, ccm),
  rdfs_assert_label(component_cloud:has_derivative_inequality_output, 'component has derivative inequality output', ccm),

  rdf_assert(component_cloud:has_magnitude_inequality_output, rdfs:subPropertyOf, component_cloud:has_inequality_output, ccm),
  rdfs_assert_label(component_cloud:has_magnitude_inequality_output, 'component has magnitude inequality output', ccm),

  rdf_assert(component_cloud:has_calculus_input, rdfs:subPropertyOf, component_cloud:has_input, ccm),
  rdfs_assert_label(component_cloud:has_calculus_input, 'component has calculus input', ccm),

  rdf_assert(component_cloud:has_calculus_output, rdfs:subPropertyOf, component_cloud:has_output, ccm),
  rdfs_assert_label(component_cloud:has_calculus_output, 'component has calculus output', ccm),

% COMPONENT DEFINITIONS
  rdf_assert(component:component, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(component:component, component, ccm),

  rdf_assert(component:relation, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(component:relation, 'component relation', ccm),

  rdf_assert(component:has_point_specification, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point_specification, 'component has point specification', ccm),

  rdf_assert(component:has_point_name, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point_name, 'component has point name', ccm),

  rdf_assert(component:has_point_expression_definition, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point_expression_definition, 'component has point expression definition', ccm),

  rdf_assert(component:has_point_relation, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point_relation, 'component has point relation', ccm),

% COMPONENTS
  rdf_assert(component:has_id, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_id, 'component has ID', ccm),

  absolute_file_name(ccm(components), ComponentsDirectory),
  rdf_assert_directory(ComponentsDirectory, ccm),
  
  rdf_assert(component:has_point_cloud, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point_cloud, 'component has point cloud', ccm),

  rdf_assert(component:has_point_cloud_active, rdfs:subPropertyOf, component:has_point_cloud, ccm),
  rdfs_assert_label(component:has_point_cloud_active, 'component has active point cloud', ccm),

  rdf_assert(component:has_point_cloud_inactive, rdfs:subPropertyOf, component:has_point_cloud, ccm),
  rdfs_assert_label(component:has_point_cloud_inactive, 'component has inactive point cloud', ccm),

% COMPONENT/POINT-RELATIONS
  rdf_assert(component:has_point, rdfs:subPropertyOf, component:relation, ccm),
  rdfs_assert_label(component:has_point, 'component has point', ccm),

  rdf_assert(component:has_input, rdfs:subPropertyOf, component:has_point, ccm),
  rdfs_assert_label(component:has_input, 'component has input', ccm),

  rdf_assert(component:has_within_state_input, rdfs:subPropertyOf, component:has_input, ccm),
  rdfs_assert_label(component:has_within_state_input, 'component has within state input', ccm),

  rdf_assert(component:has_derivative_input, rdfs:subPropertyOf, component:has_within_state_input, ccm),
  rdfs_assert_label(component:has_derivative_input, 'component has derivative input', ccm),

  rdf_assert(component:has_derivative_input1, rdfs:subPropertyOf, component:has_derivative_input, ccm),
  rdfs_assert_label(component:has_derivative_input1, 'component has derivative input 1', ccm),

  rdf_assert(component:has_derivative_input2, rdfs:subPropertyOf, component:has_derivative_input, ccm),
  rdfs_assert_label(component:has_derivative_input2, 'component has derivative input 2', ccm),

  rdf_assert(component:has_magnitude_input, rdfs:subPropertyOf, component:has_within_state_input, ccm),
  rdfs_assert_label(component:has_magnitude_input, 'component has magnitude input', ccm),

  rdf_assert(component:has_magnitude_input1, rdfs:subPropertyOf, component:has_magnitude_input, ccm),
  rdfs_assert_label(component:has_magnitude_input1, 'component has magnitude input 1', ccm),

  rdf_assert(component:has_magnitude_input2, rdfs:subPropertyOf, component:has_magnitude_input, ccm),
  rdfs_assert_label(component:has_magnitude_input2, 'component has magnitude input 2', ccm),

  rdf_assert(component:has_support_input, rdfs:subPropertyOf, component:has_input, ccm),
  rdfs_assert_label(component:has_support_input, 'component has support input', ccm),

  rdf_assert(component:has_quantity_space_input, rdfs:subPropertyOf, component:has_support_input, ccm),
  rdfs_assert_label(component:has_quantity_space_input, 'component has quantity space input', ccm),

  rdf_assert(component:has_output, rdfs:subPropertyOf, component:has_point, ccm),
  rdfs_assert_label(component:has_output, 'component has output', ccm),

  rdf_assert(component:has_within_state_output, rdfs:subPropertyOf, component:has_output, ccm),
  rdfs_assert_label(component:has_within_state_output, 'component has within state output', ccm),

  rdf_assert(component:has_derivative_output, rdfs:subPropertyOf, component:has_within_state_output, ccm),
  rdfs_assert_label(component:has_derivative_output, 'component has derivative output', ccm),

  rdf_assert(component:has_magnitude_output, rdfs:subPropertyOf, component:has_within_state_output, ccm),
  rdfs_assert_label(component:has_magnitude_output, 'component has magnitude output', ccm),

  rdf_assert(component:has_support_output, rdfs:subPropertyOf, component:has_output, ccm),
  rdfs_assert_label(component:has_support_output, 'component has support output', ccm),

  rdf_assert(component:has_quantity_space_output, rdfs:subPropertyOf, component:has_support_output, ccm),
  rdfs_assert_label(component:has_quantity_space_output, 'component has quantity space output', ccm),

  rdf_assert(component:has_support, rdfs:subPropertyOf, component:has_point, ccm),
  rdfs_assert_label(component:has_support, 'component has support', ccm),

  rdf_assert(component:has_support1, rdfs:subPropertyOf, component:has_support, ccm),
  rdfs_assert_label(component:has_support1, 'component has support 1', ccm),

  rdf_assert(component:has_support2, rdfs:subPropertyOf, component:has_support, ccm),
  rdfs_assert_label(component:has_support2, 'component has support 2', ccm),

  rdf_assert(component:has_inequality_input, rdfs:subPropertyOf, component:has_input, ccm),
  rdfs_assert_label(component:has_inequality_input, 'component has inequality input', ccm),

  rdf_assert(component:has_derivative_inequality_input, rdfs:subPropertyOf, component:has_inequality_input, ccm),
  rdfs_assert_label(component:has_derivative_inequality_input, 'component has derivative inequality input', ccm),

  rdf_assert(component:has_derivative_inequality_input1, rdfs:subPropertyOf, component:has_derivative_inequality_input, ccm),
  rdfs_assert_label(component:has_derivative_inequality_input1, 'component has derivative inequality input 1', ccm),

  rdf_assert(component:has_derivative_inequality_input2, rdfs:subPropertyOf, component:has_derivative_inequality_input, ccm),
  rdfs_assert_label(component:has_derivative_inequality_input2, 'component has derivative inequality input 2', ccm),

  rdf_assert(component:has_magnitude_inequality_input, rdfs:subPropertyOf, component:has_inequality_input, ccm),
  rdfs_assert_label(component:has_magnitude_inequality_input, 'component has magnitude inequality input', ccm),

  rdf_assert(component:has_inequality_output, rdfs:subPropertyOf, component:has_output, ccm),
  rdfs_assert_label(component:has_inequality_output, 'component has inequality output', ccm),

  rdf_assert(component:has_derivative_inequality_output, rdfs:subPropertyOf, component:has_inequality_output, ccm),
  rdfs_assert_label(component:has_derivative_inequality_output, 'component has derivative inequality output', ccm),

  rdf_assert(component:has_magnitude_inequality_output, rdfs:subPropertyOf, component:has_inequality_output, ccm),
  rdfs_assert_label(component:has_magnitude_inequality_output, 'component has magnitude inequality output', ccm),

  rdf_assert(component:has_calculus_input, rdfs:subPropertyOf, component:has_input, ccm),
  rdfs_assert_label(component:has_calculus_input, 'component has calculus input', ccm),

  rdf_assert(component:has_calculus_output, rdfs:subPropertyOf, component:has_output, ccm),
  rdfs_assert_label(component:has_calculus_output, 'component has calculus output', ccm),

% EXPLANATIONS
  rdf_assert(explanation:explanation, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(explanation:explanation, 'explanation', ccm),

  rdf_assert(explanation:request, rdfs:subClassOf, explanation:explanation, ccm),
  rdfs_assert_label(explanation:request, 'request', ccm),

  rdf_assert(explanation:explanation_offset, rdfs:subClassOf, explanation:explanation, ccm),
  rdfs_assert_label(explanation:explanation_offset, 'explanation offset', ccm),

  rdf_assert(explanation:singular, rdfs:subClassOf, explanation:explanation, ccm),
  rdfs_assert_label( explanation:singular, 'singular explanation', ccm),

  rdf_assert(explanation:proposition, rdfs:subClassOf, explanation:explanation, ccm),
  rdfs_assert_label(explanation:proposition, 'proposition in an explanation', ccm),

% RELATIONS
  rdf_assert(explanation:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(explanation:relation, 'explanation relation', ccm),

  rdf_assert(explanation:has_id, rdfs:subPropertyOf, explanation:relation, ccm),
  rdfs_assert_label(explanation:has_id, 'explanation has id', ccm),

  rdf_assert(explanation:has_explanation, rdfs:subPropertyOf, explanation:relation, ccm),
  rdfs_assert_label(explanation:has_explanation, 'explanation offset has explanation', ccm),

  rdf_assert(explanation:has_explanation_offset, rdfs:subPropertyOf, explanation:relation, ccm),
  rdfs_assert_label(explanation:has_explanation_offset, 'request has explanation offset', ccm),

  rdf_assert(explanation:about_node, rdfs:subPropertyOf, explanation:relation, ccm),
  rdfs_assert_label(explanation:about_node, 'explanation about node', ccm),

  rdf_assert(explanation:about_component, rdfs:subPropertyOf, explanation:about_node, ccm),
  rdfs_assert_label(explanation:about_component, 'explanation about component', ccm),

  rdf_assert(explanation:about_point, rdfs:subPropertyOf, explanation:about_node, ccm),
  rdfs_assert_label(explanation:about_point, 'explanation about point', ccm),

  rdf_assert(explanation:has_proposition, rdfs:subPropertyOf, explanation:relation, ccm),
  rdfs_assert_label(explanation:has_proposition, 'explanation has proposition', ccm),

  rdf_assert(explanation:has_premise, rdfs:subPropertyOf, explanation:has_proposition, ccm),
  rdfs_assert_label(explanation:has_premise, 'explanation has premise', ccm),

  rdf_assert(explanation:has_conclusion, rdfs:subPropertyOf, explanation:has_proposition, ccm),
  rdfs_assert_label(explanation:has_conclusion, 'explanation has conclusion', ccm),

% EXPRESSIONS
  rdf_assert(expression:expression, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(expression:expression, 'expression', ccm),

  absolute_file_name(ccm(expressions), ExpressionsDirectory),
  rdf_assert_directory(ExpressionsDirectory, ccm),
  
  rdf_global_id(expression:qp_derivative_equal_to, DerivativeValueExpression),
  set_expression_probability(DerivativeValueExpression, 0.1),

  rdf_global_id(expression:qp_magnitude_equal_to, MagnitudeValueExpression),
  set_expression_probability(MagnitudeValueExpression, 0.2),

  rdf_global_id(expression:inequality, InequalityExpression),
  set_expression_probability(InequalityExpression, 0.3),

  rdf_global_id(expression:quantity_space, QuantitySpaceExpression),
  set_expression_probability(QuantitySpaceExpression, 0.01),

  rdf_global_id(expression:support, SupportExpression),
  set_expression_probability(SupportExpression, 0.05),

  rdf_assert(expression:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(expression:relation, 'expression relation', ccm),

  rdf_assert(expression:has_argument, rdfs:subPropertyOf, expression:relation, ccm),
  rdfs_assert_label(expression:has_argument, 'expression has argument', ccm),

  rdf_assert(expression:has_from_argument, rdfs:subPropertyOf, expression:has_argument, ccm),
  rdfs_assert_label(expression:has_from_argument, 'expression has from argument', ccm),

  rdf_assert(expression:has_id, rdfs:subPropertyOf, expression:relation, ccm),
  rdfs_assert_label(expression:has_id, 'expression has ID', ccm),

  rdf_assert(expression:has_to_argument, rdfs:subPropertyOf, expression:has_argument, ccm),
  rdfs_assert_label(expression:has_to_argument, 'expression has to argument', ccm),

  rdf_assert(expression:has_quantity_space, rdfs:subPropertyOf, expression:relation, ccm),
  rdfs_assert_label(expression:has_quantity_space, 'expression has quantity space', ccm),

  rdf_assert(expression:has_relation, rdfs:subPropertyOf, expression:relation, ccm),
  rdfs_assert_label(expression:has_relation, 'expression has relation', ccm),

  rdf_assert(expression:has_point, rdfs:subPropertyOf, expression:relation, ccm),
  rdfs_assert_label(expression:has_point, 'expression has point', ccm),

% POINTS
  rdf_assert(point:point, rdfs:subClassOf, node:nnode, ccm),
  rdfs_assert_label(point:point, 'point', ccm),

  rdf_assert(point:relation, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(point:relation, 'point relation', ccm),

  rdf_assert(point:has_id, rdfs:subPropertyOf, point:relation, ccm),
  rdfs_assert_label(point:has_id, 'point has ID', ccm),

  rdf_assert(point:has_component, rdfs:subPropertyOf, point:relation, ccm),
  rdfs_assert_label(point:has_component, 'point has component', ccm),

  rdf_assert(point:has_input_component, rdfs:subPropertyOf, point:has_component, ccm),
  rdfs_assert_label(point:has_input_component, 'point has input component', ccm),

  rdf_assert(point:has_output_component, rdfs:subPropertyOf, point:has_component, ccm),
  rdfs_assert_label(point:has_output_component, 'point has output component', ccm),

% POINT CLOUDS
  rdf_assert(point_cloud:point_cloud, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(point_cloud:point_cloud, 'point cloud', ccm),

  rdf_assert(point_cloud:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(point_cloud:relation, 'point cloud relation', ccm),

  rdf_assert(point_cloud:has_probe_request, rdfs:subPropertyOf, point_cloud:relation, ccm),
  rdfs_assert_label(point_cloud:has_probe_request, 'point cloud has probe request', ccm),

  rdf_assert(point_cloud:has_id, rdfs:subPropertyOf, point_cloud:relation, ccm),
  rdfs_assert_label(point_cloud:has_id, 'point cloud has id', ccm),

  rdf_assert(point_cloud:has_point, rdfs:subPropertyOf, point_cloud:relation, ccm),
  rdfs_assert_label(point_cloud:has_point, 'point cloud has point', ccm),

% REQUESTS
  rdf_assert(request:request, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(request:request, 'request', ccm),

  rdf_assert(request:probe_request, rdfs:subClassOf, request:request, ccm),
  rdfs_assert_label(request:probe_request, 'Request information regarding a probe point.', ccm),

  rdf_assert(request:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(request:relation, 'request has relation', ccm),

  rdf_assert(request:has_id, rdfs:subPropertyOf, request:relation, ccm),
  rdfs_assert_label(request:has_id, 'request has id', ccm),

  rdf_assert(request:has_probe_point_cloud, rdfs:subPropertyOf, request:relation, ccm),
  rdfs_assert_label(request:has_probe_point_cloud, 'request probe point cloud', ccm),

  rdf_assert(request:has_agent, rdfs:subPropertyOf, request:relation, ccm),
  rdfs_assert_label(request:has_agent, 'request has agent', ccm),

  rdf_assert(request:has_questioner, rdfs:subPropertyOf, request:has_agent, ccm),
  rdfs_assert_label(request:has_questioner, 'request has questioner', ccm),

  rdf_assert(request:has_answerer, rdfs:subPropertyOf, request:has_agent, ccm),
  rdfs_assert_label(request:has_answerer, 'request has answerer', ccm),

% SPACES
  % Space root.
  rdf_assert(space:space, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(space:space, 'space', ccm),

  % State root.
  rdf_assert(space:state, rdfs:subClassOf, space:space, ccm),
  rdfs_assert_label(space:state, 'state', ccm),

  % Space transition root.
  rdf_assert(space:space_transition, rdfs:subClassOf, space:space, ccm),
  rdfs_assert_label(space:space_transition, 'space transition', ccm),

  % State transition root.
  rdf_assert(space:state_transition, rdfs:subClassOf, space:space_transition, ccm),
  rdfs_assert_label(space:state_transition, 'state transition', ccm),

  % Input space.
  rdf_assert(space:input, rdf:type, space:space, ccm),
  rdfs_assert_label(space:input, 'input', ccm),
  rdf_assert_datatype(space:input, space:has_id, integer, 0, ccm),

  % Global space.
  rdf_assert(space:global, rdf:type, space:space, ccm),
  rdfs_assert_label(space:global, 'global', ccm),
  rdf_assert_datatype(space:global, space:has_id, integer, 1, ccm),

  % Temporary space.
  rdf_assert(space:temp, rdf:type, space:space, ccm),
  rdfs_assert_label(space:temp, 'temporary', ccm),
  rdf_assert_datatype(space:temp, space:has_id, integer, 2, ccm),

  rdf_assert(space:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(space:relation, 'space relation', ccm),

  rdf_assert(space:has_id, rdfs:subPropertyOf, space:relation, ccm),
  rdfs_assert_label(space:has_id, 'space has ID', ccm),

  rdf_assert(space:has_node, rdfs:subPropertyOf, space:relation, ccm),
  rdfs_assert_label(space:has_node, 'space has node', ccm),

  rdf_assert(space:has_point, rdfs:subPropertyOf, space:has_node, ccm),
  rdfs_assert_label(space:has_point, 'space has point', ccm),

  rdf_assert(space:has_component, rdfs:subPropertyOf, space:has_node, ccm),
  rdfs_assert_label(space:has_component, 'space has component', ccm),

  rdf_assert(space:has_to_space, rdfs:subPropertyOf, space:relation, ccm),
  rdfs_assert_label(space:has_to_space, 'space has to space', ccm),

  rdf_assert(space:has_to_state, rdfs:subPropertyOf, space:has_to_space, ccm),
  rdfs_assert_label(space:has_to_state, 'space has to state', ccm).
