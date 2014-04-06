:- module(atms_hierarchy, [assert_atms_hierarchy/0]).

/** <module> ATMS hierarchy

@author Wouter Beek
@version 2011/12, 2012/03, 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(atms, 'http://www.wouterbeek.com/atms#').
:- xml_register_namespace(environment, 'http://www.wouterbeek.com/environment#').
:- xml_register_namespace(justification, 'http://www.wouterbeek.com/justification#').
:- xml_register_namespace(node, 'http://www.wouterbeek.com/node#').



assert_atms_hierarchy:-
% ATMS
  rdf_assert(atms:atms, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(atms:atms, 'atms', ccm),

  rdf_assert(atms:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(atms:relation, 'atms relation', ccm),

  rdf_assert(atms:has_assumption, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_assumption, 'atms has assumption', ccm),

  rdf_assert(atms:has_contradiction, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_contradiction, 'atms has contradiction', ccm),

  rdf_assert(atms:has_contradiction_node, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_contradiction_node, 'atms has contradiction node', ccm),

  rdf_assert(atms:has_empty_environment, rdfs:subPropertyOf, atms:has_environment, ccm),
  rdfs_assert_label(atms:has_empty_environment, 'atms has empty environment', ccm),

  rdf_assert(atms:has_environment, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_environment, 'atms has environment', ccm),

  rdf_assert(atms:has_id, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_id, 'An ATMS has an identifier.', ccm),
  
  rdf_assert(atms:has_justification, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_justification, 'atms has justification', ccm),

  rdf_assert(atms:has_nogood, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_nogood, 'atms has nogood', ccm),

  rdf_assert(atms:has_node, rdfs:subPropertyOf, atms:relation, ccm),
  rdfs_assert_label(atms:has_node, 'atms has node', ccm),

% ENVIRONMENTS
  rdf_assert(environment:environment, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(environment:environment, 'environment', ccm),

  rdf_assert(environment:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(environment:relation, 'environment relation', ccm),

  rdf_assert(environment:has_id, rdfs:subPropertyOf, environment:relation, ccm),
  rdfs_assert_label(environment:has_id, 'environment has id', ccm),

  rdf_assert(environment:has_node, rdfs:subPropertyOf, environment:relation, ccm),
  rdfs_assert_label(environment:has_node, 'environment has node', ccm),

  rdf_assert(environment:is_nogood, rdfs:subPropertyOf, environment:relation, ccm),
  rdfs_assert_label(environment:is_nogood, 'Whether this environment is a nogood.', ccm),

% JUSTIFICATIONS
  rdf_assert(justification:justification, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(justification:justification, 'justification', ccm),

  rdf_assert(justification:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(justification:relation, 'justification relation', ccm),

  rdf_assert(justification:has_antecedent, rdfs:subPropertyOf, justification:relation, ccm),
  rdfs_assert_label(justification:has_antecedent, 'Justification has an antecedent node.', ccm),
  
  rdf_assert(justification:has_consequence, rdfs:subPropertyOf, justification:relation, ccm),
  rdfs_assert_label(justification:has_consequence, 'Justification has a consequence node.', ccm),

  rdf_assert(justification:has_id, rdfs:subPropertyOf, justification:relation, ccm),
  rdfs_assert_label(justification:has_id, 'justification has id', ccm),

  rdf_assert(justification:has_informant, rdfs:subPropertyOf, justification:relation, ccm),
  rdfs_assert_label(justification:has_informant, 'justification has informant', ccm),

% NODES
  rdf_assert(node:nnode, rdfs:subClassOf, rdfs:'Class', ccm),
  rdfs_assert_label(node:nnode, 'nnode', ccm),

  % Since every ATMS has its own falsum node, this is a class.
  rdf_assert(node:falsum, rdfs:subClassOf, node:nnode, ccm),
  rdfs_assert_label(node:falsum, 'falsum nodes', ccm),

  rdf_assert(node:relation, rdfs:subPropertyOf, rdf:'Property', ccm),
  rdfs_assert_label(node:relation, 'node relation', ccm),

  rdf_assert(node:has_consequence, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:has_consequence, 'node has consequence justification', ccm),

  rdf_assert(node:has_datum, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:has_datum, 'node has datum', ccm),

  rdf_assert(node:has_environment, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:has_environment, 'node has environment', ccm),

  rdf_assert(node:has_id, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:has_id, 'node has id', ccm),

  rdf_assert(node:is_assumption, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:is_assumption, 'Node is an assumption.', ccm),

  rdf_assert(node:is_contradiction, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:is_contradiction, 'Node is a contradiction.', ccm),

  rdf_assert(node:has_justification, rdfs:subPropertyOf, node:relation, ccm),
  rdfs_assert_label(node:has_justification, 'node has justification', ccm).
