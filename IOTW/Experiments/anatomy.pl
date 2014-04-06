:- module(
  anatomy,
  [
  ]
).

/** <module> ANATOMY

Runs IOTW experiments on the anatomy alignment data.

--

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(db_ext)).
:- use_module(iotw(iotw_relatedness)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(os(filepath_ext)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(
  oboInOwl,
  'http://www.geneontology.org/formats/oboInOwl#'
).
:- xml_register_namespace(oboRel, 'http://www.obofoundry.org/ro/ro.owl#').
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

user:file_search_path(anatomy, oaei2012('Anatomy')).
user:file_search_path(raw_results, anatomy('Raw results')).



load_alignment_anatomy:-
  % Create a unique graph name.
  rdf_new_graph(anatomy, OntologyGraph),
  
  % Load the human ontology.
  absolute_file_name(
    anatomy(human),
    HumanFile,
    [access(read), file_type(owl)]
  ),
  rdf_load([], human, HumanFile),
  
  % Load the mouse ontology.
  absolute_file_name(
    anatomy(mouse),
    MouseFile,
    [access(read), file_type(owl)]
  ),
  rdf_load([], mouse, MouseFile),
  rdf_graph_merge([human, mouse], OntologyGraph),
  
  % Process the various alignments.
  absolute_file_name(
    raw_results(.),
    AlignmentDir,
    [access(read), file_type(directory)]
  ),
  path_walk_tree(AlignmentDir, '.*.rdf$', AlignmentFiles),
  forall(
    member(AlignmentFile, AlignmentFiles),
    (
      oaei_file_to_alignment_pairs(AlignmentFile, Alignments),
      assert_inodes(_O, OntologyGraph, Alignments, GA_Hash),
      export_inodes(O, GA_Hash, _SVG)
    )
  ).

