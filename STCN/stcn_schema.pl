:- module(
  stcn_schema,
  [
    stcn_schema/1 % +Graph:atom
  ]
).

/** <module> STCN_SCHEMA

Schema for the STCN database.

@author Wouter Beek
@tbd Use XLink for resources that denote Web pages.
@version 2013/01-2013/03, 2013/06, 2013/09-2013/10
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(picarta(picarta_query)). % Make private call.
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(stcn(stcn_kmc)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_rdf)).

:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/vocab/').



stcn_schema(G):-
  % Add the RDFS statements for XSD.
  xsd_assert_schema(G),
  
  % Publication
  rdfs_assert_class(stcnv:'Publication', G),
  rdfs_assert_label(stcnv:'Publication', en, publication, G),
  rdfs_assert_label(stcnv:'Publication', nl, publicatie, G),
  
  % Topic
  rdfs_assert_class(stcnv:'Topic', G),
  rdfs_assert_label(stcnv:'Topic', en, topic, G),
  rdfs_assert_label(stcnv:'Topic', nl, onderwerp, G),

  % Denotes relations between a PPN and the location from which information
  % about that PPN was extracted.
  rdf_assert_property(stcnv:scrapedFrom, G),
  rdfs_assert_label(stcnv:scrapedFrom, en, 'scraped from', G),
  rdfs_assert_comment(stcnv:scrapedFrom, en,
    'Some information of this PPN was scraped from this location.', G),
  rdfs_assert_domain(stcnv:scrapedFrom, stcnv:'Publication', G),
  
  % The KB uses this predicate letter to denote the same property.
  rdf_assert_property(stcnv:kb_name, G),
  rdfs_assert_domain(stcnv:kb_name, rdf:'Property', G),
  rdfs_assert_range(stcnv:kb_name, rdfs:'Literal', G),
  
  % Picarta uses this predicate letter to denote the same property.
  rdf_assert_property(stcnv:picarta_name, G),
  rdfs_assert_domain(stcnv:picarta_name, rdf:'Property', G),
  rdfs_assert_range(stcnv:picarta_name, rdfs:'Literal', G),
  
  % Picarta-derived properties
  forall(
    picarta_query:picarta_attribute(_Match, AttributeName),
    (
      rdf_global_id(stcnv:AttributeName, Attribute),
      rdf_assert_property(Attribute, G)
    )
  ),

  assert_schema_kmcs(G).

