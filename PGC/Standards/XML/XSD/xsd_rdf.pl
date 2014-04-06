:- module(
  xsd_rdf,
  [
    xsd_assert_schema/1 % +Graph:atom
  ]
).

/** <module> XSD RDF schema

@author Wouter Beek
@version 2013/10
*/

:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').



xsd_assert_schema(G):-
  forall(
    xsd_datatype(Label, Resource),
    (
      rdf_assert_individual(Resource, rdfs:'Datatype', G),
      rdfs_assert_label(Resource, en, Label, G)
    )
  ).

