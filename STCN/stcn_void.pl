:- module(
  stcn_void,
  [
    stcn_void/1 % +Graph:atom
  ]
).

/** <module> STCN VoID

Generates the VoID description of the STCN dataset.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(rdfs(rdfs_read)).
:- use_module(void(void_db)). % XML namespace.
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_dateTime_ext)).

:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/'    ).
:- xml_register_namespace(foaf,    'http://xmlns.com/foaf/0.1/'   ).
:- xml_register_namespace(format,  'http://www.w3.org/ns/formats/').



stcn_void(G):-
  % STCN dataset
  rdf_assert_individual(stcn:'STCN', void:'DatasetDescription', G),
  
  % Contributors.
  % Albert
  rdf_assert_individual(stcn:'AlbertMeroñoPeñuela', foaf:'Person', G),
  rdfs_assert_label(stcn:'AlbertMeroñoPeñuela', 'Albert Meroño Peñuela', sp, G),
  rdf_assert(stcn:'AlbertMeroñoPeñuela', foaf:mbox, 'mailto:albert.merono@vu.nl', G),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'AlbertMeroñoPeñuela', G),
  % Rinke
  rdf_assert_individual(stcn:'RinkeHoekstra', foaf:'Person', G),
  rdfs_assert_label(stcn:'RinkeHoekstra', 'Rinke Hoekstra', nl, G),
  rdf_assert(stcn:'RinkeHoekstra', foaf:mbox, 'mailto:rinke.hoekstra@vu.nl', G),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'RinkeHoekstra', G),
  % Wouter
  rdf_assert_individual(stcn:'WouterBeek', foaf:'Person', G),
  rdfs_assert_label(stcn:'WouterBeek', 'Wouter Beek', nl, G),
  rdf_assert(stcn:'WouterBeek', foaf:mbox, 'mailto:me@wouterbeek.com', G),
  rdf_assert(stcn:'STCN', dcterms:contributor, stcn:'WouterBeek', G),
  
  % Creation time.
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_assert_datatype(stcn:'STCN', dcterms:created, XSD_DT, xsd:dateTime, G),
  
  % Title
  rdf_assert_language_tagged_string(stcn:'STCN', dcterms:title,
      'Short-Title Catalogue, Netherlands', en, G),
  
  % Description
  rdf_assert_language_tagged_string(stcn:'STCN', dcterms:description,
      'De STCN (Short Title Catalogue Netherlands) is een lijst beschrijvingen\c
       van voor 1 januari 1801 gedrukte Nederlandse boeken.', nl, G),
  
  % Vocabulary namespaces.
  rdf_current_namespace(stcn, STCN_NS),
  rdf_current_namespace(stcnv, STCNV_NS),
  rdf_assert(stcn:'STCN', void:vocabulary, STCN_NS, G),
  rdf_assert(stcn:'STCN', void:vocabulary, STCNV_NS, G),
  
  % VoID Feature: RDF serialization format.
  % @tbd Automate this based on rdf_save/3.
  rdf_assert(stcn:'STCN', void:feature, format:'Turtle', G),
  
  % SPARQL endpoint.
  rdf_assert(stcn:'STCN', void:sparqlEndpoint,
      'http://semanticweb.cs.vu.nl/prasem/sparql/', G),
  
  % Web pages.
  rdf_assert(stcn:'STCN', foaf:homepage,
      'http://www.kb.nl/kbhtml/stcnhandleiding/frames.html', G),
  rdf_assert(stcn:'STCN', foaf:page,
      'http://support.oclc.org/ggc/richtlijnen/', G),
  
  % Example resource
  % @tbd What is this?
  rdf_assert(stcn:'STCN', void:exampleResource, stcnv:'Publication', G),
  
  stcn_void_authors(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Authors', G),
  stcn_void_printers(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Printers', G),
  stcn_void_publications(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Publications', G),
  stcn_void_topics(G),
  rdf_assert(stcn:'STCN', void:'ClassPartition', stcn:'STCN_Topics', G),
  
  stcn_void_dbpedia(G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'DBpedia_STCN', G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'STCN2DBpedia', G),
  stcn_void_stcnv(G),
  rdf_assert(stcn:'STCN', void:subset, stcn:'STCNV', G),
  
  % @tbd Relate this to stcn:'STCN'.
  stcn_void_authors2dbpedia(G).

stcn_void_authors(G):-
  rdf_assert_individual(stcn:'STCN_Authors', void:'Dataset', G),
  % Title.
  rdf_assert_language_tagged_string(stcn:'STCN_Authors', dcterms:title,
      'STCN authors', en, G),
  % Description.
  rdf_assert_language_tagged_string(stcn:'STCN_Authors', dcterms:description,
    'The authors in the STCN dataset.', en, G),
  % Main class.
  rdf_assert(stcn:'STCN_Authors', void:class, stcnv:'Author', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Authors', void:dataDump, 'STCN_Authors.ttl', G).

stcn_void_authors2dbpedia(G):-
  rdf_assert_individual(stcn:'STCN_Authors2DBpedia', void:'Linkset', G),
  % Title.
  rdf_assert_language_tagged_string(stcn:'STCN_Authors2DBpedia',
      dcterms:title, 'STCN author, DBpedia entry mappings.', en, G),
  % Description.
  rdf_assert_language_tagged_string(stcn:'STCN_Authors2DBpedia',
      dcterms:description,
      'Identity mappings between STCN authors and DBpedia entries.', en, G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Authors2DBpedia', void:dataDump, 'STCN_Authors2DBpedia.ttl', G),
  % Link predicate.
  rdf_assert(stcn:'STCN_Authors2DBpedia', void:linkPredicate, owl:sameAs, G),
  % From target.
  rdf_assert(stcn:'STCN_Authors2DBpedia', void:target, stcn:'STCN_Authors', G),
  % To target,
  rdf_assert(stcn:'STCN_Authors2DBpedia', void:target, stcn:'DBpedia_STCN', G).

stcn_void_dbpedia(G):-
  % DBpedia
  rdf_assert_individual(stcn:'DBpedia_STCN', void:'Dataset', G),
  % Title
  rdf_assert_language_tagged_string(stcn:'DBpedia_STCN', dcterms:title,
      'DBpedia for STCN dataset', en, G),
  % Description
  rdf_assert_language_tagged_string(stcn:'DBpedia_STCN', dcterms:description,
      'The subset of the DBpedia that is relevant for the STCN dataset.', en,
      G),
  % Datadump
  rdf_assert(stcn:'DBpedia_STCN', void:dataDump, 'DBpedia_STCN.ttl', G),
  % VoID feature: RDF serialization format.
  rdf_assert(stcn:'DBpedia_STCN', void:feature, format:'Turtle', G),
  % Web page
  rdf_assert(stcn:'DBpedia_STCN', foaf:page, 'http://dbpedia.org/About', G).

stcn_void_printers(G):-
  rdf_assert_individual(stcn:'STCN_Printers', void:'Dataset', G),
  % Title.
  rdf_assert_language_tagged_string(stcn:'STCN_Printers', dcterms:title,
    'STCN printers, publishers and booksellers.', en, G),
  % Description.
  rdf_assert_language_tagged_string(stcn:'STCN_Printers', dcterms:description,
      'The printers, publishers and booksellers in the STCN dataset.', en, G),
  % Main class.
  rdf_assert(stcn:'STCN_Printers', void:class, stcnv:'Printer', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Printers', void:dataDump, 'STCN_Printers.ttl', G).

stcn_void_publications(G):-
  rdf_assert_individual(stcn:'STCN_Publications', void:'Dataset', G),
  rdf_assert_language_tagged_string(stcn:'STCN_Publications',
      dcterms:description, 'The publications in the STCN dataset.', en, G),
  rdf_assert_language_tagged_string(stcn:'STCN_Publications', dcterms:title,
      'STCN publications', en, G),
  % Main class
  rdf_assert(stcn:'STCN_Publications', void:class, stcnv:'Publication', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Publications', void:dataDump, 'STCN_Publications.ttl',
      G).

stcn_void_stcnv(G):-
  rdf_assert_individual(stcn:'STCNV', void:'Dataset', G),
  % Titles
  rdf_assert_language_tagged_string(stcn:'STCNV', dcterms:title,
      'STCN vocabulary', en, G),
  rdf_assert_language_tagged_string(stcn:'STCNV', dcterms:title,
      'STCN vocabularium', nl, G),
  % Descriptions
  rdf_assert_language_tagged_string(stcn:'STCNV', dcterms:description,
      'The vocabulary for the STCN dataset.', en, G),
  rdf_assert_language_tagged_string(stcn:'STCNV', dcterms:description,
    'Het vocabularium voor de STCN dataset.', nl, G),
  % Datadump location.
  rdf_assert(stcn:'STCNV', void:dataDump, 'STCNV.ttl', G).

stcn_void_topics(G):-
  rdf_assert_individual(stcn:'STCN_Topics', void:'Dataset', G),
  % Title
  rdf_language_tagged_string(stcn:'STCN_Topics', dcterms:title, 'STCN topics',
      en, G),
  % Description.
  rdf_assert_language_tagged_string(stcn:'STCN_Topics', dcterms:description,
      'The topics that occur in the STCN dataset.', en, G),
  % Main class.
  rdf_assert(stcn:'STCN_Topics', void:class, stcnv:'Topic', G),
  % Datadump location.
  rdf_assert(stcn:'STCN_Topics', void:dataDump, 'STCN_Topics.ttl', G).

