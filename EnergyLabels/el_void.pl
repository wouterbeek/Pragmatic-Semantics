:- module(
  el_void,
  [
    assert_el_void/2 % +Graph:atom
                     % +BaseDirectory:atom
  ]
).

/** <module> EnergyLabels VoID

Asserts the VoID description of the energy labels dataset.

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(void(void_db)). % XML namespace.
:- use_module(void(void_stat)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_dateTime_ext)).

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- xml_register_namespace(el, 'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl/').
:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(formats, 'http://www.w3.org/ns/formats/').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').



assert_el_void(G, Dir):-
  rdf_global_id(el:'EnergyLabels', DD),

  %%%% dcterms:contributor

  % dcterms:created
  rdf_assert_now(DD, dcterms:created, G),

  % dcterms:creator
  assert_foaf_wouterbeek(G, WB),
  rdf_assert(DD, dcterms:creator, WB, G),

  % dcterms:date
  % A point or period of time associated with an event in the life-cycle
  % of the resource.
  %%%%rdf_assert_datatype(DD, dcterms:date, XSD_DT, xsd:date, G),

  % dcterms:description
  rdf_assert_language_tagged_string(DD, dcterms:description,
    'Ruwe data van afgegeven energielabels van gebouwen wordt in het kader\c
     van Apps voor Nederland ter beschikking gesteld. Per energielabel is de\c
     beschikbare informatie: gebouwinformatie; dit betreft de plaats,\c
     postcode, het huisnummer, eventuele huisnummertoevoeging en een vrij\c
     veld dat gebruikt kan worden voor extra gebouw identificatie met verder\c
     het woningtype of hoofdgebruiksfunctie; het certificaatnummer, de datum\c
     van opname en registratie door de adviseur, de labelwaarde\c
     (energie-index) en de labelklasse, bron van de opname; het berekende\c
     gebouwgebonden energieverbruik in MJ, en indien beschikbaar m3 gas,\c
     kWh elektrisch, MJ warmte en het aantal m2 van het gebouw.', nl, G),

  % dcterms:issued
  % Date of formal issuance (e.g., publication) of the dataset.
  %%%%rdf_assert_datatype(DD, dcterms:issued, XSD_DT, xsd:date, G),

  % dcterms:license
  rdf_assert(DD, dcterms:license,
    'http://www.opendatacommons.org/licenses/pddl/', G),

  % dcterms:modified
  % See module VOID_STAT.

  % dcterms:publisher
  rdf_assert(DD, dcterms:publisher, WB, G),

  % dcterms:source
  assert_energylabels_original_dataset(G, ODS),
  rdf_assert(DD, dcterms:source, ODS, G),

  % dcterms:title
  rdf_assert_language_tagged_string(DD, dcterms:title,
      'Energielabels - Agentschap NL', nl, G),

  % foaf:homepage
  rdf_assert(DD, foaf:homepage,
    'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl', G),

  % foaf:page
  rdf_assert(DD, foaf:page, 'https://github.com/wouterbeek/EnergyLabels', G),

  % rdf:type
  rdf_assert_individual(DD, void:'DatasetDescription', G),

  % rdfs:label
  rdfs_assert_label(DD, 'Energylabels', en, G),
  rdfs_assert_label(DD, 'Energielabels', nl, G),

  % void:datadump

  % void:documents
  rdf_assert_datatype(DD, void:documents, 10, xsd:integer, G),

  % void:exampleResource
  % @tbd A specific building.
  % @tbd A specific certificate.
  %%%%rdf_assert(DD, void:exampleResource, el:, G),

  % void:feature
  rdf_assert(DD, void:feature, formats:'Turtle', G),

  % void:openSearchDescription

  % void:rootResource
  rdf_assert(DD, void:rootResource, el:'Building', G),
  rdf_assert(DD, void:rootResource, el:'Certificate', G),

  % void:sparqlEndpoint

  % void:subject
  rdf_assert(DD, void:subject, dbpedia:'European_Union_energy_label', G),

  % void:subset
  forall(
    between(1, 9, N),
    assert_el_void_dataset(G, DD, N, Dir)
  ),

  % void:uriLookupEndpoint

  % void:uriRegexPattern

  % void:uriSpace
  rdf_assert(DD, void:uriSpace,
    'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl/', G),

  % void:vocabulary
  % @tbd Add `elv`
  %%%%xml_current_namespace(elv, ELV_NS),
  %%%%rdf_assert(DD, void:vocabulary, ELV_NS, G),
  xml_current_namespace(rdf, RDF_NS),
  rdf_assert(DD, void:vocabulary, RDF_NS, G),
  xml_current_namespace(rdfs, RDFS_NS),
  rdf_assert(DD, void:vocabulary, RDFS_NS, G),
  xml_current_namespace(xsd, XSD_NS),
  rdf_assert(DD, void:vocabulary, XSD_NS, G),

  % wv:declaration rdfs:Literal
  % wv:norms foaf:Document
  % wv:waiver foaf:Document

  true.

assert_el_void_dataset(G, DD, N, Dir):-
  format(atom(DS_Name), 'el_~w', [N]),
  rdf_global_id(el:DS_Name, DS),

  % dcterms:description.
  format(atom(Desc), 'Energylabel data for postcodes starting with ~w.', [N]),
  rdf_assert_language_tagged_string(DS, dcterms:description, Desc, en, G),

  % dcterms:title
  rdf_assert_language_tagged_string(DS, dcterms:title, DS_Name, en, G),

  % rdf:type
  rdf_assert_individual(DS, void:'Dataset', G),

  % void:class
  rdf_assert(DS, void:class, el:'Building', G),

  % void:dataDump
  absolute_file_name(
    DS_Name,
    DS_File,
    [access(read),file_type(turtle),relative_to(Dir)]
  ),
  relative_file_name(DS_File, Dir, DS_RelFile),
  rdf_assert(DS, void:dataDump, DS_RelFile, G),

  % void:subset
  rdf_assert(DD, void:subset, DS, G),
  void_assert_statistics(G, DS, DS_File).

assert_energylabels_original_dataset(G, ODS):-
  rdf_global_id(el:'OriginalDataset', ODS),
  assert_foaf_agentschapnl(G, ANL),
  rdf_assert(ODS, dcterms:creator, ANL, G),
  rdf_assert(ODS, dcterms:publisher, ANL, G).

assert_foaf_agentschapnl(G, ANL):-
  rdf_global_id(el:'AgentSchapNL', ANL),
  % foaf:homepage
  rdf_assert(ANL, foaf:homepage, 'http://www.agentschapnl.nl', G),
  % rdf:type
  rdf_assert_individual(ANL, foaf:'Organization', G),
  % rdfs:label
  rdfs_assert_label(ANL, 'Agentschap NL', nl, G).

assert_foaf_wouterbeek(G, WB):-
  rdf_global_id(el:'WouterBeek', WB),
  % foaf:firstName
  rdf_assert_string(WB, foaf:firstName, 'Wouter', G),
  % foaf:lastName
  rdf_assert_string(WB, foaf:lastName, 'Beek', G),
  % foaf:mbox
  rdf_assert(WB, foaf:mbox, 'mailto:me@wouterbeek.com', G),
  % rdf:type
  rdf_assert_individual(WB, foaf:'Person', G),
  % rdfs:label
  rdfs_assert_label(WB, 'Wouter Beek', nl, G).

