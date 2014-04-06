:- module(
  iotw_pleiades,
  [
    iotw_pleiades/1 % +Options:list(nvpair)
  ]
).

/** <module> IOTW experiment with the Pleiades dataset.

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(xpath)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_deb)). % Registered as a Web module.
:- use_module(server(web_console)).
:- use_module(vocabularies(void)).
:- use_module(xml(xml_namespace)).

:- register_module(rdf_deb).

:- rdf_meta(top_messages(+,r)).

:- xml_register_namespace(cito, 'http://purl.org/spar/cito/').
:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(geo, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
:- xml_register_namespace(osgeo, 'http://data.ordnancesurvey.co.uk/ontology/geometry/').
:- xml_register_namespace(osspatial, 'http://data.ordnancesurvey.co.uk/ontology/spatialrelations/').
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- xml_register_namespace(pleiades, 'http://pleiades.stoa.org/places/vocab#').
:- xml_register_namespace(prov, 'http://www.w3.org/TR/prov-o/#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').
:- xml_register_namespace(spatial, 'http://geovocab.org/spatial#').

:- xml_register_namespace('pl-errata', 'http://pleiades.stoa.org/errata/').
:- xml_register_namespace('pl-place', 'http://pleiades.stoa.org/places/').



iotw_pleiades(O1):-
  (
    option(data_status(fresh), O1, stale)
  ->

    % Make a safe copy of the data to experiment with.
    absolute_file_name(
      home('Dropbox/pleiades'),
      FromDir,
      [access(read),file_type(directory)]
    ),
    safe_copy_experiment_data(FromDir, ToDir),
    absolute_file_name(
      void,
      VoID_File,
      [access(read),file_type(turtle),relative_to(ToDir)]
    )
  ;
    experiment_directory(ExperimentDir),
    absolute_file_name(
      home('Dropbox/pleiades/void'),
      VoID_File,
      [access(write),file_type(turtle),relative_to(ExperimentDir)]
    )
  ),

  % Load the entire dataset by loading the VoID file.
  void_load(VoID_File, _, VoID_Graph),

  void_save(VoID_Graph, VoID_File).

