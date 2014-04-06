:- module(
  iotw_niod,
  [
    iotw_niod/1 % +Options:list(nvpair)
  ]
).

/** <module> IOTW experiment with the Verenigd Koninkrijk dataset.

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(xpath)).
:- use_module(os(dir_ext)).
:- use_module(void(void_file)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(top_messages(+,r)).

:- xml_register_namespace('category-nl', 'http://nl.dbpedia.org/resource/').
:- xml_register_namespace('prop-nl', 'http://nl.dbpedia.org/property/').
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- xml_register_namespace('dbpedia-owl', 'http://dbpedia.org/ontology/').
:- xml_register_namespace(niod, 'http://purl.org/collections/nl/niod/').
:- xml_register_namespace(schema, 'http://schema.org/').
:- xml_register_namespace(vk, 'http://www.wouterbeek.com/vk.owl#').



%! iotw_niod(+Options:list(nvpair)) is det.
% The following options are supported:
%   * =|data_status(+Status:oneof([fresh,stale]))|=
%     The data is either copied from the Dropbox to the experiment
%     directory (`fresh`), or the already stored version in the experiment
%     directory is used (if available).
%
% @tbd Add directory existence check as precondition for `stale`.

iotw_niod(O):-
  (
    option(data_status(fresh), O, stale)
  ->
    % Make a safe copy of the data to experiment with.
    absolute_file_name(
      home('Dropbox/VK/RDF'),
      FromDir,
      [access(read),file_type(directory)]
    ),
    safe_copy_experiment_data(FromDir, ToDir),
    absolute_file_name(
      void,
      VoID_File,
      [access(read),file_type('text/turtle'),relative_to(ToDir)]
    )
  ;
    experiment_directory(ExperimentDir),
    absolute_file_name(
      'home/wbeek/Dropbox/VK/RDF/void',
      VoID_File,
      [access(read),file_type(turtle),relative_to(ExperimentDir)]
    )
  ),
  
  % Load the entire dataset by loading the VoID file.
  void_load(VoID_File, _, VoID_Graph),
  
  % Save statistics that were added/modified while loading.
  void_save(VoID_Graph, VoID_File),
  
  debug(iotw_niod, 'VoID graph ~w is loaded.', [VoID_Graph]).

