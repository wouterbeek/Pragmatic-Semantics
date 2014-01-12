:- module(semuri_script, []).

/** <module> Semantic URIs script

Automated script that run the experiments for the project on Semantic URIs.

@author Wouter Beek
@version 2014/01
*/

:- use_module(ckan(datahub_io)).

:- intialization(semuri_script).



semuri_script:-
  ckan_to_rdf,
  
  % Packages open RDF
  % Load canonical XSD
  % OWL materialize (Jena JAR)
  % Steven (JAR)
  % Table output HTML
  
  true.

