:- module(semuri_script, []).

/** <module> Semantic URIs script

Automated script that run the experiments for the project on Semantic URIs.

@author Wouter Beek
@version 2014/01
*/

:- use_module(ckan(datahub_io)).
%:- use_module(ckan(data_gov_uk)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_serial)).

:- initialization(thread_create(semuri_script, _, [])).

:- debug(ckan).



semuri_script:-
  % Prepare.
  (
    absolute_file_name(
      data(ckan),
      File,
      [access(read),file_errors(fail),file_type(turtle)]
    )
  ->
    rdf_load2(File, [format(turtle),graph(ckan)])
  ;
    ckan_to_rdf
  ),
  
  % Collect datasets.
  setoff(
    Package,
    rdfs_individual_of(Package, ckan:'Package'),
    Packages
  ),
  length(Packages, NumberOfPackages),
  debug(
    semuri,
    'Now about to run the experiment on ~d datasets.',
    [NumberOfPackages]
  ),
  
  maplist(semuri_script, Packages).

semuri_script(_Package):-
  % Load canonical XSD
  % OWL materialize (Jena JAR)
  % Steven (JAR)
  % Table output HTML
  true.

