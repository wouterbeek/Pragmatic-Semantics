:- module(
  ap_void_fetch,
  [
    void_fetch/3 % +FromDirectory:atom
                 % +ToDirectory:atom
                 % +AP_Status:compound
  ]
).

/** <module> AP VoID fetch

Fetch datasets described in VoID (if any).
For use in the AP architecture.

@author Wouter Beek
@version 2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(library(apply)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_dataset)).
:- use_module(rdf(rdf_package)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(void(void_db)).



void_fetch(FromDir, ToDir, ApStage):-
  rdf_directory_files(FromDir, FromFiles),
  maplist(void_fetch_file(ApStage), FromFiles, RdfDatasets1),
  exclude(var, RdfDatasets1, RdfDatasets2),
  (RdfDatasets2 == [] -> add_skip(ApStage) ; true),
  link_directory_contents(FromDir, ToDir).


void_fetch_file(ApStage, FromFile, RdfDataset):-
  rdf_load([void(true)], Graph, FromFile),
  (
    void_dataset(Graph, RdfDataset)
  ->
    atomic_concat(FromFile, '.tar', ToFile),
    void_package_build([compress(false)], Graph, ToFile),
    delete_file(FromFile),
    rdf_dataset(RdfDataset, DefaultGraph, NamedGraphs),
    add_operation_on_file(
      ApStage,
      FromFile,
      'fetched VoID',
      [DefaultGraph|NamedGraphs]
    )
  ;
    true
  ).

