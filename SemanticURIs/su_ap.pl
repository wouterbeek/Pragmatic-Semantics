:- module(
  su_ap,
  [
    su_ap/0
  ]
).

/** <module> Semantic URIs Automated Processes

Automated processes for semantic URIs.

@author Wouter Beek
@tbd Load canonical XSD
@tbd OWL materialize (Jena JAR)
@version 2014/01-2014/02
*/

:- use_module(ap(ap_table)). % Debug tool.
:- use_module(ap(ap_file_size)). % AP stage.
:- use_module(ap(ap_rdf_serial)). % AP stage.
:- use_module(ap(ap_void_stat)). % AP stage.
:- use_module(ckan(ckan_ap)).
:- use_module(ckan(ckan_table)). % Debug tool.
:- use_module(su(rdf_compress)). % AP stages.
:- use_module(su(entropy_and_mutual_information)).



su_ap:-
  ckan_ap(
    [
      su_ap:ap_stage(
        [name('toTurtle'),args(['application/x-turtle'])],
        ap_rdf_convert_directory
      ),
      su_ap:ap_stage([name('VoID')], void_statistics),
      su_ap:ap_stage([name('Compress')], rdf_compress),
      su_ap:ap_stage([name('Entropies')], entropy_and_mutual_information)
    ]
  ).

