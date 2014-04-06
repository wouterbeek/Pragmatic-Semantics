:- module(iotw_rdft, []).

/** <module> IOTW RDFT experiment

Runs the RDFT experiment within the IOTW project.

@author Wouter Beek
@version 2013/12
*/

:- use_module(ap(ap)).
:- use_module(os(archive_ext)).
:- use_module(owl(owl_mat)).
:- use_module(rdf(rdf_serial_conv)).
:- use_module(standards(oaei)).

:- initialization(iotw_rdft).



iotw_rdft:-
  ap(
    [process(rdft),project(iotw)],
    [
      ap_stage([from(input,'RDFT',archive)], extract_archive),
      ap_stage([], tsv_convert_directory),
      ap_stage([], ap_rdf_convert_directory),
      ap_stage([], owl_materialize),
      ap_stage([], ap_rdf_convert_directory),
      ap_stage([between(1,5),to(output)], rdft_experiment)
    ]
  ).

rdft_experiment(StageAlias, FromDir, ToDir, N):-
  true.

