:- module(
  ap,
  [
    ap/3 % +Options:list(nvpair)
         % ?AP:iri
         % +AP_Stages:list(compound)
  ]
).
:- reexport(ap(ap_archive_ext)).
:- reexport(ap(ap_download)).
:- reexport(ap(ap_file_mime)).
:- reexport(ap(ap_file_size)).
:- reexport(ap(ap_rdf_serial)).
:- reexport(ap(ap_void_fetch)).
:- reexport(ap(ap_void_stat)).

/** <module> Auto-process

Support for running automated processing.

@tbd Implement to/2 option:
  * =|to(?ToFile:atom,?ToFileType:atom)|=
    Identifies the output from a script stage.
    The directory is not included since this is fixed to
    the process' output directory.
@tbd Add support for option =|finished(+Finished:boolean)|=,
     allowing previously finished processes to be skipped.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11, 2014/01-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(process)).
:- use_module(library(semweb/rdfs)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).



%! ap(+Options:list(nvpair), +AP:iri, +AP_Stages:list(compound)) is det.
% `Alias` is an existing path alias with prolog_file_path/2.
%
% The following options are supported:
%   * =|leave_trail(LeaveTrail:boolean)|=
%     Default `true`.
%   * =|reset(+Reset:boolean)|=
%     When true, removes the results of any prior executions of stages.
%     Default: `false`.

ap(O1, AP, AP_Stages):-
  var(AP), !,
  create_ap(_, AP),
  ap(O1, AP, AP_Stages).
ap(O1, AP, AP_Stages):-
  ap_begin(O1, AP),
  ap_stages(AP, AP_Stages),
  ap_end(O1, AP).


%! ap_begin(+Options:list(nvpair), +AP:iri) is det.

ap_begin(O1, AP):-
  rdf_string(AP, ap:alias, Alias, ap),

  % Process the reset option.
  % If `true` then any previous results are removed.
  option(reset(Reset), O1, false),
  (
    Reset == true,
    ap_directory(AP, Dir)
  ->
    process_create(path(rm), ['-r',file(Dir)], [])
    %delete_directory([include_self(true),safe(false)], Dir)
  ;
    true
  ),

  % Make sure the input directory is there.
  Spec1 =.. [Alias,input],
  create_nested_directory(Spec1),

  % Make sure the output directory is there.
  Spec2 =.. [Alias,output],
  create_nested_directory(Spec2),
  
  % DEB
  current_date_time(DateTime),
  debug(ap, 'Starting AP ~w at ~w.', [Alias,DateTime]).


%! ap_end(+Options:list(nvpair), +AP:iri) is det.
% End the script, saving the results to the output directory.

ap_end(O1, AP):-
  % The last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    ap_last_stage_directory(AP, LastStageDir)
  ->
    ap_directory(AP, write, output, OutputDir),
    link_directory_contents(LastStageDir, OutputDir)
  ;
    true
  ),

  (
    option(leave_trail(true), O1, true)
  ->
    true
  ;
    ap_clean(AP)
  ),

  rdf_collection(AP, AP_Stages, ap),
  maplist(ap_stage_duration, AP_Stages, Monthss, Secondss),
  maplist(sum_list, [Monthss,Secondss], [Months,Seconds]),
  rdf_assert_datatype(AP, ap:duration, duration(Months,Seconds), xsd:duration, ap),

  % Save the AP assertions until now.
  rdf_collection_member(AP, AP_Collection, ap),
  once(rdfs_label(AP_Collection, Label)),
  absolute_file_name(data(Label), File, [access(write),extensions([tmp])]),
  rdf_save([format(turtle)], ap, File),
  
  % DEB
  rdf_string(AP, ap:alias, Alias, ap),
  current_date_time(DateTime),
  debug(ap, 'Ended AP ~w at ~w.', [Alias,DateTime]).


%! ap_stage_duration(
%!   +ApStage:iri,
%!   +Months:nonneg,
%!   +Seconds:between(0.0,_)
%! ) is det.

ap_stage_duration(ApStage, Ms, Ss):-
  rdf_datatype(ApStage, ap:duration, duration(Ms,Ss), xsd:duration, ap), !.
ap_stage_duration(_, 0, 0.0).

