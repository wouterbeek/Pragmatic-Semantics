:- module(
  ap_dir,
  [
    ap_clean/1, % +AP:iri
    ap_directory/2, % +AP:iri
                    % -Directory:atom
    ap_directory/4, % +AP:iri
                    % +Mode:oneof([read,write])
                    % +Subdir:atom
                    % -AbsoluteDir:atom
    ap_stage_directories/2, % +AP:iri
                      % -Directories:list(atom)
    ap_last_stage_directory/2, % +AP:iri
                               % -LastStageDirectory:atom
    ap_stage_directory/3, % +ApStage:iri
                          % +Mode:oneof([read,write])
                          % -AbsoluteDir:atom
    ap_stage_directory_name/2 % +StageNumber:nonneg
                              % -StageName:atom
  ]
).

/** <module> Auto-processing directories

Directory management for running automated processes.

@author Wouter Beek
@version 2013/11, 2014/01-2014/03
*/

:- use_module(generics(codes_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(semweb/rdfs)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').



%! ap_clean(+AP:iri) is det.
% This is run after results have been saved to the `Output` directory.

ap_clean(AP):-
  ap_directory(AP, Dir),
  process_create(path(rm), ['-r',file(Dir)], []).


%! ap_directory(+AP:iri, -Directory:atom) is det.
% Retrieves the main directory of the given automated process.

ap_directory(AP, Dir):-
  once(rdfs_individual_of(AP, ap:'AP')),
  rdf_string(AP, ap:alias, Alias, ap),
  file_search_path(Alias, Spec),
  absolute_file_name(
    Spec,
    Dir,
    [access(read),file_errors(fail),file_type(directory)]
  ).


%! ap_directory(
%!   +AP:iri,
%!   +Mode:oneof([read,write]),
%!   +Subdir:atom,
%!   -AbsoluteDir:atom
%! ) is det.
% Find subdirectories of the given automated process.

ap_directory(AP, Mode, Subdir, AbsoluteDir):-
  once(rdfs_individual_of(AP, ap:'AP')),
  rdf_string(AP, ap:alias, Alias, ap),
  Spec =.. [Alias,Subdir],
  (
    absolute_file_name(
      Spec,
      AbsoluteDir,
      [access(Mode),file_errors(fail),file_type(directory)]
    ), !
  ;
    Mode = write,
    % If the AP subdirectory is not found and the mode is `write`,
    % then we create it.
    create_nested_directory(Spec, AbsoluteDir)
  ).


%! ap_stage_directories(+AP:iri, -StageDirectories:list(atom)) is det.

ap_stage_directories(AP, Dirs):-
  ap_stage_directories(AP, 1, Dirs).

ap_stage_directories(AP, Stage1, [H|T]):-
  ap_stage_directory_name(Stage1, Stage1Name),
  ap_directory(AP, read, Stage1Name, H), !,
  Stage2 is Stage1 + 1,
  ap_stage_directories(AP, Stage2, T).
ap_stage_directories(_, _, []).


%! ap_last_stage_directory(+AP:iri, -LastStageDirectory:atom) is semidet.
% Returns the last stage directory, if it exists.

ap_last_stage_directory(AP, LastStageDir):-
  ap_stage_directories(AP, StageDirs),
  StageDirs \== [],
  last(StageDirs, LastStageDir).


ap_stage_directory(ApStage, Mode, AbsoluteDir):-
  rdfs_individual_of(ApStage, ap:'AP-Stage'), !,
  rdf_collection_member(ApStage, AP, ap),
  rdf_datatype(ApStage, ap:stage, StageNum, xsd:integer, ap),
  ap_stage_directory_name(StageNum, StageName),
  ap_directory(AP, Mode, StageName, AbsoluteDir).


%! ap_stage_directory_name(+StageNumber:integer, -StageName:atom) is det.
% Returns the stage name that corresponds to the given indicator.
%
% @arg StageIndicator One of the following:
%   * `0` is converted to name `input`.
%   * Positive integers `N` are converted to names `stage_N`.
%   * Other names are unchanged.
% @arg StageName An atomic name.

ap_stage_directory_name(0, input):- !.
ap_stage_directory_name(StageNum, StageName):-
  must_be(positive_integer, StageNum), !,
  format(atom(StageName), 'stage~w', [StageNum]).
ap_stage_directory_name(StageName, StageName).

