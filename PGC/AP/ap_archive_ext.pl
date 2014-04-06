:- module(
  ap_archive_ext,
  [
    extract_archives/3 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % +ApStage:iri
  ]
).

/** <module> AP: extract archive

Archive extraction process for the AP architecture.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(os(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(library(apply)).
:- use_module(rdf(rdf_build)).



%! extact_archives(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   +ApStage:iri
%! ) is det.

extract_archives(FromDir, ToDir, ApStage):-
  directory_files([recursive(false)], FromDir, FromFiles),
  include(extract_archive0(ApStage), FromFiles, ConvertedFiles),
  link_directory_contents(FromDir, ToDir),
  (
    ConvertedFiles == []
  ->
    add_skip(ApStage)
  ;
    true
  ).

extract_archive0(ApStage, FromFile):-
  extract_archive(FromFile, Conversions),
  Conversions \== [],
  add_operation_on_file(
    ApStage,
    FromFile,
    'archive extraction',
    Conversions
  ).

