:- module(
  ap_file_mime,
  [
    mime_dir/3 % +FromDirectory:atom
               % +ToDirectory:atom
               % +ApStage:iri
  ]
).

/** <module> AP file MIME

File MIME type identification for the AP architecture.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(ckan(ckan_db)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_mime)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).



%! mime_dir(+FromDirectory:atom, +ToDirectory:atom, +ApStage:iri) is det.

mime_dir(FromDir, ToDir, ApStage):-
  directory_files([], FromDir, FromFiles),
  (
    FromFiles == []
  ->
    existence_error('File', 'No files')
  ;
    maplist(mime_file(ApStage), FromFiles)
  ),
  link_directory_contents(FromDir, ToDir).


mime_file(ApStage, File):-
  ap_stage_resource(ApStage, Resource, Graph),
  rdf_string(Resource, ckan:mimetype, MIME1, Graph),
  (
    file_mime(File, MIME2)
  ->
    (MIME1 \== MIME2 -> debug(ap, '[MIME] ~w -> ~w', [MIME1,MIME2]) ; true)
  ;
    MIME2 = MIME1
  ),
  add_properties_of_file(ApStage, File, ['MIME'-MIME2]),
  rdf_assert_string(Resource, ap:mime, MIME2, Graph).

