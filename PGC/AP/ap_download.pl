:- module(
  ap_download,
  [
    ap_download_to_directory/4 % +ApStage:iri
                               % +ToDirectory:atom
                               % +URL:url
                               % +Accept:atom
  ]
).

/** <module> AP Download

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(uri(uri_scheme)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc2616, 'http://tools.ietf.org/html/rfc2616#').



ap_download_to_directory(ApStage, ToDir, URL, Accept):-
  check_url_validity(URL),
  uri_components(URL, uri_components(_,_,Path,_,_)),
  atomic_list_concat(Components1, '/', Path),
  reverse(Components1, Components2),
  first_nonempty_atom(Components2, FileName),
  absolute_file_name(FileName, File, [access(write),relative_to(ToDir)]),
  (
    Accept == ''
  ->
    O1 = []
  ;
    O1 = [request_header('Accept'=Accept)]
  ),
  download_to_file(
    [header('Content-Type',ContentType)|O1],
    URL,
    File
  ),
  ap_stage_resource(ApStage, Resource, Graph),
  rdf_assert_string(Resource, rfc2616:'Content-Type', ContentType, Graph),
  directory_files(
    [include_directories(false),include_self(false)],
    ToDir,
    ToFiles
  ),
  forall(
    member(File, ToFiles),
    add_operation_on_file(ApStage, File, downloaded, [])
  ).

first_nonempty_atom([], dummy):- !.
first_nonempty_atom([''|T], Atom):- !,
  first_nonempty_atom(T, Atom).
first_nonempty_atom([H|_], H).

check_url_validity(URL):-
  uri_components(URL, uri_components(Scheme, _, _, _, _)),
  uri_scheme(Scheme), !.
check_url_validity(URL):-
  domain_error('URL', URL).

