:- module(
  mime_url,
  [
    mime_url/0
  ]
).

/** <module> MIME URL

Generates a CSV file with the following columns:
  - URL (not always syntactically conformant)
  - Format (according to CKAN metadata)
  - MIME (according to CKAN metadata)

I have written this script to create a file for JW.

@author Wouter Beek
@version 2013/03
*/

:- use_module(library(csv)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf(rdf_meta)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(rdf_term(rdf_string)).



mime_url:-
  absolute_file_name(
    data(datahub_io),
    CKAN_File,
    [access(read),file_type(turtle)]
  ),
  mime_url(CKAN_File).


mime_url(CKAN_File):-
  rdf_setup_call_cleanup(
    [format(turtle)],
    CKAN_File,
    mime_url_rows(Rows)
  ),
  file_alternative(CKAN_File, _, mime_url, csv, CSV_File),
  csv_write_file(CSV_File, Rows).


mime_url_rows(Rows, Graph):-
  findall(
    row(URL,Format,MIME),
    (
      rdf_string(Resource, ckan:url, URL, Graph),
      (
        rdf_string(Resource, ckan:format, Format, Graph)
      ->
        true
      ;
        Format = null
      ),
      (
        rdf_string(Resource, ckan:mimetype, MIME, Graph)
      ->
        true
      ;
        MIME = null
      )
    ),
    Rows
  ).

