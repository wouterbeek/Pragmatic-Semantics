:- module(
  ckan_file_size,
  [
    ckan_file_size/0
  ]
).

/** <module> CKAN file size

Web-based overview of CKAN datasets sorted by size.

@author Wouter Beek
@version 2014/02
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_table)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

http:location(ckan, root(ckan), []).
:- http_handler(ckan(file_size), ckan_file_size, []).

user:web_module('CKAN FileSize', ckan_file_size).



ckan_file_size(_Request):-
  ckan_file_size_rows(Rows),
  reply_html_page(
    app_style,
    title('CKAN - Datasets sorted by file size'),
    \rdf_html_table(
      [],
      html('Datasets sorted by file size'),
      [['File size in Gb','Dataset']|Rows]
    )
  ).


%! ckan_file_size is det.
% Prints the file sizes for the inspected datasets to the terminal.
% This can be used in case there is no Web browser.

ckan_file_size:-
  ckan_file_size_rows(Rows),
  dcg_with_output_to(
    user_output,
    dcg_table(
      [header_row(true)],
      html('Datasets sorted by file size'),
      rdf_term_name,
      [['File size in Gb','Dataset']|Rows]
    )
  ).


ckan_file_size_rows(Rows):-
  findall(
    FileSize-Resource,
    rdf_datatype(Resource, ap:file_size, FileSize, xsd:integer, _),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  
  pairs_keys(Pairs3, FileSizes),
  sum_list(FileSizes, FileSizeSum),
  
  findall(
    [FileSizeGb,Resource],
    (
      member(FileSize-Resource, [FileSizeSum-'All files'|Pairs3]),
      FileSizeGb is FileSize / 1024 / 1024 / 1024
    ),
    Rows
  ).

