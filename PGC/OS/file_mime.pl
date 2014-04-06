:- module(
  file_mime,
  [
    file_mime/2, % +File:atom
                 % -MIME:atom
    xml_declaration//1 % ?Version:float
  ]
).

/** <module> File mime

Returns the MIME of a given file.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)). % Meta-DCG
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_module(library(debug)).
:- use_module(library(pure_input)).



file_mime(File, _):-
  \+ access_file(File, read), !,
  debug(file_mime, 'Cannot read from file ~w.', [File]),
  fail.
file_mime(File, MIME):-
  phrase_from_file(file_mime(MIME), File), !.
file_mime(File, _):-
  debug(file_mime, 'Failed to identify MIME type of file ~w.', [File]),
  fail.


file_mime('application/x-turtle') -->
  ci_string(`@prefix`), !,
  dcg_done.
file_mime('text/html') -->
  atom('<!'), ci_string(`DOCTYPE`), ascii_whites,
  ci_string(`HTML`), !,
  dcg_done.
file_mime(MIME) -->
  ascii_whites,
  (xml_declaration(_) ; ""), ascii_whites,
  xml_comments,

  (
    atom('<rdf:RDF')
  ->
    {MIME = 'application/rdf+xml'}
  ;
    xml_doctype(MIME)
  ),
  dcg_done.


utf8 -->
  ci_string(`UTF`), `-8`.


xml_comment -->
  atom('<!--'),
  dcg_until([end_mode(inclusive)], `-->`, _),
  ascii_whites.


xml_comments -->
  xml_comment,
  ascii_whites,
  xml_comments.
xml_comments --> [].


%! xml_declaration(?Version:float)// .
% The XML specification also permits an XML declaration at
%  the top of the document with the XML version and possibly
%  the XML content encoding. This is optional but recommended.

xml_declaration(Version) -->
  atom('<?'), ci_string(`XML`), ascii_whites,
  (xml_version(Version), ascii_whites ; ""),
  (xml_encoding, ascii_whites ; ""),
  atom('?>'), ascii_whites, !.


xml_doctype('application/rdf+xml') -->
  atom('<!'), ci_string(`DOCTYPE`), ascii_whites, atom('rdf:RDF'), !,
  dcg_done.


xml_encoding -->
  atom('encoding='),
  quoted(utf8).


xml_version(Version) -->
  atom('version='),
  quoted(float(Version)).

