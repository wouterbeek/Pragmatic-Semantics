:- module(
  uri_scheme,
  [
    uri_scheme/1 % ?Scheme:atom
  ]
).

/** <module> URI Scheme

IANA-registered URI schemes.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(standards(iana_to_rdf)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- initialization(init_uri_scheme).

init_uri_scheme:-
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(read),file_errors(fail),extensions([ttl])]
  ), !,
  rdf_load(File, [format(turtle),graph(uri_scheme)]).
init_uri_scheme:-
  assert_iana(
    uri_scheme,
    'http://www.iana.org/assignments/uri-schemes/',
    iana:'URISchemaRegistration',
    ['uri-schemes-1','uri-schemes-2']
  ),
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(write),file_type(turtle)]
  ),
  rdf_save(File, [format(turtle),graph(uri_scheme)]),
  init_uri_scheme.



uri_scheme(Scheme):-
  rdfs_label(Registration, _, Scheme, uri_scheme),
  rdfs_individual_of(Registration, iana:'URISchemaRegistration').

