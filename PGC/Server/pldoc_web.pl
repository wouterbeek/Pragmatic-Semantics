:- module(pldoc_web, []).

/** <module> plDoc handler

Handle requests to view plDoc via a Web interface.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(library(doc_http)).
:- use_module(library(http/http_dispatch)).
:- use_module(server(web_modules)).

:- http_handler(root(help), pldoc_web, []).

http:location(help, root(help), [priority(10)]).
:- http_handler(help(.), pldoc_web, []).

user:web_module('plDoc', pldoc_web).



pldoc_web(Request):-
  http_location_by_id(pldoc_doc, Location),
  absolute_file_name(
    project('README'),
    File,
    [access(read),file_type('text/markdown')]
  ),
  atomic_concat(Location, File, StartPage),
  http_redirect(moved, StartPage, Request).

