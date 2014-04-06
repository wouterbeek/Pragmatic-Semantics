% Project-specific debug file for the SWAG project.


% Debug topics.
:- use_module(library(debug)).
:- debug(swag).


:- use_module(rdf_file(rdf_serial)).
:- use_module(swag(sa_scrape)).
:- initialization(thread_create(init_swag, _, [])).
%init_swag:-
%  rdf_graph(swag), !.
%init_swag:-
%  absolute_file_name(
%    data(swag),
%    File,
%    [access(read),file_errors(fail),file_type(turtle)]
%  ), !,
%  rdf_load([format(turtle)], swag, File).
init_swag:-
  sa_scrape(swag),
  absolute_file_name(data(swag), File, [access(write),file_type(turtle)]),
  rdf_save([format(turtle)], swag, File).

:- use_module(swag(sa_clean)).

