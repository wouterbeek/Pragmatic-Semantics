:- module(app_server, [start_app_server/0]).

/** <module> Application server

Using this module automatically starts the server.

This produces the home page for the development server.

@author Wouter Beek
@see http://semanticweb.cs.vu.nl/prasem/
@version 2013/11-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(server(server_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

% Define the default application server port in agreement with ClioPatria.

:- setting(
  http:port,
  nonneg,
  env('PORT',5000),
  'Port the http server listens to'
).

% If you login, the system will redirect  you to its public address. I.e.,
% if you connected to `http://localhost:5000/` it will redirect you to
% `http://my.domain.org:500/`. This can be undesirable on e.g., a notebook
% that is not always connected to the internet and/or may change address
% and/or may be behind a firewall. You can disable redirection using the
% settings below. These settings may also be necessary  if the server is
% behind a proxy.
%
% @author Jan Wielemaker
% @see ClioPatria
% @version 2013/12

:- if(predicate_property(user:debug_mode, visible)).
  :- set_setting_default(http:public_host, localhost).
  :- set_setting_default(http:public_port, setting(http:port)).
:- endif.

:- initialization(start_app_server).



%! start_app_server is det.
% Starts an application server.

% Start the application server when running on dotcloud.

% dotCloud defines the `PORT_WWW` environment variable.
% @see http://docs.dotcloud.com/services/custom/
start_app_server:-
  getenv('PORT_WWW', PortAtom), !,
  atom_number(PortAtom, Port),
  start_server(Port, http_dispatch).
% Start the application server using the default port
%  taken from settings.
start_app_server:-
  setting(http:port, Port),
  start_app_server(Port).

%! start_app_server(?Port:between(1000,9999)) is det.
% Start the application server on the given port.

start_app_server(Port):-
  (
    absolute_file_name(
      project(settings),
      File,
      [access(read),file_errors(fail),file_type(database)]
    )
  ->
    load_settings(File)
  ;
    true
  ),
  %thread_pool_create(cheapthreads, 90, []),
  start_server(Port, app_server_dispatch).

%! app_server_dispatch(+Request:list) is det.
% A wrapper predicate whose sole purpose is to be a handle for
%  trace statements.

app_server_dispatch(Request):-
  http_dispatch(Request).

