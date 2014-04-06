:- module(
  server_ext,
  [
    dispatch/1, % +Request:list
    http_method/2, % +Request:list
                   % -Method:oneof([delete,get,post])
    server_rebase/1, % +Prefix:atom
    start_server/1, % +Port:between(1000,9999)
    start_server/2 % +Port:between(1000,9999)
                   % :ServerGoal
  ]
).

/** <module> Server extensions

Extensions for SWI-Prolog servers.

Dispatching non-login methods goes through this module,
which checks whether the user has authorization.

# HTTP locations

SWI-Prolog defines the following HTTP locations:

| *Abbreviation*   | *Path*               |
| =css=            | =|root(css)|=        |
| =icons=          | =|root(icons)|=      |
| =js=             | =|root(js)|=         |
| =pldoc=          | =|root(.)|=          |
| =pldoc_man=      | =|pldoc(refman)|=    |
| =pldoc_pkg=      | =|pldoc(package)|=   |
| =pldoc_resource= | =|/debug/help/res/|= |
| =root=           | =|/|=                |

# HTTP handlers

SWI-Prolog defines the following HTTP handlers:

| *Spec*                  | *Handler*                                             |
| =|css(.)|=              | =|http_server_files:serve_files_in_directory(css)|=   |
| =|icons(.)|=            | =|http_server_files:serve_files_in_directory(icons)|= |
| =|js(.)|=               | =|http_server_files:serve_files_in_directory(js)|=    |
| =|pldoc(.)|=            | =|pldoc_root|=                                        |
| =|pldoc(doc)|=          | |
| =|pldoc(doc_for)|=      | |
| =|pldoc(edit)|=         | |
| =|pldoc(file)|=         | |
| =|pldoc('index.html')|= | |
| =|pldoc(man)|=          | |
| =|pldoc(pack)|=         | |
| =|pldoc('pack/')|=      | |
| =|pldoc(place)|=        | |
| =|pldoc('res/')|=       | |
| =|pldoc(search)|=       | |
| =|pldoc_man(.)|=        | |
| =|pldoc_pkg(.)|=        | |

@author Wouter Beek
@version 2013/10-2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)). % Session support.
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).
:- use_module(server(web_error)).
:- use_module(server(authorization)).

:- meta_predicate(dispatch(:)).

:- meta_predicate(start_server(+,:)).
:- meta_predicate(start_server_on_next_port(+,+,:,-)).

:- multifile(prolog:message//1).



%! dispatch(:Request:list)
% Dispatching a request means:
%   1. Extracting the HTTP method of the request.
%   2. Checking whether the current user is authorized to perform the request.
%   3. Dispatching the request-method pair using the given module.

dispatch(Module:Request):-
  http_method(Request, Method),
  authorized(Method, Request, User),
  dispatch_method(Module, Method, Request, User).

%! dispatch_method(
%!   +Module:atom,
%!   +Method:oneof([delete,get,send]),
%!   +Request:list,
%!   +User:atom
%! ) is det.

dispatch_method(Module, Method, Request, User):-
  catch(
    Module:dispatch_method(Method, Request, User),
    Error,
    reply_error(Error)
  ), !.
% The dispatcher for the given method is undefined.
dispatch_method(Module, Method, _Request, _User):-
  PlainError = 'Undefined HTTP method.',
  format(
    atom(Msg),
    'Method ~w is not defined by module ~w.',
    [Method,Module]
  ),
  reply_json(json([error=PlainError,message=Msg]), [width(0)]).


%! http_method(+Request:list, -Method:oneof([delete,get,post])) is det.
% Returns the HTTP method used in the given request.

http_method(Request, Method):-
  memberchk(method(Method), Request).


%! server_port(Port:between(1000,9999)) is semidet.
% Type checking for server ports.

server_port(Port):-
  between(1000, 9999, Port).


%! server_rebase(+Prefix:atom) is det.
% Rebase the entire Web application.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/http.html%27%29

server_rebase(Prefix):-
  set_setting(http:prefix, Prefix).


%! start_server(+Port:between(1000,9999)) is det.
%! start_server(+Port:between(1000,9999), :ServerGoal) is det.

start_server(Port):-
  start_server(Port, _ServerGoal).
% A server is already running at the given port.
start_server(Port, _ServerGoal):-
  http_server_property(Port, start_time(StartTime)), !,
  debug(
    server_ext,
    'The server at port ~w is used as the application server \c
     (start time ~w).',
    [Port,StartTime]
  ).
% No server is running yet, so start a server.
start_server(Port, ServerGoal):-
  % Estimate the number of workes based on the number of CPU cores.
  current_prolog_flag(cpu_count, NumberOfCores),
  NumberOfWorkers is NumberOfCores * 2,

  % Allow a custom goal for server dispatching.
  default(http_dispatch, ServerGoal),

  start_server_on_next_port(Port, NumberOfWorkers, ServerGoal, PortUsed),

  % Make sure the server is shut down whenever SWI-Prolog shuts down.
  at_halt(http_stop_server(PortUsed, [])),

  % INFO
  print_message(informational, server_ext(started(PortUsed))).
prolog:message(server_ext(started(Port))) -->
  {setting(http:prefix, Prefix)},
  ['You can access the server at http://localhost:~w/~w'-[Port,Prefix]].


%! start_server_on_next_port(
%!   +Port:between(1000,9999),
%!   +NumberOfWorkers:positive_integer,
%!   :ServerGoal,
%!   -PortUsed:between(1000,9999)
%! ) is det.
% Keeps incresing the given port number until a free port number is found;
% then starts a new server at that port.

% Increment port numbers until a free one is found.
start_server_on_next_port(Port, NumberOfWorkers, ServerGoal, PortUsed):-
  server_port(Port), !,
  catch(
    (
      http_server(ServerGoal, [port(Port),workers(NumberOfWorkers)]),
      PortUsed = Port
    ),
    error(socket_error(_Msg), _),
    (
      NextPort is Port + 1,
      start_server_on_next_port(
        NextPort,
        NumberOfWorkers,
        ServerGoal,
        PortUsed
      )
    )
  ).
% At the end of the port numeber list we start over
% by trying out the lowest port number.
start_server_on_next_port(_Port, NumberOfWorkers, ServerGoal, PortUsed):-
  start_server_on_next_port(1000, NumberOfWorkers, ServerGoal, PortUsed).

