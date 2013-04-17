:- module(
  prasem,
  [
    deregister_server/1, % +Name:atom
    register_server/2, % +Name:atom
                       % +Port:integer
    running_server/2, % ?Name:atom
                      % ?Port:integer
    start_prasem/0,
    start_wallace/0
  ]
).

/** <module> PraSem

The Prasem project module. This should tie the various services that
are running under the header of 'PraSem' in a uniform manner.

@author Wouter Beek
@version 2012/08-2012/09, 2013/03
*/

:- use_module(generic(logging)).
:- use_module(generic(os_ext)).
:- use_module(server(server)).
:- use_module(server(wallace)).

:- at_halt(quit_prasem).

:- dynamic(running_server(_Name, _Port)).



deregister_server(Name):-
  retract(running_server(Name, Port)),
  !,
  stop_server(Port).
deregister_server(Name):-
  print_message(error, no_registered_server(Name)).
prolog:message(no_registered_server(Name)) -->
  [
    ansi([], 'No registered server called "', []),
    ansi([bg(yellow)], '~w', [Name]),
    ansi([], '".', [])
  ].

%% quit_prasem is det.
% This is performed upon quitting PraSem.

quit_prasem:-
  end_log.

register_server(Name, Port):-
  running_server(Name, Port),
  !,
  print_message(error, registered_server_exists(Name)).
register_server(Name, Port):-
  assert(running_server(Name, Port)),
  start_server(Port).
prolog:message(registered_server_exists(Name)) -->
  [
    ansi([], 'Already a registered server called "', []),
    ansi([bg(yellow)], '~w', [Name]),
    ansi([], '".', [])
  ].

%% start_prasem is det.
% Starts the PraSem logging, database, and Web interface.

start_prasem:-
  % Start generic logging.
  start_log,
  (
    wallace(on)
  *->
    % Start the Wallace webserver.
    start_wallace
  ;
    true
  ).

start_wallace:-
  register_server('Wallace', 5000).

