:- module(reset_deployment_script, []).

/** <module> Reset deployment

Script for auto-resetting a PGC-based server deployment.

@author Wouter Beek
@version 2013/12
*/

:- use_module(os(safe_file)).
:- use_module(server(password_db)).
:- use_module(server(user_db)).

:- initialization(reset_deployment).



reset_deployment:-
  % Reset the user database.
  reset_deployment_users,
  % Reset the passwords.
  reset_deployment_passwords.

reset_deployment_passwords:-
  password_db:password_db_file(File),
  safe_delete_file(File).

reset_deployment_users:-
  user_db:user_db_file(File),
  safe_delete_file(File).

