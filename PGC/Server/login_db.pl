:- module(
  login_db,
  [
    logged_in/1, % ?UserName:atom
    logged_in/3, % ?Session:atom
                 % ?UserName:atom
                 % ?LoginTime:float
    login/1, % +UserName:atom
    logout/1 % +UserName:atom
  ]
).

/** <module> Login database

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_session)).
:- use_module(library(persistency)). % Declarations
:- use_remote_module(os(file_ext)).

%! logged_in_(?Session:atom, ?UserName:atom, ?LoginTime:float) is nondet.
:- persistent(logged_in_(session:atom,user_name:atom,login_time:float)).

:- initialization(init_login_db).


user:prolog_file_type(db, database).
init_login_db:-
  absolute_file_name(
    project(login),
    File,
    [access(write),file_type(database)]
  ),
  create_file(File),
  db_attach(File, []).

%! logged_in(?Session:atom, ?UserName:atom, ?LoginTime:float) is nondet.

logged_in(Session, UserName, LoginTime):-
  with_mutex(user_db, logged_in_(Session, UserName, LoginTime)).

%! logged_in(?UserName:atom) is det.
% Succeeds if the given user name denotes the currently logged in user.

logged_in(UserName):-
  % Identify the current session.
  http_in_session(Session),
  logged_in(Session, UserName, _LoginTime).

%! login(+UserName:atom) is det.
% Accept the given user as logged into the current session.

login(UserName) :-
  get_time(LoginTime),
  http_session_id(Session),
  with_mutex(
    login_db,
    (
      retractall_logged_in_(Session, _UserName, _LoginTime),
      assert_logged_in_(Session, UserName, LoginTime)
    )
  ),
  debug(login_db, 'Login user ~w on session ~w.', [UserName,Session]).

%! logout(+UserName:atom) is det.
% Logout the user with the given name.

logout(UserName):-
  with_mutex(
    login_db,
    (
      once(logged_in_(_Session1, UserName, _LoginTime1)),
      retractall_logged_in_(_Session2, UserName, _LoginTime2)
    )
  ),
  debug(login_db, 'Logout user ~w.', [UserName]).
logout(UserName):-
  existence_error(user, UserName).

