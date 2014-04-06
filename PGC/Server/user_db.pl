:- module(
  user_db,
  [
    add_user/2, % +UserName:atom
                % +Properties:list
    add_user_property/3, % +UserName:atom
                         % +PropertyName:atom
                         % +PropertyValue
    user/1, % ?UserName:atom
    user/2, % ?UserName:atom
            % ?Properties:list
    user_property/2, % ?UserName:atom
                     % ?Property:compound
    user_property/3, % ?UserName:atom
                     % ?PropertyName:atom
                     % ?PropertyValue
    remove_user/1 % +UserName:atom
  ]
).

/** <module> User administration

Core user administration.
Also keeps track of who is logged in.

The user administration is based on the following:
  * Persistent facts logged_in/3 and user/2.
  * Session management.

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(option_ext)).
:- use_remote_module(generics(user_input)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(library(persistency)). % Declarations
:- use_remote_module(os(os_ext)).
:- use_remote_module(pl(pl_mode)).
:- use_remote_module(server(login_db)).
:- use_remote_module(server(password_db)).

:- db_add_novel(user:prolog_file_type(db, database)).

%! user(?UserName:atom, ?Properties:list:compound) is nondet.
:- persistent(user(user:atom,properties:list(compound))).

%:- initialization(init_user_db).

init_user_db:-
  user_db_file(File),
  (
    exists_file(File)
  ->
    db_attach(File, [])
  ;
    touch(File),
    db_attach(File, []),
    % First time deployment.
    add_user(admin, [roles([admin])]),
    user_input_password('Enter the password for admin.', UnencryptedPassword),
    add_password(admin, UnencryptedPassword)
  ).



%! add_user(+UserName:atom, +Properties:list) is det.
% Adds a new user with the given properties.

add_user(UserName, _Properties1):-
  current_user(UserName, _Properties2), !,
  permission_error('Add user', 'User name', UserName).
add_user(UserName, Properties1):-
  % Make sure the properties are sorted.
  sort(Properties1, Properties2),
  with_mutex(user_db, assert_user(UserName, Properties2)).


%! add_user_property(
%!   +UserName:atom,
%!   +PropertyName:atom,
%!   +PropertyValue
%! ) is det.

add_user_property(User, Name, Value):-
  with_mutex(
    user_db,
    (
      user(User, O1),
      add_option(O1, Name, Value, O2),
      retractall_user(User, _O0),
      assert_user(User, O2)
    )
  ).


%! current_user(?UserName:atom, ?Properties:list:compound) is nondet.

current_user(UserName, Properties):-
  with_mutex(user_db, user(UserName, Properties)).


%! remove_user(+UserName:atom) is det.
% Delete named user from user-database.

remove_user(UserName):-
  with_mutex(
    user_db,
    (
      once(user(UserName, Properties)),
      retractall_user(UserName, Properties)
    )
  ).
remove_user(UserName):-
  existence_error('User name', UserName).


%! user(?UserName:atom) is nondet.
% Registered users.

user(UserName):-
  current_user(UserName, _Properties).


%! user_db_file(-File:atom) is semidet.
% Returns the file that stores the database of users.
% Fails in case the file does not exist.

user_db_file(File):-
  absolute_file_name(
    project(user),
    File,
    [access(write),file_type(database)]
  ).


%! user_property(?UserName:atom, ?Property:compound) is nondet.
%! user_property(+UserName:atom, +Property:compound) is semidet.
% Users and their properties.
%
% In addition to properties explicitly stored with users, we define:
%   * =|connection(LoginTime,Idle)|=
%   * =|session(SessionID)|=

% Connection information for a user.
user_property(UserName, connection(LoginTime,Idle)):- !,
  logged_in(Session, UserName, LoginTime),
  http_current_session(Session, idle(Idle)).
% Session identification for a user.
user_property(UserName, session(Session)):- !,
  logged_in(Session, UserName, _LoginTime),
  % A session can have at most one user.
  (nonvar(Session) -> ! ; true).
% Explicitly stored properties.
user_property(UserName, Property):-
  maplist(nonvar, [UserName,Property]), !,
  '_user_property'(UserName, Property), !.
user_property(UserName, Property):-
  '_user_property'(UserName, Property).

'_user_property'(UserName, Property):-
  current_user(UserName, Properties),
  member(Property, Properties).
'_user_property'(UserName, Property):-
  user_property(UserName, Property).

user_property(UserName, PropertyName, PropertyValue):-
  Property =.. [PropertyName,PropertyValue],
  user_property(UserName, Property).

