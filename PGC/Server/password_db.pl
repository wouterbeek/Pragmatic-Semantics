:- module(
  password_db,
  [
    add_password/2, % +UserName:atom
                    % +UnencryptedPassword:list(code)
    password_db_file_unix/1, % -File:atom
    remove_password/1 % +UserName:atom
  ]
).

/** <module> Password database

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(crypt)).
:- use_module(library(persistency)). % Persistent declaration.
:- use_module(os(file_ext)).
:- use_module(os(os_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- persistent(password(user_name:atom,encrypted_password:atom)).

:- initialization(init_password_db).



add_password(UserName, UnencryptedPassword):-
  crypt(UnencryptedPassword, EncryptedPassword1),
  atom_codes(EncryptedPassword2, EncryptedPassword1),
  with_mutex(
    password_db,
    (
      % Remove all previous passwords (expected to be at most one).
      retractall_password(UserName, _OldEncryptedPassword),
      assert_password(UserName, EncryptedPassword2)
    )
  ).

init_password_db:-
  password_db_file(File),
  create_file(File),
  db_attach(File, []).

password_db_file(File):-
  absolute_file_name(
    project(password),
    File,
    [access(write),file_type(database)]
  ).

%! password_db_file_unix(-File:atom) is det.
% Persistency files have a formatting that is inconsistent with
% the UNIX/Apache format which http_authenticate/3 supports.
%
% @tbd Check whether it is a security issue to use the persistency file
%      directly.

password_db_file_unix(File):-
  absolute_file_name(project(password), File, [access(write)]),
  setup_call_cleanup(
    open(File, write, Stream, [lock(write)]),
    forall(
      password(UserName, EncryptedPassword),
      format(Stream, '~p:~@\n', [UserName,format(EncryptedPassword)])
    ),
    close(Stream)
  ).

%! remove_password(+UserName:atom) is det.
% Removes all passwords for the user with the given name.

remove_password(UserName):-
  with_mutex(
    password_db,
    retractall_password(UserName, _OldEncryptedPassword)
  ).

