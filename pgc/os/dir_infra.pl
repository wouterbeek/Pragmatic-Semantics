:- module(
  dir_infra,
  [
    create_personal_subdirectory/2, % +NestedDirectories:compound
                                    % -AbsoluteDirectory:atom
    output_directory/1, % -OutputDirectory:atom
    set_data_directory/0,
    set_data_directory/1, % +Directory:atom
    trashcan/1 % -Directory:atom
  ]
).

/** <module> Directory infrastructure

Predicates for creating an maintaining a specific infrastructure of
directories that are used for certain purposes, e.g. output, data, trash.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09, 2013/11-2014/02, 2014/04
*/

:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_remote_module(os(dir_ext)).
:- use_remote_module(os(file_ext)).



%! create_personal_subdirectory(
%!   +NestedDirectories:compound,
%!   -AbsoluteDirectory:atom
%! ) is det.
% Asserts a project-specific directory that is a direct subdirectory of the
% current user's home.
%
% This can be used to write files to. Something that is not guaranteed for
% the location where the code is run from.
%
% This requires that the project name has been set using project/2.

create_personal_subdirectory(Nested, Abs):-
  % Make sure the personal directory is there.
  personal_directory_init,

  create_nested_directory(personal(Nested), Abs).


home_init:-
  file_search_path(home, _), !.
home_init:-
  expand_file_name('~', [HomeDir]),
  assert(user:file_search_path(home, HomeDir)).


output_directory(OutDir):-
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_file_path(DataDir, 'Output', OutDir),
  make_directory_path(OutDir).


personal_directory_init:-
  file_search_path(personal, _), !.
personal_directory_init:-
  % Make sure the home directory is there.
  home_init,

  % Make sure that the project name has been asserted.
  current_predicate(project/2),
  project(Project, _),

  hidden_file_name(Project, Hidden),
  create_nested_directory(home(Hidden), _Dir),
  assert(user:file_search_path(personal, home(Hidden))).


set_data_directory:-
  absolute_file_name(
    project('.'),
    Dir1,
    [access(write),file_type(directory)]
  ),
  directory_file_path(Dir1, data, Dir2),
  make_directory_path(Dir2),
  assert(user:file_search_path(data, Dir2)).

% The data directory was already set.
set_data_directory(_):-
  user:file_search_path(data, _), !.
% Set data directory based on `data=DIR` command-line argument.
set_data_directory(Dir1):-
  absolute_file_name(
    Dir1,
    Dir2,
    [access(write),file_errors(fail),file_type(directory)]
  ), !,
  set_data_directory(Dir2).
% Set default data directory.
set_data_directory(_):-
  set_data_directory.


trashcan(Dir):-
  trashcan_init,
  absolute_file_name(
   personal('Trash'),
    Dir,
    [access(write),file_type(directory)]
  ).


trashcan_init:-
  file_search_path(trash, _), !.
trashcan_init:-
  create_personal_subdirectory('Trash', Dir),
  assert(user:file_search_path(trash, Dir)).

