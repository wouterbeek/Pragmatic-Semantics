:- module(dir_ext,
  [
    append_directories/3, % +Dir1:atom
                          % +Dir2:atom
                          % -Dir3:atom
    copy_directory/3, % +Options:list(nvpair)
                      % +FromDirectory:atom
                      % +ToDirectory:atom
    create_directory/1, % +Directory:atom
    create_nested_directory/1, % +NestedDirectories:compound
    create_nested_directory/2, % +NestedDirectories:compound
                               % -Absolute:atom
    delete_directory/2, % +Options:list(nvpair)
                        % +Directory:atom
    directory_files/3, % +Options:list(nvpair)
                       % +Directory:atom
                       % -Absolutes:list(atom)
    directory_subdirectories/2, % ?Directory:atom
                                % ?Subdirectories:list(atom)
    link_directory_contents/2, % +FromDir:atom
                               % +ToDir:atom
    process_directory_files/5, % +FromDirectory:atom
                               % +FromFileTypes:list(atom)
                               % +ToDirectory:atom
                               % +ToFileType:atom
                               % :Goal
    process_directory_files/6, % +FromDirectory:atom
                               % +FromFileTypes:list(atom)
                               % +ToDirectory:atom
                               % +ToFileType:atom
                               % :Goal
                               % +Args:list
    run_in_working_directory/2, % :Call
                                % +WorkingDirectory:atom
    subdirectories_to_directory/2 % +Subdirectories:list(atom)
                                  % -Directory:atom
  ]
).

/** <module> Directory extensions

Extensions for handling directories.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09, 2013/11-2014/02, 2014/04
*/

:- use_remote_module(generics(atom_ext)).
:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(option_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(os(safe_file)).

:- meta_predicate(run_in_working_directory(0,+)).



%! append_directories(+Dir1:atom, +Dir2:atom, -Dir3:atom) is det.

append_directories(Dir1, Dir2, Dir3):-
  directory_subdirectories(Dir1, Subdirs1),
  directory_subdirectories(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  subdirectories_to_directory(Subdirs3, Dir3),
  make_directory_path(Dir3).


%! copy_directory(
%!   +Options:list(nvpair),
%!   +FromDirectory:atom,
%!   +ToDirectory:atom
%! ) is det.
% The following options are supported:
%   * =|safe(+SafeOrNot:boolean)|=

copy_directory(O1, FromDir, ToDir):-
  merge_options([include_self(true)], O1, O2),
  delete_directory(O2, ToDir),
  copy_directory(FromDir, ToDir).


%! create_directory(+Dir:atom) is det.
% Creates a directory with the given name.
%
% @arg Dir The atomic name of a directory.

% The directory already exists, so do nothing.
create_directory(Dir):-
  exists_directory(Dir), !.
% The directory does not already exist, so create it.
create_directory(Dir):-
  is_absolute_file_name(Dir), !,
  directory_subdirectories(Dir, SubDirs),
  % Recursively assert all subpaths.
  % The root node is indicated by the empty atom.
  create_directory('', SubDirs).

create_directory(_CurrentDir, []):- !.
create_directory(CurrentDir, [NextSubDir|SubDirs]):-
  directory_file_path(CurrentDir, NextSubDir, Dir),
  (
    exists_directory(Dir)
  ;
    make_directory(Dir)
  ), !,
  create_directory(Dir, SubDirs).

create_nested_directory(NestedDir):-
  create_nested_directory(NestedDir, _Absolute).


%! create_nested_directory(
%!   +NestedDirectory:compound,
%!   -AbsoluteDirectory:atom
%! ) is det.
% Returns a nested file path.
%
% @arg NestedDir A compound term of linearly nested atoms
%      representing the subsequent subdirectories. The final atom
%      is the name of the file.
% @arg Absolute The absolute path of the nested directory specification.

create_nested_directory(AbsoluteDir, AbsoluteDir):-
  atomic(AbsoluteDir), is_absolute_file_name(AbsoluteDir), !,
  create_directory(AbsoluteDir).
create_nested_directory(Alias, AbsoluteDir):-
  atom(Alias), !,
  Spec =.. [Alias,'.'],
  absolute_file_name(Spec, AbsoluteDir),
  create_directory(AbsoluteDir).
create_nested_directory(NestedDir, AbsoluteDir):-
  % First we construct the atomic name of the outer directory.
  NestedDir =.. [NestedOuterDir,NestedInnerDir],
  create_nested_directory(NestedOuterDir, AbsoluteOuterDir),
  % Then we add the inner directories recursively.
  create_nested_directory(NestedInnerDir, AbsoluteOuterDir, AbsoluteDir).

%! create_nested_directory(
%!   +NestedDirectory:compound,
%!   +OldAbsoluteDirectory:atom,
%!   -NewAbsoluteDirectory:atom
%! ) is det.
% Adds the nested directories term to the given atomic directory,
% returning another atomic directory.

create_nested_directory(SubDir, OldDir, NewDir):-
  atom(SubDir), !,
  atomic_list_concat([OldDir,'/',SubDir], NewDir),
  create_directory(NewDir).
create_nested_directory([], NewDir, NewDir):- !.
create_nested_directory(NestedDir, OldDir, NewDir):-
  (
    is_list(NestedDir)
  ->
    NestedDir = [OuterDir|InnerNestedDir]
  ;
    NestedDir =.. [OuterDir,InnerNestedDir]
  ),
  atomic_list_concat([OldDir,'/',OuterDir], TempDir),
  create_directory(TempDir),
  create_nested_directory(InnerNestedDir, TempDir, NewDir).


%! delete_directory(+Options:list(nvpair), +Directory:atom) is det.
% Deletes all file in the given directory that are of the given file type.
%
% The following options are supported:
%   * =|include_self(+IncludeOrNot:boolean)|=
%   * =|safe(+SafeOrNot:boolean)|=
%
% @arg Options A list of name-value pairs.
% @arg Directory The atomic name of a directory.

% Directories.
delete_directory(O1, Dir):-
  exists_directory(Dir), !,
  directory_files(
    [include_directories(true),include_self(false),recursive(false)],
    Dir,
    Children
  ),
  replace_option(O1, include_self, true, IncludeSelf, O2),
  maplist(delete_directory(O2), Children),
  (
    IncludeSelf == true
  ->
    delete_directory(Dir),
    debug(dir_ext, 'Directory ~w was safe-deleted.', [Dir])
  ;
    true
  ).
% Non-file entries.
delete_directory(_, X):-
  directory_subdirectories(X, SubDirs),
  last(SubDirs, Last),
  memberchk(Last, ['.','..']), !.
% Files.
delete_directory(O1, File):-
  (
    option(safe(true), O1, false)
  ->
    safe_delete_file(File)
  ;
    delete_file(File)
  ).


%! directory_files(
%!   +Options:list(nvpair),
%!   +Directory:atom,
%!   -AbsoluteFiles:list(atom)
%! ) is det.
% Variant of directory_files/2 that returns absolute file names
% instead of relative ones and excludes non-file entries.
%
% The following options are supported:
%   * =|file_types(+FileTypes:list(atom))|=
%     A list of atomic file types that are used as a filter.
%     Default: no file type filter.
%   * =|include_directories(+IncludeDirectories:boolean)|=
%     Whether (sub)directories are included or not.
%     Default: `false`.
%   * =|include_self(+IncludeSelf:boolean)|=
%     Whether or not the enclosing directory is included.
%     Default: `false`.
%   * =|order(+Order:oneof([lexicographic,none]))|=
%     The order in which the files are returned.
%     Lexicographic order uses ordsets.
%     Default: `none`.
%   * =|recursive(+Recursive:boolean)|=
%     Whether subdirectories are searched recursively.
%     Default: `true`.
%
% @arg Options A list of name-value pairs.
% @arg Directory The atomic name of a directory.
% @arg AbsoluteFiles a list of atomic names of absolute file names.

directory_files(O1, Directory, Files4):-
  % Note that the list of files is *not* ordered!
  directory_files(Directory, New1),

  % Remove `.` and `..`.
  exclude(nonfile_entry, New1, New2),

  % Make the file names absolute.
  maplist(directory_file_path(Directory), New2, New3),

  partition(exists_directory, New3, NewDirectories, NewFiles1),
  option(file_types(FileTypes), O1, _VAR),
  include(has_file_type(FileTypes), NewFiles1, NewFiles2),

  % Include directories and files from deeper recursion levels.
  (
    option(recursive(true), O1)
  ->
    select_option(include_self(_), O1, O2, _),
    maplist(directory_files(O2), NewDirectories, NewFiless),
    append([NewFiles2|NewFiless], Files1)
  ;
    Files1 = NewFiles2
  ),

  % Include directories from this recursion level.
  (
    option(include_directories(true), O1)
  ->
    append(Files1, NewDirectories, Files2)
  ;
    Files2 = Files1
  ),

  % Include the parent directory.
  (
    option(include_self(true), O1, false)
  ->
    Files3 = [Directory|Files2]
  ;
    Files3 = Files2
  ),

  % Apply requested ordering.
  option(order(Order), O1, none),
  (
    Order == lexicographic
  ->
    sort(Files3, Files4)
  ;
    Files4 = Files3
  ).


%! directory_subdirectories(
%!   +Directory:atom,
%!   +Subdirectories:list(atom)
%! ) is semidet.
%! directory_subdirectories(
%!   +Directory:atom,
%!   -Subdirectories:list(atom)
%! ) is det.
%! directory_subdirectories(
%!   -Directory:atom,
%!   +Subdirectories:list(atom)
%! ) is det.

directory_subdirectories(Dir, Subdirs1):-
  (
    nonvar(Dir),
    \+ is_absolute_file_name(Dir)
  ->
    Subdirs2 = Subdirs1
  ;
    Subdirs2 = [''|Subdirs1]
  ),
  atomic_list_concat(Subdirs2, '/', Dir).


has_file_type(FileTypes, _):-
  var(FileTypes), !.
has_file_type(FileTypes, File):-
  file_type(FileType, File),
  nonvar(FileType),
  memberchk(FileType, FileTypes).

nonfile_entry('.').
nonfile_entry('..').


%! link_directory_contents(+FromDir:atom, +ToDir:atom) is det.
% Creates symbolic link in `ToDir` for all the files in `FromDir`.

link_directory_contents(FromDir, ToDir):-
  directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    FromDir,
    FromFiles
  ),
  maplist(link_file(ToDir), FromFiles).


process_directory_files(FromDir, FromFileTypes, ToDir, ToFileType, Goal):-
  process_directory_files(
    FromDir,
    FromFileTypes,
    ToDir,
    ToFileType,
    Goal,
    []
  ).

process_directory_files(
  FromDir,
  FromFileTypes,
  ToDir,
  ToFileType,
  Goal,
  Args
):-
  directory_files(
    [
      file_types(FromFileTypes),
      include_directories(false),
      order(lexicographic),
      recursive(true)
    ],
    FromDir,
    FromFiles
  ),
  maplist(
    process_directory_file(FromDir, ToDir, ToFileType, Goal, Args),
    FromFiles
  ).

process_directory_file(FromDir, ToDir, ToFileType, Goal, Args, FromFile):-
  relative_file_name(FromFile, FromDir, FromRelativeFile),

  % Change the extension of the new file
  % to reflect the new serialization format.
  file_type_alternative(FromRelativeFile, ToFileType, ToRelativeFile),

  directory_file_path(ToDir, ToRelativeFile, ToFile),

  % The directory structure may not yet exist.
  create_file_directory(ToFile),

  apply(Goal, [FromFile,ToFile|Args]).


%! run_in_working_directory(:Goal, +WorkingDirectory:atom) is det.

run_in_working_directory(Goal, WD1):-
  working_directory(WD1, WD2),
  call(Goal),
  working_directory(WD2, WD1).


subdirectories_to_directory(Subdirs, Dir2):-
  atomic_list_concat(Subdirs, '/', Dir1),
  atomic_concat('/', Dir1, Dir2).

