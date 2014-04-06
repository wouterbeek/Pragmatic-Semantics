:- module(
  archive_ext,
  [
    create_archive/2, % +Files:list(atom)
                      % +Archive:atom
    create_tarball/2, % +Files:list(atom)
                      % +Tarball:atom
    extract_archive/1, % +FromFile:atom
    extract_archive/2, % +FromFile:atom
                       % -Conversions:list(oneof([gunzipped,untarred,unzipped]))
    extract_directory/2, % +Options:list(nvpair)
                         % +Directory:atom
    is_archive/1 % +File:atom
  ]
).

/** <module> Archive extensions

Extensions to the support for archived files.

There are two groups of predicates here:
  1. Those that directly call GNU tools:
     - create_archive/2
     - create_tarball/2
     - extract_archive/[1,2]
     - is_archive/1
  2. Those that use =|library(archive)|= and `libarchive`:
     - extract_directory/2

@author Wouter Beek
@version 2013/12-2014/04
*/

:- use_module(generics(db_ext)).
:- use_module(library(archive)).
:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(mime_type)).

% application/x-bzip2
% .bz,.bz2,.tbz,.tbz2
:- mime_register_type(application, 'x-bzip2', bz2).
:- db_add_novel(user:prolog_file_type(bz, archive)).
:- db_add_novel(user:prolog_file_type(bz, bunzip2)).
:- db_add_novel(user:prolog_file_type(bz2, archive)).
:- db_add_novel(user:prolog_file_type(bz2, bunzip2)).
:- db_add_novel(user:prolog_file_type(tbz, archive)).
:- db_add_novel(user:prolog_file_type(tbz, bunzip2)).
:- db_add_novel(user:prolog_file_type(tbz2, archive)).
:- db_add_novel(user:prolog_file_type(tbz2, bunzip2)).
% application/x-gzip
% .gz
:- mime_register_type(application, 'x-gzip', gz).
:- db_add_novel(user:prolog_file_type(gz, archive)).
:- db_add_novel(user:prolog_file_type(gz, gunzip)).
% application/x-rar-compressed
% .rar
:- mime_register_type(application, 'x-rar-compressed', rar).
:- db_add_novel(user:prolog_file_type(rar, archive)).
:- db_add_novel(user:prolog_file_type(rar, rar)).
% application/x-tar
% .tar
% .tgz
:- mime_register_type(application, 'x-tar', tar).
:- db_add_novel(user:prolog_file_type(tar, archive)).
:- db_add_novel(user:prolog_file_type(tar, tar)).
% application/zip
% .zip
:- mime_register_type(application, 'zip', zip).
:- db_add_novel(user:prolog_file_type(zip, archive)).
:- db_add_novel(user:prolog_file_type(zip, zip)).



create_archive(Files, Archive):-
  findall(file(File), member(File, Files), O1),
  process_create(path(tar), ['-cjf',file(Archive)|O1], []).


create_tarball(Files, Archive):-
  findall(file(File), member(File, Files), O1),
  process_create(path(tar), ['-cf',file(Archive)|O1], []).


%! extract_archive(+FromFile:atom) is det.
% @see Wrapper around extract_archive/2.

extract_archive(File):-
  extract_archive(File, _).


%! extract_archive(
%!   +FromFile:atom,
%!   -Conversions:list(oneof([gunzipped,untarred,unzipped]))
%! ) is det.

extract_archive(FromFile1, Conversions):-
  file_name_extension(_, tgz, FromFile1), !,
  file_alternative(FromFile1, _, _, '.tar.gz', FromFile2),
  rename_file(FromFile1, FromFile2),
  extract_archive(FromFile2, Conversions).
:- db_add_novel(user:prolog_file_type(tgz, archive)).
extract_archive(FromFile, L):-
  file_name_extension(Base, Ext, FromFile),
  prolog_file_type(Ext, archive), !,
  extract_archive(Ext, FromFile, H),
  extract_archive(Base, T),
  L = [H|T].
extract_archive(_, []).


%! extract_archive(
%!   +Extension:oneof([bz2,gz,tgz,zip]),
%!   +FromFile:atom,
%!   -Conversion:oneof([gunzipped,untarred,unzipped])
%! ) is semidet.

extract_archive(Extension, File, gunzipped):-
  prolog_file_type(Extension, bunzip2), !,
  process_create(path(bunzip2), ['-f',file(File)], []).
extract_archive(Extension, File, gunzipped):-
  prolog_file_type(Extension, gunzip), !,
  process_create(path(gunzip), ['-f',file(File)], []).
extract_archive(Extension, File, untarred):-
  prolog_file_type(Extension, tar), !,
  directory_file_path(Directory, _, File),
  atomic_list_concat(['--directory',Directory], '=', C),
  process_create(path(tar), [xvf,file(File),C], []),
  delete_file(File).
extract_archive(Extension, File, unzipped):-
  prolog_file_type(Extension, zip), !,
  directory_file_path(Directory, _, File),
  process_create(path(unzip), [file(File),'-fo','-d',file(Directory)], []),
  delete_file(File).


%! extract_directory(+Options:list(nvpair), +Directory:atom) is det.
% Extracts all archives in the given directory.
% Extract files recursively, e.g. first `gunzip`, then `tar`.
%
% Options are passed to directory_files/3. Important are:
%   * =|file_types(+FileTypes:list(atom))|=
%     Only extracts files of the given types.
%   * =|recursive(+Recursive:boolean)|=
%     Includes archives that reside in subdirectories.

extract_directory(O1, Dir):-
  directory_files(O1, Dir, Files),
  extract_directory(O1, Dir, Files).

extract_directory(_, _, []):- !.
extract_directory(O1, Dir, PreviouslyExtractedFiles):-
  PreviouslyExtractedFiles \== [], !,
  directory_files(O1, Dir, Files),
  % Leaves files that are candidates for further extraction.
  include(extract_file, Files, ExtractedFiles),
  extract_directory(O1, Dir, ExtractedFiles).


%! extract_file(+File:atom) is semidet.
% Succeeds if the given file can be extracted.

extract_file(File):-
  file_directory_name(File, Dir),
  catch(
    (
      archive_extract(File, Dir, []),
      delete_file(File)
    ),
    error(archive_error(25, _UnrecognizedArchiveFormat), _),
    fail
  ).


%! is_archive(+File:atom) is semidet.

is_archive(File):-
  file_name_extension(_, Ext, File),
  prolog_file_type(Ext, archive), !.

