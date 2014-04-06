:- module(
  http_download,
  [
    download_to_file/3 % +Options:list(nvpair)
                       % +Url:url
                       % ?File:atom
  ]
).

/** <module> HTTP download

Support for downloading files over HTTP(S).

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/04
*/

:- use_remote_module(generics(uri_ext)).
:- use_remote_module(http(http_goal)).
:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(uri)).



%! download_to_file(+Options:list(nvpair), +Url:url, ?File:atom) is det.
% Downloads files from a URL to either the given file (when instantiated)
% of to the a file name that is created based on the URL.
%
% The following options are supported:
%   * =|force(+Redownload:boolean)|=
%     Sets whether files that were downloaded in the past
%     are overwritten or not.
%     Default: `false`.
%   * Other options are passed to http_goal/3 and, subsequently, http_open/3.
%
% @see url_nested_file/3 for how the file name is created based on the URL.

% The file was already downloaded in the past.
download_to_file(O1, _, File):-
  nonvar(File),
  option(force(false), O1, false),
  exists_file(File), !,
  access_file(File, read).
% An absolute file name is specified.
download_to_file(O1, Url, File):-
  nonvar(File),
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir),
  make_directory_path(Dir),

  % Check write access to the file.
  access_file(File, write),

  % Check the URL.
  uri_is_global(Url),

  % Multiple threads could be downloading the same file,
  % so we cannot download to the file's systematic name.
  % Instead we save to a thread-specific name.
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', ThreadName),
  file_name_extension(File, ThreadName, TmpFile),

  % The actual downloading part.
  http_goal(Url, O1, file_from_stream(TmpFile)),

  % Give the file its original name.
  rename_file(TmpFile, File).
% No file name is given; create a file name is a standardized way,
% based on the URL.
download_to_file(O1, Url, File):-
  url_nested_file(data(.), Url, File),
  download_to_file(O1, Url, File).

file_from_stream(File, HTTP_Stream):-
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HTTP_Stream, FileStream),
    close(FileStream)
  ).

