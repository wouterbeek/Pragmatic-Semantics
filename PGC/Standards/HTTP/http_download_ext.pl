:- module(
  http_download_ext,
  [
    download_and_extract_to_files/3 % +Options:list(nvpair)
                                    % +Url:url
                                    % -Files:ordset(atom)
  ]
).

/** <module> HTTP download extensions

Advanced predicates for downloading over HTTP(S),
e.g. automatically extracting the downloaded files if they are archives.

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/04
*/

:- use_module(http(http_download)).
:- use_module(os(archive_ext)).
:- use_module(os(dir_ext)).



%! download_and_extract_to_files(
%!   +Options:list(nvpair),
%!   +Url:url,
%!   -Files:list(atom)
%! ) is det.

download_and_extract_to_files(O1, Url, Files):-
  download_to_file(O1, Url, File),
  file_directory_name(File, Dir),
  extract_archive(File),
  directory_files(
    [
      include_directories(true),
      include_self(false),
      order(lexicographic),
      recursive(true)
    ],
    Dir,
    Files
  ).

