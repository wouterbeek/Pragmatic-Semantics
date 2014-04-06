:- module(
  rdf_serial_ext,
  [
    rdf_download_extract_load/2 % +Url:url
                                % +Options:list(nvpair)
  ]
).

/** <module> RDF serial extensions

Advances ways of loading/saving RDF,
e.g. unpacking archives, recursively loading VoID.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_module(generics(uri_ext)).
:- use_module(http(http_download_ext)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(rdf_file(rdf_file)).



%! rdf_download_extract_load(+Url:url, +Options:list(nvpair)) is det.

rdf_download_extract_load(Url, O1):-
  url_nested_file(data(.), Url, File),
  % The directory not the file (which may be deleted by now).
  file_directory_name(File, Dir),
  exists_directory(Dir), !,

  % Make sure all files are extracted.
  directory_files([], Dir, Files),
  maplist(extract_archive, Files),

  % These are all the RDF files we can get for this URL.
  rdf_directory_files(Dir, RdfFiles),
  rdf_load(RdfFiles, O1).
rdf_download_extract_load(Url, O1):-
  download_and_extract_to_files([], Url, _),
  rdf_download_extract_load(Url, O1).

