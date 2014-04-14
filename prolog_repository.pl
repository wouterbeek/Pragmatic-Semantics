:- set_prolog_flag(autoload, true).

% This file allows remotely stored file to be locally copied.
% It presupposes that the file search path named `project` is set.
%
% This file allows a file to be downloaded
% over unreliable internet connections,
% since it simply retries until it succeeds.

:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(uri)).



assert_index(Alias, Path):-
  catch(is_absolute_file_name(Path), _, fail), !,
  make_directory_path(Path),
  assert(user:file_search_path(Alias, Path)).
assert_index(Alias, Path):-
  Path =.. [Parent,Child],
  Spec =.. [Parent,'.'],
  absolute_file_name(Spec, ParentDir, [file_type(directory)]),
  directory_file_path(ParentDir, Child, ChildDir),
  make_directory_path(ChildDir),
  assert(user:file_search_path(Alias, ChildDir)).


base_url(Url):-
  uri_components(
    Url,
    uri_components(
      https,
      'github.com',
      '/wouterbeek/Prolog-Communities/raw/master/',
      _,
      _
    )
  ).


cert_verify(_, _, _, _, _):- !.


guarantee_download(Url, Path):-
  catch(
    setup_call_cleanup(
      http_open(
        Url,
        HttpStream,
        [cert_verify_hook(cert_verify),status_code(Status)]
      ),
      http_process(Status, HttpStream, Url, Path),
      close(HttpStream)
    ),
    _,
    guarantee_download(Url, Path)
  ).


guarantee_download(_, Path, false):-
  exists_file(Path), !.
guarantee_download(Url, Path, true):-
  exists_file(Path), !,
  delete_file(Path),
  guarantee_download(Url, Path).
guarantee_download(Url, Path, _):-
  guarantee_download(Url, Path).


http_process(Status, HttpStream, _, File):-
  between(200, 299, Status), !,
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HttpStream, FileStream),
    close(FileStream)
  ).
http_process(_, _, Url, Path):-
  guarantee_download(Url, Path).


load_remote_file(Base):-
  base_url(Url1),
  file_name_extension(Base, pl, Name),
  atom_concat(Url1, Name, Url2),
  absolute_file_name(project(Base), File, [access(write),file_type(prolog)]),
  guarantee_download(Url2, File),
  ensure_loaded(File).


prolog_repository(Mode, Dir):-
  load_remote_file(optparse2),
  (
    Mode == remote
  ->
    load_remote_file(prolog_repository_remote)
  ;
    Mode == local
  ->
    load_remote_file(prolog_repository_local),
    prolog_local_init(Dir)
  ).

