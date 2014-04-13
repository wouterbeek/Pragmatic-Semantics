% This file allows remotely stored file to be locally copied.
% It presupposes that the file search path named `project` is set.
%
% This file allows a file to be downloaded
% over unreliable internet connections,
% since it simply retries until it succeeds.

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(uri)).



base_url(Url):-
  uri_components(
    Url,
    uri_components(
      https,
      'github.com',
      'wouterbeek/Prolog-Communities/raw/master'
    )
  ).


cert_verify(_, _, _, _, _):- !.


guarantee_download(_, LocalPath):-
  exists_file(LocalPath), !.
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
  base_url(Url),
  absolute_file_name(project(Base), File, [access(write),file_type(prolog)]),
  guarantee_download(Url, File),
  ensure_loaded(File).


prolog_repository(Mode):-
  load_remote_file(optparse2),
  (
    Mode == remote
  ->
    load_remote_file(prolog_repository_remote)
  ;
    Mode == local
  ->
    load_remote_file(prolog_repository_local)
  ).

