:- module(test, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(process)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

:- initialization(test).

test:-
  download_archive(FromFile),
  unpack_archive(FromFile, ToFile),
  load_and_unload_triples(ToFile).

download_archive(File):-
  source_file(test, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  URL = 'https://dl.dropboxusercontent.com/s/ud3te781s7lidso/sparql.tar.gz?dl=1&token_hash=AAGp9b36MZiOgCV1NsZ9WpLx6CXeooF4H08Nrt1ewUnhnw',
  absolute_file_name(
    sparql,
    File,
    [access(write),extensions(['tar.gz']),relative_to(ThisDir)]
  ),
  setup_call_cleanup(
    http_open(URL, HTTP_Stream, [cert_verify_hook(cert_verify)]),
    setup_call_cleanup(
      open(File, write, FileStream, [type(binary)]),
      copy_stream_data(HTTP_Stream, FileStream),
      close(FileStream)
    ),
    close(HTTP_Stream)
  ).

unpack_archive(FromFile, ToFile):-
  process_create(path(tar), [zxvf,file(FromFile)], []),
  directory_file_path(Dir, Base1, FromFile),
  atomic_list_concat([Base2|_], '.', Base1),
  absolute_file_name(Base2, ToFile, [access(read),relative_to(Dir)]).

load_and_unload_triples(File):-
  rdf_load(File, [format(turtle),graph(Graph)]),
  rdf_save_turtle(File, [graph(Graph)]),
  rdf_unload_graph(Graph),
  rdf_gc.

cert_verify(_, _, _, _, _).

