:- module(test, []).

:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri)).

:- debug(test).

:- initialization(test).

url('https://dl.dropboxusercontent.com/s/ud3te781s7lidso/sparql.tar.gz?dl=1&token_hash=AAGp9b36MZiOgCV1NsZ9WpLx6CXeooF4H08Nrt1ewUnhnw').



archive_found_locally(File):-
  url(URL),
  url_to_base(URL, Base),
  absolute_file_name(
    Base,
    File,
    [access(read),extensions(['tar.gz']),file_errors(fail)]
  ).


cert_verify(_, _, _, _, _).


download_archive(File):-
  url(URL),
  url_to_base(URL, Base),
  absolute_file_name(Base, File, [access(write),extensions(['tar.gz'])]),
  setup_call_cleanup(
    http_open(URL, HTTP_Stream, [cert_verify_hook(cert_verify)]),
    setup_call_cleanup(
      open(File, write, FileStream, [type(binary)]),
      copy_stream_data(HTTP_Stream, FileStream),
      close(FileStream)
    ),
    close(HTTP_Stream)
  ).


file_found_locally(File):-
  url(URL),
  url_to_base(URL, Base),
  absolute_file_name(Base, File, [access(read),file_errors(fail)]).


load_and_unload_triples(File):-
  Graph = temporary,
  setup_call_cleanup(
    rdf_load(File, [format(turtle),graph(Graph)]),
    rdf_save_turtle(File, [graph(Graph)]),
    (
      rdf_unload_graph(Graph),
      rdf_gc
    )
  ).


test:-
  file_found_locally(File), !,
  load_and_unload_triples(File).
test:-
  archive_found_locally(File), !,
  unpack_archive(File),
  test.
test:-
  download_archive(_), !,
  test.


unpack_archive(File):-
  process_create(path(tar), [zxvf,file(File)], []).


url_to_base(URL, Base):-
  uri_components(URL, uri_components(_, _, Path, _, _)),
  atomic_list_concat(PathComponents, '/', Path),
  reverse(PathComponents, [Tmp|_]),
  atom_concat(Base, '.tar.gz', Tmp).

