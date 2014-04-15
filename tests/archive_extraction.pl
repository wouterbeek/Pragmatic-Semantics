:- use_module(library(archive)).
:- use_module(library(http/http_open)).

:- initialization(test).

test:-
  File = 'elezione.rdf.gz',
  Url = 'http://dati.camera.it/ocd/files/elezione.rdf.gz',
  setup_call_cleanup(
    http_open(Url, HttpStream, []),
    setup_call_cleanup(
      open(File, write, FileStream, [type(binary)]),
      copy_stream_data(HttpStream, FileStream),
      close(FileStream)
    ),
    close(HttpStream)
  ),
  file_directory_name(File, Dir),
  archive_extract(File, Dir, [format(all),format(raw)]).

