:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

:- debug(test).

:- initialization(test).

test:-
  % Download the data file.
  FileName1 = 'el_4.ttl.tar.gz',
  (
    absolute_file_name(FileName1, File1, [access(read),file_errors(fail)]), !
  ;
    absolute_file_name(FileName1, File1, [access(write)]),
    setup_call_cleanup(
      (
        open(File1, write, Out, [type(binary)]),
        http_open('http://www.wouterblog.com/el_4.ttl.tar.gz', In, [])
      ),
      copy_stream_data(In, Out),
      (
        close(In),
        close(Out)
      )
    )
  ),
  
  % Unpack archive.
  file_directory_name(File1, Dir),
  archive_extract(File1, Dir, []),

  % Load the data into Semweb.
  FileName2 = 'el_4.ttl',
  G = test,
  absolute_file_name(FileName2, File2, [access(read)]),
  setup_call_cleanup(
    (
      number_of_triples('SETUP'),
      rdf_load(File2, [format(turtle),graph(G)])
    ),
    number_of_triples('CALL'),
    (
      rdf_unload_graph(G),
      rdf_gc,
      number_of_triples('CLEANUP')
    )
  ).

number_of_triples(C):-
  forall(
    rdf_graph(G),
    (
      rdf_statistics(triples_by_graph(G,N)),
      debug(test, '[~w] Graph ~w has ~w triples.', [C,G,N])
    )
  ).

