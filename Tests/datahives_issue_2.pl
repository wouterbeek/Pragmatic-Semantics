:- module(datahives_issue_2, [go/0]).

/** <module> DataHives Issue 2

@author Wouter Beek
@see https://github.com/wouterbeek/DataHives/issues/2
@version 2014/03
*/

:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(process)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri)).

% Datahub
%url('http://km.aifb.kit.edu/projects/btc-2012/datahub/data-1.nq.gz').
url('http://km.aifb.kit.edu/projects/btc-2012/datahub/data-2.nq.gz').
% Rest
%url('http://km.aifb.kit.edu/projects/btc-2012/rest/data-2.nq.gz').
% Timbl
%url('http://km.aifb.kit.edu/projects/btc-2012/timbl/data-4.nq.gz').



go:-
  url(URL),
  url_nested_file(URL, ArchiveFile),
  download_to_file(URL, ArchiveFile),
  process_create(path(gunzip), ['-f',file(ArchiveFile)], []),
  file_name_extension(NQuadsFile, _, ArchiveFile),
  rdf_load(NQuadsFile, [format(nquads),graph(default)]),
  nquads_message(NQuadsFile),
  file_name_extension(Base, _, NQuadsFile),
  file_name_extension(Base, ttl, TurtleFile),
  save_as_turtle(TurtleFile).


save_as_turtle(TurtleFile):-
  findall(G, rdf_graph(G), Gs),
  forall(
    (
      member(G, Gs),
      rdf(S, P, O, G)
    ),
    rdf_assert(S, P, O, temp)
  ),
  rdf_save(TurtleFile, [format(turtle),graph(temp)]).


nquads_message(NQuadsFile):-
  findall(
    G-T,
    (
      rdf_graph_property(G,source(NQuadsFile)),
      rdf_statistics(triples_by_graph(G,T))
    ),
    Pairs
  ),
  pairs_keys(Pairs, Gs),
  length(Gs, G),
  pairs_values(Pairs, Ts),
  sum_list(Ts, T),
  format(user_output, '~:d triples were loaded in ~:d graphs.', [T,G]).


download_to_file(URL, File):-
  setup_call_cleanup(
    http_open(URL, HttpStream, []),
    setup_call_cleanup(
      open(File, write, FileStream, [type(binary)]),
      copy_stream_data(HttpStream, FileStream),
      close(FileStream)
    ),
    close(HttpStream)
  ).


url_nested_file(URL, File):-
  uri_components(URL, uri_components(_,_,Path,_,_)),
  atomic_list_concat(PathComponents, '/', Path),
  last(PathComponents, File).

