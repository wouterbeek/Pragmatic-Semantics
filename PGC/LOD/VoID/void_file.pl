:- module(
  void_file,
  [
    void_load/2, % +File:atom
                 % :Options:list(nvpair)
    void_save/3 % +Options:list(nvpair)
                % +VoidGraph:atom
                % ?File:atom
  ]
).

/** <module> VoID file handling

Support for the Vocabulary of Interlinked Datasets (VoID).

VoID is an RDF Schema vocabulary for expressing metadata about RDF datasets.
It is intended as a bridge between the publishers and users of RDF data,
with applications ranging from data discovery to cataloging and archiving
of datasets.

VoID is a W3C Interest Group Note as of 2011/03/03.

VoiD covers four areas of metadata:
  * *|General metadata|* following the Dublin Core model.
  * *|Access metadata|* describes how RDF data can be accessed
  *  using various protocols.
  * *|Structural metadata|* describes the structure and schema of datasets
    and is useful for tasks such as querying and data integration.
  * *|Description of links between datasets|* are helpful
    for understanding how multiple datasets are related.

@author WouterBeek
@compat http://www.w3.org/TR/void/
@version 2013/03-2013/05, 2013/09-2013/11, 2014/03
*/

:- use_remote_module(generics(thread_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(thread)).
:- use_remote_module(rdf(rdf_graph_name)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(void(void_db)).
:- use_remote_module(void(void_stat)).



%! void_load is det.

void_load:-
  % NO THREADS:
  findall(
    rdf_load(Location, []),
    (
      rdfs_individual_of(VoidDataset, void:'Dataset'),
      rdf(VoidDataset, void:dataDump, Location)
    ),
    Goals
  ),
  current_prolog_flag(cpu_count, N),
  concurrent(N, Goals, []).


%! void_load(+FileOrList, +Options:list(nvpair)) is det.
% Loads a VoID file and all the datasets defined in it.
%
% Also calculates VoID statistics for all datasets and asserts those
%  in the VoID file.

void_load(In, O1):-
  rdf_load(In, O1),
  void_load.


%! void_save(+Options:list(nvpair), +VoidGraph:atom, ?File:atom) is det.

void_save(O1, VoidGraph, File):-
  % Update VoID statistics.
  void_update(VoidGraph),

  % NO THREADS
  forall(
    void_dataset(VoidGraph, VoidDataset),
    void_save_dataset(O1, VoidGraph, VoidDataset)
  ),

  /*% THREADS
  forall_thread(
    (
      void_dataset(VoidGraph, VoidDataset),
      format(atom(Msg), 'Saving VoID dataset ~w.', [VoidDataset])
    ),
    void_save_dataset(O1, VoidGraph, VoidDataset),
    void_file,
    Msg
  ),*/

  % Then save the VoID graph itself.
  rdf_save(O1, VoidGraph, File).


%! void_save_dataset(
%!   +Options:list(nvpair),
%!   +VoidGraph:atom,
%!   +VoidDataset:iri
%! ) is det.

void_save_dataset(O1, VoidGraph, VoidDataset):-
  void_dataset_location(VoidGraph, VoidDataset, DatadumpFile),
  rdf_save(O1, VoidDataset, DatadumpFile).

