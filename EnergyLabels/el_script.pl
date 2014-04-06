:- module(
  el_script,
  [
    el_script/0
  ]
).

/** <module> Energylabels script

Script for generating an RDF representation of an XML file on energylabels.

The result was initially intended to be used within the Huiskluis project.

@author Wouter Beek
@see https://data.overheid.nl/data/dataset/energielabels-agentschap-nl
@tbd insert_newlines/2 seems to use too much memory.
@version 2013/04, 2013/06-2013/07, 2013/09-2013/12, 2014/02-2014/03
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(thread)).
:- use_module(os(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(io_ext)). %DEB
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xml_to_rdf)).

:- xml_register_namespace(el,
    'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl/').

% Global stack set to 4 Gb.
:- set_prolog_stack(global, limit(4*10**9)).



el_script:-
  absolute_file_name(el_data(.), Dir, [access(read),file_type(directory)]),
  extract_directory([recursive(true)], Dir),
  directory_files([order(lexicographic)], Dir, Files),
  forall(
    member(File, Files),
    parse_file(File, el, _)
  ).
  % Parallel processing costs too much RAM to run on my laptop.
  %%%%concurrent_maplist(parse_file, Files).

