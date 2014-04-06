:- module(
  rdf_ntriples_write,
  [
    rdf_ntriples_write/2 % +File:atom
                         % +Options:list
  ]
).

/** <module> RDF save to N-Triples

A simple implementation for emitting RDF data in
 N-Triples serialization format, reusing some of the Turtle writer.
Intended to work with RDF data stored using SWI-Prolog's Semweb library.

In N-Triples only short Turtle strings (delimited by a single double quote)
 occur.
Linefeeds and carriage returns are escaped.
This means that we can guarantee that the number of triples
 is the same as the number of lines in the generated file.

@author Wouter Beek
@author Jan Wielemaker
@compat http://www.w3.org/TR/2014/REC-n-triples-20140225/
@tbd We would like to serialize no duplicate triples.
     Provide this at least as an option.
@version 2014/03
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)). % Private predicates.

:- thread_local(bnode_counter/1).
:- thread_local(bnode_map/2).



%! rdf_ntriples_write(+File:atom, +Options:list) is det.
% Writes RDF data serialization in the N-Triples format to the given file.
%
% The following options are supported:
%   * =|graph(?Graph:atom)|=
%     The atomic name of a currently loaded RDF graph,
%      to restrict the triples that are saved,
%      or uninstantiated, in which case
%      all currently loaded triples are saved.
%
% @arg File The atomic name of a file.
% @arg Options A list of name-value pairs.

rdf_ntriples_write(File1, O1):-
  absolute_file_name(File1, File2, [access(write)]),
  setup_call_cleanup(
    open(File2, write, Out),
    rdf_write_ntriples(Out, O1),
    close(Out)
  ).


rdf_write_ntriples(Out, O1):-
  % Reset the blank node store.
  retractall(bnode_counter/1),
  assert(bnode_counter(0)),
  retractall(bnode_map/1),
  
  (
    option(graph(Graph), O1)
  ->
    forall(
      rdf(S, P, O, Graph:_),
      rdf_write_ntriple(Out, S, P, O)
    )
  ;
    forall(
      % Avoid duplicate triples.
      rdf(S, P, O),
      rdf_write_ntriple(Out, S, P, O)
    )
  ).


rdf_write_ntriple(Out, S, P, O):-
  rdf_write_term_space(Out, S),
  rdf_write_term_space(Out, P),
  rdf_write_term_space(Out, O),
  put_char(Out, '.'),
  put_code(Out, 10). % Newline


rdf_write_term_space(Out, Term):-
  rdf_write_term(Out, Term),
  put_char(Out, ' ').


rdf_write_term(Out, BNode):-
  rdf_is_bnode(BNode), !,
  (
    bnode_map(BNode, Id2)
  ->
    true
  ;
    retract(bnode_counter(Id1)),
    Id2 is Id1 + 1,
    assert(bnode_counter(Id2)),
    assert(bnode_map(BNode, Id2))
  ),
  format(Out, '_:~w', [Id2]).
rdf_write_term(Out, literal(type(Datatype,Value))):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  write(Out, '^^'),
  rdf_write_term(Out, Datatype).
rdf_write_term(Out, literal(lang(Language,Value))):- !,
  turtle:turtle_write_quoted_string(Out, Value),
  format(Out, '@~w', [Language]).
rdf_write_term(Out, literal(Value)):- !,
  turtle:turtle_write_quoted_string(Out, Value).
rdf_write_term(Out, IRI):-
  turtle:turtle_write_uri(Out, IRI).

