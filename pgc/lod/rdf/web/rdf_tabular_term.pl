:- module(
  rdf_tabular_term,
  [
    rdf_tabular_term//2, % ?RdfGraph:atom
                         % +RdfTerm:or([bnode,iri,literal])
    rdf_tabular_terms//2 % ?RdfGraph:atom
                         % +RdfTerms:list(or([bnode,iri,literal]))
  ]
).

/** <module> RDF HTML table term

Generates HTML tables for overviews of singular RDF terms.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_remote_module(dcg(dcg_content)). % Meta-argument.
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf(rdf_stat)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(rdf_web(rdf_tabular)).
:- use_remote_module(rdf_web(rdf_tabular_class)).
:- use_remote_module(rdf_web(rdf_tabular_datatype)).
:- use_remote_module(rdf_web(rdf_tabular_property)).



%! rdf_tabular_term(
%!   ?RdfGraph:atom,
%!   +RdfTerm:or([bnode,iri,literal])
%! )// is det.
% Generates an HTML description of the given RDF term.
%
% The RDF graph parameter is optional.
% When uninstantiated, descriptions of the RDF term are gathered
% from all graphs.
%
% The following variants are supported, based on the kind of RDF term:
%   * Term is an RDFS class.
%     Module [rdf_tabular_class] handles this.
%   * Term is an RDF property.
%     Module [rdf_tabular_property] handles this.
%   * Term is a datatype IRI.
%     Enumerate all lexical expressions that occur for this datatype
%     in the given graph (or all graphs, if var).
%   * Terms is none of the above.
%     Enumerate all predicate-object pairs and all subject-predicate pairs
%     for the term.


% RDFS class
rdf_tabular_term(Graph, Class) -->
  {rdfs_class(Class)}, !,
  rdf_tabular_class(Graph, Class).
% RDF property.
% Show:
%   1. all domain classes,
%   2. all range classes,
%   3. all values for literal ranges.
rdf_tabular_term(Graph, P) -->
  {rdfs_property(P)}, !,
  rdf_tabular_property(Graph, P).
% Datatype Iri.
rdf_tabular_term(G, D) -->
  {rdf_datatype(D, _)}, !,
  rdf_tabular_datatype(G, D).
% Other
rdf_tabular_term(Graph, RdfTerm) -->
  html([
    h2('Predicate-object pairs'),
    \rdf_tabular_triples(RdfTerm, _, _, Graph),
    h2('Subject-object pairs'),
    \rdf_tabular_triples(_, _, RdfTerm, Graph)
  ]).


%! rdf_tabular_terms(
%!   ?RdfGraph:atom,
%!   +RdfTerms:list(or([bnode,iri,literal]))
%! )// is det.
% Generates an HTML listing of the given RDF terms,
% that is ordered by the number of triples in which the term participates
% (as a crude proxy for relevance).
%
% The RDF graph is optional, ranking the RDF terms according to
% triples in that graph.

rdf_tabular_terms(G, Ts) -->
  {
    % Order all resources based on the number of triples describing them.
    aggregate_all(
      set(N-T),
      (
        member(T, Ts),
        rdf_triples_by_term(G, T, N)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [T,N],
      member(N-T, Pairs3),
      Rows
    )
  },
  rdf_html_table([graph(G)], html('RDF terms'), [['RDF term']|Rows]).


%! rdfs_class(+RdfTerm:or([bnode,iri,literal])) is semidet.
%! rdfs_class(-RdfTerm:or([bnode,iri,literal])) is nondet.
% RDF terms that are RDFS classes.
%
% @tbd This predicate should be replaced by the RDF-MT rewrite.

rdfs_class(Class):-
  rdfs_individual_of(Class, rdfs:'Class').


%! rdfs_property(+RdfTerm:or([bnode,iri,literal])) is semidet.
%! rdfs_property(-RdfTerm:or([bnode,iri,literal])) is nondet.
% RDF terms that are RDF properties.
%
% @tbd This predicate should be replaced by the RDF-MT rewrite.

rdfs_property(P):-
  rdf(_, P, _).
rdfs_property(P):-
  rdfs_individual_of(P, rdf:'Property').

