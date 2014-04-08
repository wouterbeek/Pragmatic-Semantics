:- module(rdf_nonstring_label, []).

/** <module> RDF non-string label

Test case which shows that Semweb loads RDFS labels
with no language tag as language-tagged strings without language tags,
but they should be loaded as typed literals with datatype IRI `xsd:string`.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/turtle)).

:- initialization(rdf_label).

rdf_label:-
  rdf_load('http://vocab.deri.ie/void.ttl', [format(turtle)]),
  aggregate_all(count, (rdf(_, _, literal(lang(LT,_))), var(LT)), N),
  format(user_output, '~d language-tagged strings with no language tag.', [N]).

