:- module(quality, [quality/3]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

/** <module> Quality

Hypothesis: the frequency of syntax error is a good predictor of
errors in semantics.

@author Veruska
@author Wouter
@version 2014/04
*/

%! quality(+File:atom, Format:oneof([turtle]), -Quality:between(0.0,1.0)) is det.

quality(File, Format, Quality):-
  rdf_load(File, [format(Format),graph(monkey)]),
  rdf_statistics(triples_by_graph(monkey,T)),
  flag(x, I, I),
  Quality is T / (I + T).

:- multifile(user:message).

user:message(syntax_error(X)) -->
  ['Ok',X], {flag(a, X, X + 1)}.

