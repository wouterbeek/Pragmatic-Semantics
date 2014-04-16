:- module(quality, [quality/3]).

:- use_module(library(check_installation)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

/** <module> Quality

Hypothesis: the frequency of syntax error is a good predictor of
errors in semantics.

@author Veruska
@author Wouter
@version 2014/04
*/

%! quality(
%!   +File:atom,
%!   +Format:oneof([turtle]),
%!   -Quality:between(0.0,1.0)
%! ) is det.

quality(File, Format, Quality):-
  check_installation:run_collect_messages(
    rdf_db:rdf_load(File, [format(Format),graph(monkey)]),
    true,
    Messages
  ),
  length(Messages, I),
  rdf_statistics(triples_by_graph(monkey,T)),
  Quality is T / (I + T).

