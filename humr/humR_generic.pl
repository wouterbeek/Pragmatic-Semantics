:- module(
  humR_generic,
  [
    axis_label/2, % +Resource:or([bnode,iri,literal])
                  % -AxisLabel:atom
    construct_bars/3, % +Predicate:iri
                      % +Values:or([list(ordset),ordset])
                      % -Bars:list(nonneg)
    discretize/3, % +Values:ordset
                  % +Intervals:positive_integer
                  % -Discretized:list(ordset)
    interval_label/2, % +ValueOrValues
                      % -IntervalLabel:atom
    is_total_order/1, % +Resource:iri
    resource_class/2, % +Resource:or([bnode,iri,literal])
                      % -Class:iri
    top_dogs/4 % +Resources:list(or([bnode,iri,literal]))
               % +Predicate:iri
               % +NumberOfTopResources:nonneg
               % -TopResources:list(or([bnode,iri,literal]))
  ]
).

/** <module> humR generics

Generic predicates that are used within the humR project.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(generics(list_ext)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_term(rdf_typed_literal)).
:- use_remote_module(rdfs(rdfs_read)).



%! axis_label(+Resource:or([bnode,iri,literal]), -AxisLabel:atom) is det.
% Returns an atomic label for resources _|such as|_ the given resource.

axis_label(Resource, Label):-
  phrase(axis_label(Resource), Codes),
  atom_codes(Label, Codes).

axis_label(Resource) -->
  {resource_class(Resource, Class)},
  rdf_term_name(Class).

%! construct_bars(
%!   +Predicate:iri,
%!   +Values:or([list(ordset),ordset]),
%!   -Bars:list(nonneg)
%! ) is det.
% Bars are constructed by counting the number of subjects that occur
% for the given predicate and object pairs.
%
% The object terms are either drawn from a set or from a list of sets.
% In the latter case a count occurs if the found object term is in the
% object term set.

construct_bars(P, Os, Bars):-
  findall(
    NumberOfS,
    (
      member(O, Os),
      aggregate_all(
        count,
        (rdf_has(_, P, O0), belongs_to(O0, O)),
        NumberOfS
      )
    ),
    Bars
  ).

belongs_to(O0, O):-
  is_list(O), !,
  memberchk(O0, O).
belongs_to(O, O):- !.

%! determine_range(
%!   +Min:integer,
%!   +Max:integer,
%!   -Begin:integer,
%!   -End:integer,
%!   -Step:nonneg
%! ) is det.

determine_range(Min, Max, Begin, End, Step):-
  Min =< Max,
  Diff is Max - Min,
  Step is 10 ** ceil(log10(Diff / 100)),
  Begin is floor(Min / Step) * Step,
  End is ceil(Max / Step) * Step.

%! discretize(
%!   +Values:ordset,
%!   +Intervals:positive_integer,
%!   -Discretized:list(ordset)
%! ) is det.
% Discretized the given set of totally ordered values.

discretize(Set, N, Disc):-
  split_list_by_number_of_sublists(Set, N, Disc).

%! interval_label(+ValueOrValues, -IntervalLabel:atom) is det.
% Returns a descriptive label for the given set of values.
%
% Single values are considered to be intervals of length 1.
% In these cases the label of this single value is given.

interval_label(Set, Label):-
  phrase(interval_label(Set), Codes),
  atom_codes(Label, Codes).

interval_label(Set) -->
  {
    is_list(Set),
    length(Set, Length),
    Length > 1, !,
    first(Set, A1),
    last(Set, Z1)
  },
  rdf_term_name(A1),
  "..",
  rdf_term_name(Z1).
interval_label([SingleValue]) -->
  interval_label(SingleValue).
interval_label([]) --> !, [].
interval_label(T) -->
  {rdfs_label(T, N)}, !,
  atom(N).
interval_label(T) -->
  rdf_term_name(T).

%! is_total_order(+Resource:iri) is semidet.
% Succeeds if resources _|such as|_ the given resource are totally ordered.

is_total_order(R):-
  resource_class(R, C),
  rdf_memberchk(C, [xsd:decimal,xsd:gYear,xsd:integer]).

%! resource_class(+Resource:or([bnode,iri,literal]), -Class:iri) is det.
% Returns the most descriptive RDFS class for the given resource.
%
% The *|most descriptive|* class is the one of which the resource is
% a direct member, i.e. subclasses of the most descriptive class do not
% have the resource as an instance.
%
% Since RDFS reasoning (with datatype extension) is not 100% yet,
% we include some simple hacks here.

resource_class(R, C):-
  rdf_typed_literal(R), !,
  rdf_typed_literal(R, _, C).
resource_class(R, C):-
  rdfs_individual_of(R, C).

top_dogs(Os1, P, N, Os2):-
  findall(
    C-O,
    (
      member(O, Os1),
      rdf_estimate_complexity(_, P, O, C)
    ),
    L1
  ),
  keysort(L1, L2),
  length(L3, N),
  append(_, L3, L2),
  pairs_values(L3, Os2).

