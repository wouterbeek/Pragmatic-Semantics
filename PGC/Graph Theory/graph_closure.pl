:- module(
  graph_closure,
  [
    graph_closure/3, % +Elements:ordset
                     % :NeighborPredicate
                     % -ElementsUnderClosure:ordset
    graph_closure/4 % +Elements:ordset
                    % :NeighborPredicate
                    % -ElementsUnderClosure:ordset
                    % -Propositions:ordset
  ]
).

/** <module> Graph closure

graph_closure/4 can be used with a SPARQL query as `NeighborPredicate`:

~~~{.pl}
graph_closure/4 % +Elements:ordset([bnode,iri,literal])
                % :NeighborPredicate:iri
                % -ElementsUnderClosure:ordset(or([bnode,iri,literal]))
                % -PropositionsUnderClosure:ordset(compound)
~~~

@author Wouter Beek
@version 2013/12-2014/01
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate(graph_closure(+,2,-)).
:- meta_predicate(graph_closure_(+,2,+,-)).
:- meta_predicate(graph_closure(+,3,-,-)).
:- meta_predicate(graph_closure_(+,3,+,-,+,-)).



%! graph_closure(+Set:ordset, :NeighborPredicate, -ClosedSet:ordset) is det.
% @arg Set
% @arg NeighborPredicate Must return the neighbors in ordered sets.
% @arg ClosedSet

graph_closure([], _, []):- !.
graph_closure([H|T], NeighborPred, Sol):-
  graph_closure_([H|T], NeighborPred, [H], Sol).

graph_closure_([], _, Sol, Sol):- !.
graph_closure_([H1|T1], NeighborPred, Vs1, Sol):-
  call(NeighborPred, H1, Neighbors),
  exclude(in_ordset(Vs1), Neighbors, UnvisitedNeighbors),
  append(T1, UnvisitedNeighbors, T2),
  ord_union(Vs1, Neighbors, Vs2),
  graph_closure_(T2, NeighborPred, Vs2, Sol).

graph_closure([], _, [], []):- !.
graph_closure([H|T], Pred, VSol, PropsSol):-
  graph_closure_([H|T], Pred, [H], VSol, [], PropsSol).

graph_closure_([H1|T1], Pred, Vs1, VSol, Props1, PropsSol):-
  call(Pred, H1, Neighbors, NewProps),
  exclude(in_ordset(Vs1), Neighbors, NewNeighbors),
  append(T1, NewNeighbors, T2),
  ord_union(Vs1, Neighbors, Vs2),
  ord_union(Props1, NewProps, Props2),
  graph_closure_(T2, Pred, Vs2, VSol, Props2, PropsSol).

in_ordset(Set, Element):-
  memberchk(Element, Set).

