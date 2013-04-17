:- module(
  ragent,
  [
    add_location/3, % +Graph:atom
                    % +Ragent:uri
                    % +Resource:uri
    add_ragent/2, % +Graph:atom
                  % +Ragent:atom
    current_resource/3, % +Graph:atom
                        % +Ragent:uri
                        % -Resource:uri
    next_location/3, % +Graph:atom
                     % +Ragent:uri
                     % -Next:uri
    ragent/2, % ?Ragent:uri
              % ?Name:atom
    ragent_path/3, % +Graph:atom
                   % +Ragent:uri
                   % -Path:list(uri)
    ragents/2, % +Graph:atom
               % -Ragents:ord_set(uri)
    remove_ragent/1, % +Ragent:uri
    rlocation/2, % ?Ragent:uri
                 % ?Resource:uri
    rlocation/3 % ?Ragent:uri
                % ?DateTime:date_time
                % ?Resource:uri
  ]
).

/** <module> RAGENT

Rational or reasoning agent (the same according to Aristotle).

@author Wouter Beek
@version 2012/12-2013/01
*/

:- use_module(generic(list_ext)).
:- use_module(graph_theory(graphs_trav)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)). % Used by a meta predicate.
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).

:- rdf_register_prefix(ragent, 'http://www.wouterbeek.com/prasem/ragent.owl#', [keep(true)]).



%% add_location(+Graph:atom, +Ragent:uri, +Resource:uri) is det.
% Registers a location for a reasoning agent.
%
% @param Graph The atomic name of a graph.
% @param Ragent A reasoning agent.
% @param Resource A resource at which the agent dwells.

add_location(Graph, Ragent, Resource):-
  flag(location, ID, ID + 1),
  format(atom(LocationID), 'loc_~w', [ID]),
  rdf_global_id(ragent:LocationID, Location),
  get_time(DateTime),
  rdf_assert_datatype(Location, ragent:has_datetime, dateTime, DateTime, Graph),
  rdf_assert(Location, ragent:has_resource, Resource, Graph),

  % If there is a previous location, then move the agent forward.
  (
    rdf(Ragent, ragent:current_location, PreviousLocation, Graph)
  ->
    rdf_retractall(Ragent, ragent:current_location, PreviousLocation, Graph),
    rdf_assert(PreviousLocation, ragent:next_location, Location, Graph)
  ;
    true
  ),
  rdf_assert(Ragent, ragent:has_location, Location, Graph),
  rdf_assert(Ragent, ragent:current_location, Location, Graph).

%% add_ragent(+Graph:atom, +Name:atom) is semidet.
% Creates a reasoning agent with the given name of the given graph.
% Agent names must be unique within a graph.
%
% @param Graph The atomic name of a graph.
% @param Name The atomic name of an agent.

add_ragent(Graph, _Name):-
  \+ rdf_graph(Graph),
  existence_error(atom, Graph).
add_ragent(Graph, Name):-
  init_ragent(Graph),
  rdf_global_id(ragent:Name, Ragent),
  rdfs_assert_individual(Ragent, ragent:ragent, Graph),
  rdfs_assert_label(Ragent, Name, Graph),
  % Register an initial location for the ragent.
  rdf_random(Resource, _Predicate, _Object, Graph, _Index),
  add_location(Graph, Ragent, Resource).

current_resource(Graph, Ragent, Resource):-
  rdf(Ragent, ragent:current_location, Location, Graph),
  rdf(Location, ragent:has_resource, Resource, Graph).

init_ragent(Graph):-
  format(atom(Label), 'Reasoning agents for graph ~w.', [Graph]),
  rdfs_assert_individual(ragent:ragent, rdfs:'Class',    Graph),
  rdfs_assert_subclass(ragent:ragent,   rdfs:'Resource', Graph),
  rdfs_assert_label(ragent:ragent, Label, Graph).

%% next_location(+Graph:atom, +Ragent:uri, -Next:uri) is det.
% Returns a new location where the agent can travel to.
%
% @param Graph The atomic name of a graph.
% @param Ragent A reasoning agent.
% @param Next A resource.

next_location(Graph, Ragent, Next):-
  current_resource(Graph, Ragent, From),
  % Consider all possibilities.
  setoff(To, rdf_edge([graph(Graph)], From-To), Tos),
  % Choose one of the alternatives blindly.
  random_member(Tos, Next),
  add_location(Graph, Ragent, Next).

%% ragent(?Ragent:uri, ?Name:atom) is nondet.
% A reasoning agent by name and graph.
%
% @param Ragent A reasoning agent resource.
% @param Name The atomic name of an agent.

ragent(Ragent, Name):-
  nonvar_det(ragent0(Ragent, Name)).
ragent0(Ragent, Name):-
  rdfs_individual_of(Ragent, ragent:ragent),
  rdfs_label(Ragent, Name).

ragent_path(Graph, Ragent, Path):-
  rdf(Ragent, ragent:current_location, CurrentLocation, Graph),
  rdf_global_id(ragent:next_location, Predicate),
  depth_path(CurrentLocation, Predicate, 10, true, Path).

%% ragents(+Graph:atom, -Ragents:ord_set(uri)) is det.
% Returns the ordered set of reasoning agents for the given graph.
%
% @param Graph The atomic name of a graph.
% @param Ragents An ordered set of reasoning agents.

ragents(Graph, Ragents):-
  setoff(
    Ragent,
    (
      rdfs_individual_of(Ragent, ragent:ragent),
      rdf_subject(Graph, Ragent)
    ),
    Ragents
  ).

%% remove_ragent(+Ragent:uri) is det.
% Removes the given reasoning agent.
%
% @param Ragent A reasoning agent.

remove_ragent(Ragent):-
  rdf_retractall(Ragent, _Predicate, _Object, _Graph).

%% rlocation(?Ragent:uri, ?Resource:uri) is nondet.
% Pairs of a reasoning agent and the resource where the agent is currently at.
%
% @see rlocation/3

rlocation(Ragent, Resource):-
  rlocation(Ragent, _DateTime, Resource).

%% rlocation(?Ragent:uri, ?DateTime:dateTime, ?Resource:uri) is nondet.
% Triples of a reasoning agent, a dateTime value, and the
% resource where the agent is at at the given dateTime.
%
% @param Ragent A reasoning agent.
% @param DateTime A dateTime value.
% @param Resource The resource where the reasoning agent at at the dateTime.

% We ensure that there is always exactly one location for
% a given reasoning agent.
rlocation(Ragent, DateTime, Resource):-
  nonvar(Ragent),
  !,
  rlocation0(Ragent, DateTime, Resource),
  !.
rlocation(Ragent, DateTime, Resource):-
  rlocation0(Ragent, DateTime, Resource).

rlocation0(Ragent, DateTime, Resource):-
  rdf(Ragent, ragent:has_location, Location, Graph),
  rdf(Location, ragent:has_resource, Resource, Graph),
  rdf_datatype(Location, ragent:has_datetime, dateTime, DateTime, Graph).
