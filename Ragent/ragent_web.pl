:- module(
  ragent_web,
  [
    add_ragent_web/3, % +Graph:atom
                      % +RagentName:atom
                      % -Markup:list
    ragent_path_web/3, % +Graph:atom
                       % +RagentName:atom
                       % -Markup:list
    ragent_web/3, % +Graph:atom
                  % +RagentName:atom
                  % -Markup:list
    ragents_web/1, % -Markup:list
    remove_ragent_web/3, % +Graph:atom
                         % +RagentName:atom
                         % -Markup:list
    start_walker_web/2, % +Graph:atom
                        % -Markup:list
    walk_step_web/3 % +Graph:atom
                    % +RagentName:atom
                    % -Markup:list
  ]
).

/** <module> Ragent Web

Web interface for reasoning agents.

@author Wouter Beek
@version 2012/12-2013/01
*/

:- use_module(ragent(ragent)).
:- use_module(ragent(ragent_walker)).
:- use_module(server(graph_web)).
:- use_module(standards(html)).



add_ragent_web(Graph, RagentName, [element(p, [], [Message])]):-
  ragent(_Ragent, RagentName),
  !,
  format(
    atom(Message),
    'Ragent with name ~w already exists for graph ~w.',
    [RagentName, Graph]
  ).
add_ragent_web(Graph, RagentName, [element(p, [], [Message])]):-
  add_ragent(Graph, RagentName),
  format(
    atom(Message),
    'Reasoning agent ~w added to graph ~w.',
    [RagentName, Graph]
  ).

ragent_path_web(Graph, RagentName, [element(p, [], [Caption]), Table]):-
  ragent(Ragent, RagentName),
  format(
    atom(Caption),
    'The travels of ragent ~w on graph ~w.',
    [RagentName, Graph]
  ),
  ragent_path(Graph, Ragent, Path),
  ragent_path_web0(Graph, Path, Rows),
  list_to_table(
    [header(true)],
    [['From location', 'From resource', 'To location', 'To resource'] | Rows],
    Table
  ).

ragent_path_web0(_Graph, [], []).
ragent_path_web0(_Graph, [_LastLocation], []).
ragent_path_web0(
  Graph,
  [FromLocation, ToLocation | Rest],
  [[FromLocation, FromResource, ToLocation, ToResource] | Rows]
):-
  rdf(FromLocation, ragent:has_resource, FromResource, Graph),
  rdf(ToLocation, ragent:has_resource, ToResource, Graph),
  ragent_path_web0(Graph, [ToLocation | Rest], Rows).

%% ragent_web(+Graph:atom, +RagentName:atom, -Markup:list) is det.
% Returns the currently existing ragents for each of the current graphs.
%
% @param Graph The atomic name of a graph.
% @param RagentName The atomic name of an existing ragent.
% @param Markup A list of markup elements.

ragent_web(Graph, RagentName, [Table | GraphMarkup]):-
  ragent(Ragent, RagentName),
  findall(
    [Ragent, DateTime, Location],
    rlocation(Ragent, DateTime, Location),
    Rows
  ),
  list_to_table(
    [header(true)],
    [['Ragent', 'Datetime', 'Location'] | Rows],
    Table
  ),
  circle_graph_web(Graph, CircleGraphMarkup),
  random_graph_web(Graph, RandomGraphMarkup),
  append(CircleGraphMarkup, RandomGraphMarkup, GraphMarkup).

%% ragents_web(-Markup:list) is det.
% Returns the currently existing ragents for each of the current graphs.
%
% @param Markup A list of markup elements.

ragents_web(Markup):-
  % Retrieve the ordered set of graphs, then collect
  % the reasoning agents for each graph.
  findall(
    [element(b, [], [Title]) | RagentMarkup],
    (
      ragent(_Ragent, RagentName),
      format(atom(Title), '~w: ~w', [Graph, RagentName]),
      ragent_web(Graph, RagentName, RagentMarkup)
    ),
    Markups
  ),
  (
    Markups == []
  ->
    Markup = [element(p, [], ['There are no agents.'])]
  ;
    append(Markups, Markup)
  ).

remove_ragent_web(Graph, RagentName, [element(p, [], [Message])]):-
  ragent(_Ragent, RagentName),
  !,
  format(
    atom(Message),
    'Reasoning agent ~w was removed from graph ~w.',
    [RagentName, Graph]
  ).
remove_ragent_web(Graph, RagentName, [element(p, [], [Message])]):-
  format(
    atom(Message),
    'There is not agent called ~w in graph ~w to release.',
    [RagentName, Graph]
  ).

start_walker_web(Graph, [element(p, [], ['Something started walking.'])]):-
  start_walker(Graph).

walk_step_web(Graph, RagentName, Markup):-
  ragent(Ragent, RagentName),
  walk_step(Graph, Ragent),
  ragent_web(Graph, RagentName, Markup).
