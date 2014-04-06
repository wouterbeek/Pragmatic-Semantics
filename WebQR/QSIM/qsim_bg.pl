:- module(
  qsim_bg,
  [
    export_bg/3, % +Mode:oneof([natlang,qr,triple])
                 % +Graph:atom
                 % -GIF:compound
    print_bg/3 % +Indent:nonneg
               % +Mode:oneof([natlang,qr,triple])
               % +Graph:atom
  ]
).

/** <module> QSIM behavior graphs

Predicates for outputting QSIM behavior graphs:
  * export_bg/3 for exporting to the Graph Interchange Format (GIF).
  * dcg_bg//4 for printing to the current output stream.

@author Wouter Beek
@version 2013/09, 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).
:- use_module(generics(pair_ext)).
:- use_module(html(html_dcg)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qsim(qsim_export)).
:- use_module(qsim(qsim_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').



% EXPORT %

%! bg_edge_term(
%!   +States:ordset(iri),
%!   +StatePair:pair(iri),
%!   -EdgeTerm:compound
%! ) is det.
% Returns the edge term for the given pair of states,
% used in the behavior graph export.

bg_edge_term(States, FromState-ToState, edge(FromV_Id,ToV_Id,[])):-
  nth0chk(FromV_Id, States, FromState),
  nth0chk(ToV_Id, States, ToState).


%! bg_vertex_color(+Graph:atom, +State:iri, -Color:atom) is det.
% Returns the color of the given state,
% used in the behavior graph export.

% States that have no duration / represent time points are colored red.
bg_vertex_color(G, S, red):-
  once((
    magnitude(G, S, Q, M),
    rdfs_individual_of(M, qsim:'Landmark'),
    derivative(G, S, Q, D),
    \+ rdf_global_id(qsim:std, D)
  )), !.
% States that have duration are colored green.
bg_vertex_color(_, _, green).


%! bg_vertex_label(
%!   +Indent:nonneg,
%!   +Brackets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Graph:atom,
%!   +State:iri
%! )// is det.
% Produces the label for the given state,
% used in the behavior graph export.

bg_vertex_label(Indent, Brackets, Mode, Graph, State) -->
  % Tell GraphViz that this is the start and end of an HTML label.
  dcg_between(
    `<`,
    `>`,
    bg_vertex_label_(Indent, Brackets, Mode, Graph, State)
  ).

bg_vertex_label_(Indent, Brackets, Mode, Graph, State) -->
  % The name of the state.
  {once(rdfs_label(State, StateName))},
  atom(StateName),
  html_element(br, []),

  % We have to use HTML mode here in order to use HTML escape codes
  % for the angular brackets that occur in the statement prints,
  % since non-escaped angular brackets would produce incorrect HTML syntax.
  {default(html, Brackets)},

  (
    {Mode == qr}
  ->
    % Collect the quantities for which we produce descriptions.
    % @tbd Restrict this to quantities from the current model/graph.
    {
      aggregate_all(
        set(Q),
        rdfs_individual_of(Q, qsim:'Quantity'),
        Qs
      )
    },

    % Multiple statements can be printed under one another
    % (separated by HTML linebreaks).
    dcg_multi1(
      qr_quantity(Graph, State),
      _,
      Qs,
      [separator(html_element(br, []))]
    )
  ;
    % Collect the statements for which we produce descriptions.
    {
      aggregate_all(
        set(Stmt),
        rdf(State, qsim:statement, Stmt, G),
        Stmts
      )
    },

    % Multiple statements can be printed under one another
    % (separated by HTML linebreaks).
    dcg_multi1(
      qr_stmt(Indent, Brackets, Mode),
      _,
      Stmts,
      [separator(html_element(br, []))]
    )
  ).


%! bg_vertex_term(
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Graph:atom,
%!   +States:ordset(iri),
%!   +State:iri,
%!   -VertexTerm:compound
%! ) is det.
% Returns the vertex compound term for the given state,
% used in the export of the behavior graph to the Graph Interchange Format.

bg_vertex_term(Mode, Graph, States, State, vertex(V_Id,State,V_Attrs)):-
  nth0chk(V_Id, States, State),
  bg_vertex_color(G, State, V_Color),
  dcg_with_output_to(
    atom(V_Label),
    bg_vertex_label(0, html, Mode, Graph, State)
  ),
  V_Attrs = [color(V_Color),label(V_Label)].


%! export_bg(
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Graph:atom,
%!   -GraphTerm:compound
%! ) is det.
% Exports the behavior graph of the given RDF graph.
% We assume that the RDF graph contains QSIM simulation results.

export_bg(Mode, Graph, graph(V_Terms,E_Terms,G_Attrs)):-
  % Collect the ordered state transitions.
  aggregate_all(
    set(FromState-ToState),
    rdf(FromState, qsim:next_state, ToState, Graph),
    StateTransitions
  ),

  % Collect the ordered states.
  pairs_to_members(StateTransitions, States),

  % Create the edge terms.
  maplist(bg_edge_term(States), StateTransitions, E_Terms),

  % Create the vertex terms.
  maplist(bg_vertex_term(Mode, Graph, States), States, V_Terms),

  % Graph attributes.
  G_Attrs = [dir(forward),label(Graph)].


%! dcg_bg(
%!   +Indent:nonneg,
%!   +Brakets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Graph:atom
%! ) is det.
% Prints the behavior graph that is encoded in the given RDF graph
% to the current output.

dcg_bg(Indent, Brackets, Module, Graph) -->
  % Make sure the indentation option is set.
  {default(0, Indent)},

  % Collect the state transitions per state (ordered by 'from'-state).
  {
    aggregate_all(
      set(From-Tos),
      (
        state(Graph, From),
        aggregate_all(
          set(To),
          rdf(From, qsim:next_state, To, Graph),
          Tos
        )
      ),
      Pairs
    )
  },

  % Print a separator before the first state.
  horizontal_line,
  newline,

  % Each state is treated separately.
  dcg_multi1(
    dcg_bg_(Indent, Brackets, Mode, Graph),
    _Rep,
    Pairs,
    [separator(newline)]
  ).


dcg_bg_(Indent, Brackets, Mode, G, From-Tos) -->
  % First print a description of the state
  % (based on the statements in it).
  qr_state(Indent, Brackets, false, Mode, G, From),

  % Print the transitions for this statem, if any.
  (
    % There are no transitions, so do not print the header either.
    {Tos == []}, !
  ;
    % Header for the transitions.
    `Transitions:`,
    newline,

    % Increment indentation and set the prefix to `none`.
    {succ(Indent1, Indent2)},
    
    % The transitions are processed one-by-one.
    dcg_multi1(
      qr_transition(Indent2, false, From),
      _Rep,
      Tos,
      [separator(newline)]
    )
  ),

  % End with a separator, provided at least one state was printed.
  (
    {Tos == []}, !
  ;
    horizontal_line
  ).

print_bg(Indent, Mode, Graph):-
  dcg_with_output_to(current_output, dcg_bg(Indent, Mode, Graph)).

