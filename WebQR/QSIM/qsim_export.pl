:- module(
  qsim_export,
  [
    qr_state//6, % +Indent:nonneg
                 % +New:boolean
                 % +Brackets:oneof([ascii,html])
                 % +Mode:oneof([natlang,qr,triple])
                 % +Graph:atom
                 % +State:iri
    qr_stmt//4, % +Indent:nonneg
                % +Brackets:oneof([ascii,html])
                % +Mode:oneof([natlang,qr,triple])
                % +Stmt:iri
    qr_transition//4, % +Indent:nonneg
                      % +New:boolean
                      % +FromState:iri
                      % +ToState:iri
    qr_quantity//3 % +Graph:atom,
                   % +State:iri,
                   % +Quantity:iri
  ]
).

/** <module> QSIM export

This module asserts certain triples in a given graph. This custom-created
graph can then subsequently be exported using export methods from the PGC.

@author Wouter Beek
@tbd Phase this out.
@version 2013/03, 2013/07, 2013/09, 2014/01
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(option_ext)).
:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(qsim(qsim_read)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf(rdf_reification)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').



% GENERIC IMPLEMENTATION, USING RDF-STATEMENT %

%! qr_state(
%!   +Indent:nonneg,
%!   +New:boolean,
%!   +Brackets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Graph:atom,
%!   +State:iri
%! )// is det.
% A description of the given state.
%
% @arg Indent
% @arg New Whether the state is new or not.
% @arg Mode `natlang` or `triple`; or `qr`, which produces QR-specific
%      descriptions (see the second part of this module).
% @arg Graph
% @arg State

qr_state(Indent1, New, Brackets, Mode, Graph, State) -->
  % Make sure the indentation option is set and is incremented.
  {
    default(0, Indent1),
    succ(Indent1, Indent2)
  },
  indent(Indent2),

  % New state?
  (
    {New == none}
  ->
    ``
  ;
    {New == true}
  ->
    `New state: `
  ;
    `State: `
  ),

  % State name.
  rdf_term_name(State),

  `:`,
  newline,

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
    dcg_multi1(
      qr_quantity(Graph, State),
      _Rep1,
      Qs,
      [separator(newline)]
    )
  ;
    % Statements describing the state.
    {
      aggregate_all(
        set(Stmt),
        rdf(State, qsim:statement, Stmt, Graph),
        Stmts
      )
    },
    dcg_multi1(
      qr_stmt(Indent2, Brackets, Mode),
      _Rep2,
      Stmts,
      [separator(newline)]
    )
  ).


%! qr_stmt(
%!   +Indent:nonneg,
%!   +Brackets:oneof([ascii,html]),
%!   +Mode:oneof([natlang,qr,triple]),
%!   +Statement:iri
%! )// is det.
% @arg Brackets Either `ascii` or `html`.
%      HTML mode has to be used for exporting via GraphViz,
%      since HTML escape codes have to be used for the angular brackets
%      that occur in the QR statement prints, as
%      non-escaped angular brackets would produce incorrect HTML syntax.

qr_stmt(Indent, Brackets, Mode, Stmt) -->
  % Indentation.
  indent(Indent),

  % Use the RDF statement printer.
  dcg_stmt(Brackets, Mode, Stmt).


%! qr_transition(
%!   +Indent:nonneg,
%!   +New:boolean,
%!   +FromState:iri,
%!   +ToState:iri
%! ) is det.
% A qualitative state transition.

qr_transition(Indent, New, FromState, ToState) -->
  % Indentation.
  {default(0, Indent)},
  indent(Indent),

  % New transition?
  (
    {New == none}
  ->
    ``
  ;
    {New == true}
  ->
    `New transition: `
  ;
    `Transition: `
  ),
  
  transition(rdf_term_name(FromState), rdf_term_name(ToState)).



% QR-SPECIFIC IMPLEMENTATION %

qr_derivative(G, S, Q) -->
  {
    derivative(G, S, Q, DQV),
    once(rdfs_label(DQV, DQV_Name))
  },
  atom(DQV_Name).


qr_magnitude(G, S, Q) -->
  {magnitude(G, S, Q, MQV)},
  qr_magnitude_(MQV).

qr_magnitude_(MQV) -->
  {rdf_is_bnode(MQV)}, !,
  {rdf_list(MQV, [MQV1,MQV2])},
  qr_magnitude_(MQV1),
  `..`,
  qr_magnitude_(MQV2).
qr_magnitude_(MQV) -->
  {once(rdfs_label(MQV, MQV_Label))},
  atom(MQV_Label).


%! qr_quantity(
%!   +Graph:atom,
%!   +State:iri,
%!   +Quantity:iri
%! )// is det.

qr_quantity(G, S, Q) -->
  % Quantity name
  {once(rdfs_label(Q, QName))},
  atom(QName),

  `:`,

  % Magnitude value
  qr_magnitude(G, S, Q),

  forward_slash,

  % Derivative value
  qr_derivative(G, S, Q).

