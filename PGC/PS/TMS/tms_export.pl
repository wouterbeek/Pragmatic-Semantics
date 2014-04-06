:- module(
  tms_export,
  [
    tms_export_graph/3, % +Options:list(nvpair)
                        % +TMS:atom
                        % -GraphInterchangeFormat:compound
    tms_export_node/3 % +Options:list(nvpair)
                      % +Node:iri
                      % -GraphInterchangeFormat:compound
  ]
).

/** <module> TMS export

Exports TMS belief states,

@author Wouter Beek
@version 2013/05, 2013/09-2013/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(tms(tms)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_export_edges(+,+,r,r,+,-)).
:- rdf_meta(tms_export_node(+,r,-)).



%! tms_export_graph(+Options:list(nvpair), +TMS:atom, -GIF:compound) is det.
% Exports the TMS using GraphViz.

tms_export_graph(O1, TMS, GIF):-
  tms(TMS),
  aggregate_all(
    set(N),
    tms_node(TMS, N),
    Ns
  ),
  aggregate_all(
    set(J),
    tms_justification(TMS, J),
    Js
  ),
  tms_export_graph(O1, TMS, Ns, Js, GIF).

%! tms_export_graph(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Nodes:ordset(iri),
%!   +Justifications:ordset(iri),
%!   -GIF:compound
%! ) is det.
% The following options are supported:
%   * =|highlight(+Node:iri)|=
%     The node that gets highlighted.

tms_export_graph(O1, TMS, Ns, Js, graph(Vs,Es4,G_Attrs)):-
  maplist(tms_export_node_(O1, TMS), Ns, N_Vs),
  maplist(tms_export_justification_(O1, TMS), Js, J_Vs),
  ord_union(N_Vs, J_Vs, Vs),
  ord_empty(Es1),
  foldl(tms_export_cons_edges(TMS), Js, Es1, Es2),
  foldl(tms_export_in_edges(TMS), Js, Es2, Es3),
  foldl(tms_export_out_edges(TMS), Js, Es3, Es4),
  G_Attrs = [
    charset('UTF-8'),
    dir(forward),
    fontsize(11),
    label(TMS),
    overlap(false)
  ].

%! tms_export_node(+Options:list(nvpair), +Node:iri, -GIF:compound) is det.
% The following options are supported:
%   * =|base_url(url)|=
%     Relativizes TMS node IRIs to this base URL.
%   * =|recursive(boolean)|=
%     Whether the justifications graph is traversed recursively or not.

tms_export_node(O1, N, GIF):-
  % The argument for the node consists of a list of justifications.
  aggregate_all(
    set(J),
    (
      (
        rdf_has(J, tms:has_antecedent, N)
      ;
        rdf_has(J, tms:has_consequent, N)
      )
    ),
    Js
  ),

  % Collect the nodes that are involved in the justifications.
  % Notice that the exported node `N` is also part of the antecedent nodes `A_Ns`.
  aggregate_all(
    set(A_N),
    (
      member(J, Js),
      (
        rdf_has(J, tms:has_antecedent, A_N)
      ;
        rdf_has(J, tms:has_consequent, A_N)
      )
    ),
    A_Ns
  ),

  % Retrieve the TMS in which the node appears.
  tms_node(TMS, N),

  % The nodes (vertices) and justifications (edges) form a graph.
  merge_options([highlight(N)], O1, O2),
  tms_export_graph(O2, TMS, A_Ns, Js, GIF).



% SUPPORT PREDICATES: EDGES %

%! tms_export_edges(
%!   +TMS:atom,
%!   +InverseDirection:boolean,
%!   +Justification:iri,
%!   +PredicateTerm:iri,
%!   +EdgeTerms1:list(compound),
%!   -EdgeTerms:list(compound)
%! ) is det.

tms_export_edges(TMS, Inv, J, P, Es1, Es2):-
  aggregate_all(
    set(edge(From,To,E_Attrs)),
    (
      rdf(J, P, N, TMS),
      rdf_global_id(_:N_Id, N),
      rdf_global_id(_:J_Id, J),
      tms_export_edge_style(P, Style),
      E_Attrs = [color(black),style(Style)],
      (Inv == true -> From = J_Id, To = N_Id ; From = N_Id, To = J_Id)
    ),
    NewEs
  ),
  ord_union(Es1, NewEs, Es2).

tms_export_edge_style(P, solid ):-
  rdf_global_id(tms:has_consequent, P), !.
tms_export_edge_style(P, solid ):-
  rdf_global_id(tms:has_in, P), !.
tms_export_edge_style(P, dashed):-
  rdf_global_id(tms:has_out, P), !.

tms_export_cons_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, true, J, tms:has_consequent, Es1, Es2).

tms_export_in_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_in, Es1, Es2).

tms_export_out_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_out, Es1, Es2).



% SUPPORT PREDICATES: VERTICES %

%! tms_export_justification_(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Justification:iri,
%!   -VertexTerm:compound
%! ) is det.

tms_export_justification_(_O1, _TMS, J, vertex(J_Id,J,V_Attrs)):-
  rdf_global_id(_:J_Id, J),
  rdfs_label(J, L),
  V_Attrs = [color(blue),label(L),shape(rectangle),style(solid)].

%! tms_export_node_(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Node:iri,
%!   -VertexTerm:compound
%! ) is det.
% The following options are supported:
%   * =|highlight(+Node:iri)|=
%     The node that gets highlighted.

tms_export_node_(O1, TMS, N, vertex(N_Id,N,V_Attrs)):-
  rdf_global_id(_:N_Id, N),
  rdfs_label(N, L),
  tms_export_node_color(O1, TMS, N, C),

  % Retrieve the URL for the given TMS node.
  % Using a custom base URL breaks dereferencing the TMS node,
  % but it allows TMS graph navitation to be wrapped into
  % an arbitrary context, e.g. a development server running on localhost.
  option(base_url(BaseURL), O1, _),
  tms_node_to_url(N, BaseURL, N_URL),

  V_Attrs = [color(C),label(L),shape(ellipse),style(solid),'URL'(N_URL)].

%! tms_export_node_color(+TMS:atom, +Node:iri, -Color:atom) is det.
% Returns the color indicating the support status of the node.
%
% The following options are supported:
%   * =|highlight(+Node:iri)|=
%     The node that gets highlighted.

tms_export_node_color(O1, _TMS, N, purple):-
  option(highlight(N), O1), !.
tms_export_node_color(_O1, TMS, N, green):-
  is_in_node(TMS, N), !.
tms_export_node_color(_O1, TMS, N, red):-
  is_out_node(TMS, N), !.
tms_export_node_color(_O1, _TMS, _N, black).

