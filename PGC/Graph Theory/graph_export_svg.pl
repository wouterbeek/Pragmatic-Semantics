:- module(
  graph_export_svg,
  [
    export_graph_svg/2 % +GraphTerm:compound
                       % -SvgDom:element
  ]
).

/** <module> GRAPH_EXPORT_SVG

Predicates for exporting an intermediate graph structure to SVG.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(svg(svg_generic)).
:- use_module(standards(markup)).

:- setting(
  default_surface,
  compound,
  coord(2,[10.0,10.0]),
  'The default surface on which graphs are displayed.'
).
:- setting(
  default_vertex_color,
  atom,
  black,
  'The default vertex color.'
).
:- setting(
  default_vertex_radius,
  float,
  0.1,
  'The default vertex radius in centimeters.'
).



%! export_edge_svg(+Vs:list(vertex), +E_Term:compound, -E_DOM:element) is det.
% Returns the SVG element for the given edge term.
%
% @arg Vs A list of vertices.
% @arg E A compound term of the form `edge(FromV_Id, ToV_Id, Attrs)`.
% @arg E_DOM An SVG element.

export_edge_svg(Vs, edge(FromV/FromV_Id, ToV/ToV_Id, E_Attrs1), E_DOM):-
  % Ids.
  nth0(FromV_Id, Vs, FromV),
  nth0(ToV_Id, Vs, ToV),
  
  % X1 and Y1.
  nth0(FromV_Id, Vs, vertex(FromV_Id, FromV_Attrs)),
  option(coord(coord(2,[X1,Y1])), FromV_Attrs),
  
  % X2 and Y2.
  nth0(ToV_Id, Vs, vertex(ToV_Id, ToV_Attrs)),
  option(coord(coord(2,[X2,Y2])), ToV_Attrs),
  maplist(format_number(cm), [X1,Y1,X2,Y2], [X1_cm,Y1_cm,X2_cm,Y2_cm]),
  
  % Name.
  select_option(label(E_Name), E_Attrs1, E_Attrs2, nolabel),
  
  % Color
  setting(default_vertex_color, DefaultColor),
  select_option(color(E_Color), E_Attrs2, _E_Attrs3, DefaultColor),
  LineAttrs = [stroke(E_Color)],
  
  line(LineAttrs, X1_cm, Y1_cm, X2_cm, Y2_cm, E_Name, E_DOM).

%! export_graph_svg(+G_Term:compound, -G_DOM:dom) is det.
% Writes a GraphViz structure to an output stream.
%
% @arg G_Term An intermediate graph representation:
%      `graph(V_Terms, E_Terms, G_Attrs)`.
% @arg G_DOM SVG DOM representation of the graph.
%
% @tbd Add support for ranks.
% @tbd Use graph name.

export_graph_svg(graph(Vs, _Ranks, Es, G_Attrs1), SVG_Root):-
  % Graph name.
  %%%%select_option(label(G_Name), G_Attrs1, G_Attrs2, nolabel),
  G_Attrs2 = G_Attrs1,
  
  % Graph surface.
  setting(default_surface, DefaultSurface),
  select_option(surface(Surface), G_Attrs2, DefaultSurface),
  svg_head(Surface, SVG_Head),
  
  % Unranked vertices.
  maplist(export_vertex_svg(Vs), Vs, Vs_DOM),
  
  % Unranked edges.
  maplist(export_edge_svg(Vs), Es, Es_DOM),
  append(Vs_DOM, Es_DOM, SVG_Body),
  root_element(svg, SVG_Head, SVG_Body, SVG_Root).

%! export_vertex_svg(
%!   +Vertices:ordset(vertex),
%!   +VertexTerm:compound,
%!   -Vertex_DOM:element
%! ) is det.

export_vertex_svg(Vs, V, V_DOM):-
  % Id.
  nth0(V_Id, Vs, V),
  V = vertex(V_Id, V_Attrs1),
  
  % X0 and Y0.
  select_option(coord(coord(2,[X0,Y0])), V_Attrs1, V_Attrs2),
  
  % Radius.
  setting(default_vertex_radius, DefaultR),
  select_option(radius(R), V_Attrs2, V_Attrs3, DefaultR),
  maplist(format_number(cm), [X0,Y0,R], [X0_cm,Y0_cm,R_cm]),
  
  % Name.
  select_option(label(Name), V_Attrs3, V_Attrs4, nolabel),
  
  % Color.
  setting(default_vertex_color, DefaultColor),
  select_option(color(V_Color), V_Attrs4, DefaultColor),
  CircleAttrs = [stroke(V_Color)],
  
  circle(CircleAttrs, X0_cm, Y0_cm, R_cm, Name, V_DOM).

