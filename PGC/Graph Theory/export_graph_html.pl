:- module(
  graph_export_html,
  [
    export_graph_html/2 % +GraphTerm:compound
                        % -Graph_DOM:list
  ]
).

/** <module> GRAPH_EXPORT_SVG

Predicates for exporting an intermediate graph structure to an HTML table.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_generic)).
:- use_module(html(html_dcg)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(
  default_border_width,
  integer,
  1,
  'The default width of the HTML table in pixels.'
).
:- setting(
  default_edge_color,
  atom,
  black,
  'The default color for edges.'
).
:- setting(
  default_vertex_color,
  atom,
  black,
  'The default color for vertices.'
).



%! export_graph_html(+GraphTerm:compound,-Graph_DOM:list) is det.

export_graph_html(graph(V_Terms, _Ranks, E_Terms, G_Attrs1), G_DOM):-
  % Header row: include when specified in options.
  % Display edge labels when specified in the options.
  select_option(edge_labels(DisplayEdgeLabels), G_Attrs1, G_Attrs2, none),
  (
    DisplayEdgeLabels == none
  ->
    HeaderRow = element(tr, [], [
                  element(th, [], ['From']),
                  element(th, [], ['To'])])
  ;
    HeaderRow = element(tr, [], [
                  element(th, [], ['From']),
                  element(th, [], ['Edge']),
                  element(th, [], ['To'])])
  ),
  setting(default_border_width, DefaultBorderWidth),
  select_option(border_width(BorderWidth), G_Attrs2, DefaultBorderWidth),
  maplist(export_edge_html(V_Terms), E_Terms, E_Rows),
  % The HTML property that sets the border width is called `border`.
  dcg_phrase(html_style([border(BorderWidth)]), StyleContents),
  G_DOM = element(table, [style(StyleContents)], [HeaderRow | E_Rows]).

%! export_edge_html(
%!   +V_Terms:list(compound),
%!   +E_Term:compound,
%!   -E_DOM:list
%! ) is det.

export_edge_html(V_Terms, edge(FromV_Id, ToV_Id, E_Attrs1), E_DOM):-
  setting(default_vertex_color, DefaultVColor),
  
  % From vertex.
  nth0(FromV_Id, V_Terms, vertex(FromV_Id, FromV_Attrs1)),
  % From vertex color.
  select_option(color(FromV_Color), FromV_Attrs1, FromV_Attrs2, DefaultVColor),
  dcg_phrase(html_style([color(FromV_Color)]), FromV_Style),
  % From vertex name.
  select_option(label(FromV_Name), FromV_Attrs2, _FromV_Attrs3, nolabel),
  FromV_Cell = element(td, [style(FromV_Style)], [FromV_Name]),
  
  % To vertex.
  nth0(ToV_Id, V_Terms, vertex(ToV_Id, ToV_Attrs1)),
  % To vertex color.
  select_option(color(ToV_Color), ToV_Attrs1, ToV_Attrs2, DefaultVColor),
  dcg_phrase(html_style([color(ToV_Color)]), ToV_Style),
  % To vertex name.
  select_option(label(ToV_Name), ToV_Attrs2, _ToV_Attrs3, nolabel),
  ToV_Cell = element(td, [style(ToV_Style)], [ToV_Name]),
  
  % Edge, if set in options.
  select_option(edge_labels(DisplayEdgeLabels), E_Attrs1, E_Attrs2, none),
  (
    DisplayEdgeLabels == none
  ->
    V_Cells = [FromV_Cell, ToV_Cell]
  ;
    % Edge color.
    setting(default_edge_color, DefaultEColor),
    select_option(color(E_Color), E_Attrs2, E_Attrs3, DefaultEColor),
    dcg_phrase(html_style(color(E_Color)), E_Style),
    % Edge name.
    select_option(label(E_Name), E_Attrs3, _E_Attrs4, nolabel),
    E_Cell = element(td, [style(E_Style)], [E_Name]),
    V_Cells = [FromV_Cell, E_Cell, ToV_Cell]
  ),
  
  E_DOM = element(tr, [], V_Cells).

