:- module(
  ugraph_export,
  [
    export_ugraph/2, % +Graph:ugraph
                     % -GraphTerm:compound
    export_ugraph/3, % +Options:list(nvpair)
                     % +Graph:ugraph
                     % -GraphTerm:compound
    export_ugraph/4 % +Options:list(nvpair)
                    % :CoordFunc
                    % +Graph:ugraph
                    % -GraphTerm:compound
  ]
).

/** <module> UGRAPH_EXPORT

Predicates for exporting undirected graphs.

# The intermediate graph format

  graph(Vs, Ranks, Es, G_Attrs)

The following attributes are supported:
  * `label(Name:atom)`

## Ranks format

  rank(RankVertex, ContentVertices)

## Vertex format

  vertex(V_Id, V_Attrs)

The following attributes are supported:
  * `color(Color:atom)`
  * `coord(Coord:coord)`
  * `label(Name:atom)`
  * `radius(Radius:float)`

## Edge format

  edge(FromV/FromV_Id, ToV/ToV_Id, E_Attrs)

The following attributes are supported:
  * `arrowhead(ArrowType:atom)`
  * `color(Color:atom)`
  * `label(Name:atom)`
  * `style(Style:atom)`

@author Wouter Beek
@version 2013/02-2013/03, 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(ugraph(ugraph_ext)).

:- meta_predicate(export_ugraph(+,4,+,+)).
:- meta_predicate(export_ugraph_vertex(+,+,4,+,-)).

:- rdf_meta(export_ugraph_vertex(+,+,:,r,-)).

:- setting(
  charset,
  oneof(['UTF-8']),
  'UTF-8',
  'The default encoding for undirected graph exports.'
).
:- setting(
  colorscheme,
  oneof([none,svg,x11]),
  x11,
  'The default colorscheme used for exporting undirected graphs.'
).
:- setting(
  edge_name,
  atom,
  ugraph_edge_name,
  'The predicate for assigning names to edges.'
).
:- setting(
  fontsize,
  float,
  11.0,
  'The default font size for text that occurs in undirected graph exports.'
).
:- setting(
  overlap,
  boolean,
  false,
  'The default for overlap in undirected graph exports.'
).
:- setting(radius, float, 0.1, 'The default radius of vertices.').



% GRAPH %

%! export_ugraph(+Graph:ugraph, -GraphTerm:compound) is det.
% @see Wrapper around export_ugraph/4.

export_ugraph(G, G_Term):-
  export_ugraph([], G, G_Term).

%! export_ugraph(+Graph:ugraph, -GraphTerm:compound) is det.
% @see Wrapper around export_ugraph/4.

export_ugraph(O, G, G_Term):-
  export_ugraph(O, random_vertex_coordinate, G, G_Term).

%! export_ugraph(
%!   +Options:list(nvpair),
%!   :CoordFunc,
%!   +Graph:ugraph,
%!   -GraphTerm:compound
%! ) is det.
% Exports the given unordered graph to the intermediate graph format.
% The following options are supported:
%   1. `border(+Border:coord)`
%   2. `charset(+Characterset:atom)`
%      The name of the character set that is used to encode text in the graph
%      (default `UTF-8`).
%   3. `colorscheme(+Colorscheme:atom)`
%      The name of the colorscheme that is used for colors in the graph
%      (default `x11`).
%   4. `edge_labels(+IncludeEdgeLabels:boolean)`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).
%   5. `fontsize(+FontSize:float)`
%      The font size of text in the graph (default `11.0`).
%   6. `name(+GraphName:atom)`
%      The atomic name of the graph.
%      Defaults to the atomic representation of the graph term.
%   7. `overlap(+Overlap:boolean)`
%      Whether the vertices are allowed to overlap (default `false`).
%   8. `surface(+Surface:coord)`
%      The 2D surface on which the graph is drawn.
%      The default surface is `10.0` cubic.
%
% @arg Options A list of nane-value pairs.
% @arg CoordFunc A function that maps vertices to coordinates.
% @arg Graph An undirected graph.
% @arg GraphTerm A compound term in the intermediate graph format.

export_ugraph(O, CoordFunc, G, graph(V_Terms, E_Terms, G_Attrs)):-
  % Vertices.
  ugraph_vertices(G, Vs),
  maplist(export_ugraph_vertex(O, Vs, CoordFunc), Vs, V_Terms),

  % Edges.
  ugraph_edges(G, Es),
  maplist(export_ugraph_edge(O, Vs), Es, E_Terms),

  % Graph properties.
  ugraph_name(O, G, G_Name),
  setting(charset, DefaultCharset),
  option(charset(Charset), O, DefaultCharset),
  setting(colorscheme, DefaultColorscheme),
  option(colorscheme(Colorscheme), O, DefaultColorscheme),
  setting(fontsize, DefaultFontSize),
  option(fontsize(FontSize), O, DefaultFontSize),
  setting(overlap, DefaultOverlap),
  option(overlap(Overlap), O, DefaultOverlap),
  G_Attrs = [
    charset(Charset),
    colorscheme(Colorscheme),
    fontsize(FontSize),
    label(G_Name),
    overlap(Overlap)
  ].



% EDGE EXPORT %

%! export_ugraph_edge(
%!   +Options:list(nvpair),
%!   +Vertices:ordset(vertex),
%!   +Edge:edge,
%!   -EdgeTerm:compound
%! ) is det.
% The following options are supported:
%   1. `edge_labels(+IncludeEdgeLabels:boolean)`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).

export_ugraph_edge(O, Vs, FromV-ToV, edge(FromV_Id, ToV_Id, E_Attrs)):-
  nth0(FromV_Id, Vs, FromV),
  nth0(ToV_Id, Vs, ToV),
  ugraph_edge_arrow_head(FromV-ToV, E_ArrowHead),
  ugraph_edge_color(FromV-ToV, E_Color),
  ugraph_edge_name(O, FromV-ToV, E_NameOptions),
  ugraph_edge_style(FromV-ToV, E_Style),
  E_Attrs = [
    arrowhead(E_ArrowHead),
    color(E_Color),
    style(E_Style)
  | E_NameOptions
  ].

%! ugraph_edge_arrow_head(+E:edge, -ArrowType:atom) is det.
% @arg ArrowType One of the following values:
%      * `box`
%      * `crow`
%      * `diamond`
%      * `dot`
%      * `ediamond`
%      * `empty`
%      * `halfopen`
%      * `inv`
%      * `invodot`
%      * `normal`
%      * `invdot`
%      * `invempty`
%      * `none`
%      * `odiamond`
%      * `obox`
%      * `odot`
%      * `open`
%      * `tee`
%      * `vee`

ugraph_edge_arrow_head(_FromV-_ToV, normal).

ugraph_edge_color(_FromV-_ToV, black).

ugraph_edge_name(FromV-ToV, EdgeName):-
  maplist(ugraph_vertex_name, [FromV, ToV], [FromV_Name, ToV_Name]),
  phrase(ugraph_edge_name(FromV_Name, ToV_Name), Codes),
  atom_codes(EdgeName, Codes).

%! ugraph_edge_name(+Options:list(nvpair), +Edge:edge, -EdgeName:atom) is det.
% Returns a name for the given edge.
% The following options are supported:
%   1. `edge_labels(+IncludeEdgeLabels:boolean)`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).
%   2. `edge_name(:Goal)`
%      The predicate that is used to assign names to edges.

% Edge names are enabled.
ugraph_edge_name(O, Edge, [label(EdgeName)]):-
  option(edge_labels(true), O, true), !,
  setting(edge_name, DefaultP),
  option(edge_name(P), O, DefaultP),
  call(P, Edge, EdgeName).
% Edge names are disabled.
ugraph_edge_name(_O, _FromV-_ToV, []).

ugraph_edge_name(FromV_Name, ToV_Name) -->
  {atom_codes(FromV_Name, FromV_NameCodes)},
  FromV_NameCodes,
  space,
  arrow([head(both)], 3),
  space,
  {atom_codes(ToV_Name, ToV_NameCodes)},
  ToV_NameCodes.

%! ugraph_edge_style(+E:edge, -Style:atom) is det.
% @arg Style One of the following values:
%      * `bold`
%      * `dashed`
%      * `dotted`
%      * `invis`
%      * `solid`
%      * `tapered`

ugraph_edge_style(_FromV-_ToV, solid).

% The graph name was explicitly set.
ugraph_name(O, _UG, Name2):-
  option(name(Name1), O), !,
  term_to_atom(Name1, Name2).
% The graph name is not set, take the atom representing the graph term.
ugraph_name(_O, UG, Name):-
  term_to_atom(UG, Name).



% VERTEX %

%! export_ugraph_vertex(
%!   +Options:list(nvpair),
%!   +Vertices:ordset(vertex),
%!   :CoordFunc,
%!   +Vertex:vertex,
%!   -V_Term:compound
%! ) is det.
% @tbd Add support for the vertex/1 property on vertex terms.

export_ugraph_vertex(O, Vs, CoordFunc, V, vertex(V_Id, V, V_Attrs)):-
  nth0(V_Id, Vs, V),
  ugraph_vertex_color(V, V_Color),
  call(CoordFunc, O, Vs, V, V_Coord),
  %ugraph_vertex_image(V, V_Image),
  ugraph_vertex_name(V, V_Name),
  setting(radius, V_R),
  ugraph_vertex_shape(V, V_Shape),
  V_Attrs = [
    color(V_Color),
    coord(V_Coord),
    %image(V_Image),
    label(V_Name),
    radius(V_R),
    shape(V_Shape)
  ].

ugraph_vertex_color(_V, black).

%ugraph_vertex_image(V, V_Image):-

ugraph_vertex_name(vertex(_Id, V), V_Name):- !,
  ugraph_vertex_name(V, V_Name).
ugraph_vertex_name(V, V_Name):-
  term_to_atom(V, V_Name).

%! ugraph_vertex_shape(+Vertex:vertex, -Shape:atom) is det.
% @arg Vertex
% @arg Shape The following values are supported:
%      `box`
%      `polygon`
%      `ellipse`
%      `oval`
%      `circle`
%      `point`
%      `egg`
%      `triangle`
%      `plaintext`
%      `diamond`
%      `trapezium`
%      `parallelogram`
%      `house`
%      `pentagon`
%      `hexagon`
%      `septagon`
%      `octagon`
%      `doublecircle`
%      `doubleoctagon`
%      `tripleoctagon`
%      `invtriangle`
%      `invtrapezium`
%      `invhouse`
%      `Mdiamond`
%      `Msquare`
%      `Mcircle`
%      `rect`
%      `rectangle`
%      `square`
%      `star`
%      `none`
%      `note`
%      `tab`
%      `folder`
%      `box3d`
%      `component`
%      `promoter`
%      `cds`
%      `terminator`
%      `utr`
%      `primersite`
%      `restrictionsite`
%      `fivepoverhang`
%      `threepoverhang`
%      `noverhang`
%      `assembly`
%      `signature`
%      `insulator`
%      `ribosite`
%      `rnastab`
%      `proteasesite`
%      `proteinstab`
%      `rpromoter`
%      `rarrow`
%      `larrow`
%      `lpromoter`

ugraph_vertex_shape(_V, ellipse).

