:- module(
  vertex_coordinate,
  [
    vertice_coordinates_to_size/3, % +Options:list(nvpair)
                                   % +VertexCoords:list(vertex_coord)
                                   % -Size:size
    vertice_coordinates_table//1, % +VertexCoords:list(vertex_coord)
    vertice_coordinates_web//2 % +Graph:graph
                               % +VertexCoords:list(vertex_coord)
  ]
).

/** <module> Vertex coordinate

Methods for processing and geenrating vertex coordinates.

These are used for visualizing graphs and for calculating the vertex
positions in iterations of spring embedding.

@author Wouter Beek
@version 2013/01, 2013/07, 2014/03
*/

:- use_module(generics(list_ext)).
:- use_module(graph_theory(graph_export)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(xml(xml_dom)).



%! vertice_coordinates_to_size(
%!   +Options:list(nvpair),
%!   +VCs:list(vertex_coord),
%!   -Size:size
%! ) is det.
% Returns the size structure that is big enough to display the given vertex
% coordinates. This means that the size has the following properties:
%     1. Its dimension is the same as the dimension of each vertex coordinate
%        (we assume the vertex coordinates are all in the same dimension).
%     2. Each of its subsizes (for each dimension one) starts at 0.
%     3. Each of its subsizes (for each dimension one) can represent every
%        of the given vertex coordinates.
%     4. Each of its subsizes (for each dimension one) has a border on both
%        sides of the spectrum. The border is either given or of the default
%        size.

vertice_coordinates_to_size(Options, VCs, size(Dimension, Limits)):-
  % Retrieve the dimension of the vertex coordinates, so that the default
  % borders can be generated.
  memberchk(vertex_coord(_, coordinate(Dimension, _)), VCs),
  repeating_list(0.5, Dimension, DefaultBorders),
  option(
    border_size(size(Dimension, Borders)),
    Options,
    size(Dimension, DefaultBorders)
  ),

  % For all indices =I=, find the maximum coordinate value for that index.
  findall(
    MaxI,
    (
      % We also use the borders for walking through all dimension used in VCs.
      nth0(I, Borders, Border),
      findall(
        Arg,
        (
          member(vertex_coord(_, coordinate(Dimension, Args)), VCs),
          nth0(I, Args, Arg)
        ),
        Args
      ),
      max_list(Args, MaxCVI),
      MaxI is MaxCVI + (2 * Border)
    ),
    Limits
  ).

%! vertice_coordinates_table(+VertexCoords:list(vertex_coord))// is det.
% Generates an HTML table showing vertex coordinates.

vertice_coordinates_table(VertexCoords) -->
  {
    % Generate the header row for the table.
    memberchk(
      vertex_coord(_Vertex, coordinate(MaxDimension, _Coordinates)),
      VertexCoords
    ),
    MaxDimension0 is MaxDimension - 1,
    findall(
      DimensionName,
      (
        between(0, MaxDimension0, Dimension),
        format(atom(DimensionName), 'Dimension ~w', [Dimension])
      ),
      DimensionNames
    ),
    % Generate the data rows for the table.
    findall(
      [Vertex|Coordinates],
      member(
        vertex_coord(Vertex, coordinate(_,Coordinates)),
        VertexCoords
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html('Vertex coordinates'),
    [['Vertex'|DimensionNames]|Rows]
  ).

%! vertice_coordinates_web(
%!   +Graph:graph,
%!   +VertexCoords:list(vertex_coord)
%! )// is det.
% Generates the markup for the given graph and vertex coordinates.

vertice_coordinates_web(Graph, VertexCoords) -->
  {
    export_graph(
      [
        out(svg),
        vertex_coord(lookup_vertice_coordinates),
        vertice_coordinates(VertexCoords)
      ],
      Graph,
      SvgDom
    )
  },
  html(\xml_dom_as_atom(SvgDom)).

lookup_vertice_coordinates(Options, Vertex, Coordinate):-
  option(vertice_coordinates(VertexCoords), Options),
  memberchk(vertex_coord(Vertex, Coordinate), VertexCoords).

