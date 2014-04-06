:- module(
  circular_graph_representation,
  [
    circular_vertex_coordinate/4 % +Options
                                  % +Vs:list
                                  % +V
                                  % -VCoord:coord
  ]
).

/** <module> CIRCULAR_GRAPH_REPRESENTATION

Circular graph representation.

@author Wouter Beek
@version 2013/07
*/

:- use_module(generics(list_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(standards(markup)).

:- setting(
  default_border_size,
  compound,
  coord(2,[0.5,0.5]),
  'The default border.'
).
:- setting(
  default_surface,
  compound,
  size(2,[10.0,10.0]),
  'The default surface to draw graphs on.'
).



%! circular_vertex_coordinate(+Options, +Vs:list, +V, -Coord:coord) is det.
% Returns the coordinate of the given vertex, so that all vertices in a
% graph are plotted on a circle.
%
% @arg Options Supported values:
%      * `border(+Border:size)`
%      * `surface(+Surface:size)`
% @arg Vertices A list of vertices.
% @arg Vertex A vertex.
% @arg Coord A compound term of the form
%      `coord(Dimension:integer, Coordinates:list)`,
%      where `Dimension` is the length of `Coordinates`.

circular_vertex_coordinate(Options, Vs, V, coordinate(2,[X_cm,Y_cm])):-
  % Surface
  setting(default_surface, DefaultSurface),
  option(surface(size(2, [Width, Height])), Options, DefaultSurface),
  
  % Borders
  setting(default_border_size, DefaultBorderSize),
  option(
    border_size(size(2,[X_Border,Y_Border])),
    Options,
    DefaultBorderSize
  ),
  
  % Graph radius.
  GraphXDiameter is Width - 2 * X_Border,
  GraphYDiameter is Height - 2 * Y_Border,
  GraphDiameter is max(GraphXDiameter, GraphYDiameter),
  GraphRadius is GraphDiameter / 2,

  % V index
  nth0chk(I, Vs, V),

  % Angle
  length(Vs, NumberOfVertices),
  % Specifically cater for the case in which there are no vertices.
  (
    NumberOfVertices == 0
  ->
    AnglePerVertice is 2 * pi
  ;
    AnglePerVertice is 2 * pi / NumberOfVertices
  ),

  % (X,Y)-coordinate.
  X is X_Border + GraphRadius + GraphRadius * cos(I * AnglePerVertice),
  Y is Y_Border + GraphRadius + GraphRadius * sin(I * AnglePerVertice),
  maplist(format_number(cm), [X,Y], [X_cm,Y_cm]).
