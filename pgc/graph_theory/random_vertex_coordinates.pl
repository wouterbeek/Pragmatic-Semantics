:- module(
  random_vertex_coordinates,
  [
    random_vertex_coordinate/4, % +Options:list(nvpair)
                                % +Vertices:ordset(vertex)
                                % +Vertex:vertex
                                % -VertexCoords:list(vcoord)
    random_vertex_coordinates/4 % +Options:list(nvpair)
                                % +Graph:atom
                                % :V_P
                                % -VertexCoords:list(vcoord)
  ]
).

/** <module> RANDOM_VERTEX_COORDINATES

Genrates random coordinates for vertices in a graph.

@author Wouter Beek
@version 2013/07, 2014/03
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(random)).
:- use_module(library(settings)).

:- meta_predicate(random_vertex_coordinates(+,+,2,-)).

:- setting(
  default_surface,
  compound,
  size(2,[10.0,10.0]),
  'The default surface to draw graphs on.'
).



%! random_vertex_coordinate(
%!   +Options:list(nvpair),
%!   +Vertices:ordset(vertex),
%!   +Vertex:vertex,
%!   -VertexCoordinate:vcoord
%! ) is det.
%
% @arg Options A list of name-value pairs.
%      1. `surface(Size:size)`
% @arg Vertices
% @arg Vertex
% @arg VertexCoordinate Vertex coordinate.

random_vertex_coordinate(O, _Vs, _V, VCoord):-
  setting(default_surface, DefaultSurface),
  option(surface(Surface), O, DefaultSurface),
  Surface = size(Dimension, Sizes),
  VCoord = coordinate(Dimension, Args),
  maplist(random(1.0), Sizes, Args).

random_vertex_coordinates(O, G, V_P, VCoords):-
  call(V_P, G, Vs),
  maplist(random_vertex_coordinate(O, Vs), Vs, VCoords).

