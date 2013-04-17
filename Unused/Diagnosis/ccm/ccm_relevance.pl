:- module(
  ccm_relevance,
  [
    backward/2, % +ToPointCloud:point_cloud
                % ?FromPointCloud:point_cloud
    backward/3, % +ToPointCloud:point_cloud
                % +FromPointCloud:point_cloud
                % -Path:list(point_cloud)
    forward/2, % +FromPointCloud:point_cloud
               % ?ToPointCloud:point_cloud
    forward/3 % +FromPointCloud:point_cloud
              % +ToPointCloud:point_cloud
              % -Path:list(point_cloud)
  ]
).

/** <module> The Component Connection Model relevance module.

This module contains methods for filtering the CCM based on some relevance
criterion.

Calculates which components are relevant from a given set of components and
a given set of points.

@author Wouter Beek
@version Aug 2011 - Feb 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).



backward(ToPointCloud, FromPointCloud):-
  backward(ToPointCloud, FromPointCloud, _Path).

backward(ToPointCloud, FromPointCloud, Path):-
  backward(ToPointCloud, FromPointCloud, [], Path).

backward(PointCloud, PointCloud, Path, Path):-
  !.
backward(ToPointCloud, FromPointCloud, Path, Solution):-
  component_output_point_cloud(Component, ToPointCloud),
  component_input_point_cloud(Component, PointCloud),
  \+(member(ToPointCloud, Path)),
  backward(PointCloud, FromPointCloud, [ToPointCloud | Path], Solution).

forward(FromPointCloud, ToPointCloud):-
  forward(FromPointCloud, ToPointCloud, _Path).

forward(FromPointCloud, ToPointCloud, Path):-
  forward(FromPointCloud, ToPointCloud, [], Path).

forward(PointCloud, PointCloud, Path, Path):-
  !.
forward(FromPointCloud, ToPointCloud, Path, Solution):-
  point_cloud_output_component(FromPointCloud, Component),
  component_output_point_cloud(Component, PointCloud),
  \+(member(FromPointCloud, Path)),
  forward(PointCloud, ToPointCloud, [FromPointCloud | Path], Solution).
