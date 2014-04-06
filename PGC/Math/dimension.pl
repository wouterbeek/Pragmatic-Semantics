:- module(
  dimension,
  [
    dimension_scale/3 % +ContentCube:list(float)
                      % +ContainerCube:list(float)
                      % -ScaledCube:list(float)
  ]
).

/** <module> Dimension

Operations in dimensions.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_remote_module(math(float_ext)).



%! dimension_scale(
%!   +ContentCube:list(float),
%!   +ContainerCube:list(float),
%!   -ScaledCube:list:list(float)
%! ) is det.
% @throws domain_error(type,term) When content and container cubes
%         are of different size.

dimension_scale(L1, L2, _):-
  \+ same_length(L1, L2), !,
  domain_error(list, L1).
dimension_scale(L1, L2, L3):-
  maplist(float_div, L1, L2, L0),
  max_list(L0, Scale),
  findall(
    Y,
    (
      member(X, L1),
      float_div(X, Scale, Y)
    ),
    L3
  ).

