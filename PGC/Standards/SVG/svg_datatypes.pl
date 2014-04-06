:- module(
  svg_datatypes,
  [
% SVG DATATYPES
    svg_color//2, % -Tree:compound
                  % ?Color:atom
    svg_coordinate//3, % -Tree:compound
                       % ?Amount:float
                       % ?Unit:atom
    svg_extension//2, % -Tree:compound
                      % ?Extension:iri
    svg_length//3, % -Tree:compound
                   % ?Amount:float
                   % ?Unit:atom
    svg_content_type//1, % ?MimeType:atom
    svg_profile_name//2, % -Tree:compound
                         % ?ProfileName:atom
% SVG PARAMETERS
    svg_align//3, % -Tree:compound
                  % ?XAlign:oneof([min,mid,max,none])
                  % ?YAlign:oneof([min,mid,max,none])
    svg_defer//2, % -Tree:compound
                  % ?Defer:boolean
    svg_feature_string//2, % -Tree:compound
                           % ?Feature:atom
    svg_meet_or_slice//2 % -Tree:compound
                         % ?Value:oneof([meet,slice])
  ]
).

/** <module> SVG_DATATYPES

DCGs for SVG datatypes.

@author Wouter Beek
@version 2013/07
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(svg(svg_colors)).



% SVG DATATYPES %

svg_color(color(Color), Color) -->
  {nonvar(Color)}, !,
  {svg_color(Color, _RGB)},
  word(Color).
svg_color(color(Color), Color) -->
  word(Color),
  {svg_color(Color, _RGB)}.

%! svg_coordinate(-Tree:compound, ?Number:float, ?Unit:atom)//
% A coordinate is a length in the user coordinate system that is
% the given distance from the origin of the user coordinate system along
% the relevant axis.
%
% @see Syntax is the same as for svg_length//3.

svg_coordinate(coordinate(number(Number),unit(Unit)), Number, Unit) -->
  svg_length(_T1, Number, Unit).

%! svg_length(-Tree:compund, ?Number:float, ?Unit:atom)//
% A length is a distance measurement, given as a number along with
% a unit which may be optional.
%
% When svg_length//3 is used in an SVG presentation attribute,
% the syntax must match the following pattern:
% ~~~
% length ::=
%   number
%   ("em" | "ex" | "px" | "in" | "cm" | "mm" | "pt" | "pc"| "%")?
% ~~~
%
% The unit identifier, if present, must be in lower case; if not present,
% the length value represents a distance in the current
% user coordinate system.
%
% Note that the non-property svg_length//3 definition also allows
% a percentage unit identifier.
% The meaning of a percentage length value depends on the attribute
% for which the percentage length value has been specified.
% Two common cases are:
%   a. When a percentage length value represents a percentage of
%      the viewport width or height.
%   b. When a percentage length value represents a percentage of
%      the bounding box width or height on a given object.
%
% The following units are supported:
%   1. `cm`
%   2. `em`
%   3. `ex`
%   4. `in`
%   5. `mm`
%   6. `pc`
%   7. `pt`
%   8. `px`
%   9. `%`

svg_length(length(number(Number),unit(Unit)), Number, Unit) -->
  unsigned_number(Number),
  (svg_unit(Unit) ; percent_sign, {Unit = '%'}).

%! svg_content_type(?MediaType:atom)//
%
% @tbd Implement the full RFC 2046 standard for media types.

svg_content_type(MediaType) -->
  word(First),
  forward_slash,
  word(Second),
  {atomic_list_concat([First,'/',Second], MediaType)}.

svg_profile_name(profile_name(none), none) --> "none".
svg_profile_name(profile_name(tiny), tiny) --> "tiny".



% SVG PARAMETERS %

%! svg_align(
%!   -Tree:compound,
%!   ?XAlign:oneof([min,mid,max,none]),
%!   ?YAlign:oneof([min,mid,max,none])
%! )//
% The align parameter indicates whether to force uniform scaling and,
% if so, the alignment method to use in case the aspect ratio
% of svg_view_box// does not match the aspect ratio of the viewport.
% The following values are supported:
%   1. `none`
%      Do not force uniform scaling.
%      Scale the graphic content of the given element non-uniformly
%      if necessary such that the element's bounding box exactly matches
%      the viewport rectangle.
%      The value for `meetOrSlice` is ignored.
%   2. `xMinYMin`
%      Force uniform scaling.
%      Align the svg_min_x// of the element's svg_view_box// with
%      the smallest X value of the viewport.
%      Align the svg_min_y// of the element's svg_view_box// with
%      the smallest Y value of the viewport.
%   3. `xMidYMin`
%      Force uniform scaling.
%      Align the midpoint X value of the element's svg_view_box// with
%      the midpoint X value of the viewport.
%      Align the svg_min_y// of the element's svg_view_box// with
%      the smallest Y value of the viewport.
%    4. `xMaxYMin`
%       Align the svg_min_x// plus svg_width// of the element's
%       svg_view_box// with the maximum X value of the viewport.
%       Align the svg_min_y// of the element's svg_view_box// with
%       the smallest Y value of the viewport.
%    5. `xMinYMid`
%    6. `xMidYMid`
%       This is the default value.
%    7. `xMaxYMid`
%    8. `xMinYMax`
%    9. `xMidYMax`
%    10. `xMaxYMax`

svg_align(align(x(none), y(none)), none, none) --> "none".
svg_align(align(x(X),y(Y)), X, Y) -->
  x_lowercase, svg_min_mid_max(X),
  y_uppercase, svg_min_mid_max(Y).

svg_defer(defer(false), false) --> [].
svg_defer(defer(true), true) --> "defer".

%! svg_extension(-Tree:compound, ?Extension:iri)//
% @tbd Add extensions IRIs. (What are these anyway?)

svg_extension(extension(Extension), Extension) --> word(Extension).

%! svg_feature_string(-Tree:compound, ?Feature:iri)//
% @tbd Add feature string IRIs.

svg_feature_string(feature_string(Feature), Feature) --> word(Feature).

%! svg_meet_or_slice(-Tree:compound, ?Value:oneof([meet,slice]))//
% The scg_meet_or_slice// parameter is optional and, if provided,
% is separated from the svg_align// value by one or more spaces.
%
% The following values are supported:
%   1. `meet` (the default)
%      Scale the graphic such that:
%        * aspect ratio is preserved
%        * the entire `viewBox` is visible within the viewport
%        * the `viewBox` is scaled up as much as possible, while still
%          meeting the other criteria.
%      In this case, if the aspect ratio of the graphic does not match
%      the viewport, some of the viewport will extend beyond the bounds of
%      the `viewBox` (i.e., the area into which the `viewBox` will draw
%      will be smaller than the viewport).
%   2. `slice`
%      Scale the graphic such that:
%        * aspect ratio is preserved
%        * the entire viewport is covered by the `viewBox`
%        * the `viewBox` is scaled down as much as possible,
%          while still meeting the other criteria
%      In this case, if the aspect ratio of the `viewBox` does not match
%      the viewport, some of the `viewBox` will extend beyond the bounds
%      of the viewport (i.e., the area into which the `viewBox` will draw
%      is larger than the viewport).

svg_meet_or_slice(meet_or_slice(meet), meet) --> "meet".
svg_meet_or_slice(meet_or_slice(slice), slice) --> "slice".

svg_min_mid_max(min) --> "Min".
svg_min_mid_max(mid) --> "Mid".
svg_min_mid_max(max) --> "Max".

svg_unit(cm) --> "cm".
svg_unit(em) --> "em".
svg_unit(ex) --> "ex".
svg_unit(in) --> "in".
svg_unit(mm) --> "mm".
svg_unit(pc) --> "pc".
svg_unit(pt) --> "pt".
svg_unit(px) --> "px".

