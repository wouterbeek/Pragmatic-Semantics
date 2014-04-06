:- module(
  svg_generic,
  [
    svg_head/2, % +Size:size
                % -Head:list
    svg_head/3, % +Width:number
                % +Height:number
                % -Head:list
    svg_namespace//1 % :DCG_Namespace
  ]
).

/** <module> SVG generic

@author Wouter Beek
@version 2012/10, 2013/01-2013/08
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(db_ext)).
:- use_module(standards(markup)).

:- meta_predicate(svg_namespace(//,?,?)).

:- dynamic(user:mime_type/2).
:- dynamic(user:public_identifier/2).
:- dynamic(user:system_identifier/2).

% SVG MIME type.
:- db_add_novel(user:mime_type(svg, 'image/svg+xml')).
% SVG public identifier.
:- db_add_novel(
  user:public_identifier(svg, 'PUBLIC "-//W3C//DTD SVG 1.1//EN"')).
% SVG system identifier.
:- db_add_novel(
  user:system_identifier(
    svg,
    'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'
  )
).



%! svg_head(+Size:size, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given 2D size.
%
% @see Wrapper around svg_head/3.

svg_head(size(2,[Width,Height]), Head):-
  svg_head(Width, Height, Head).

%! svg_head(+Width:integer, +Height:integer, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given
% height and width.

svg_head(Width, Height, [height=Height_cm, width=Width_cm]):-
  format_number(cm, Width, Width_cm),
  format_number(cm, Height, Height_cm).

svg_namespace(DCG_Namespace) -->
  {phrase(DCG_Namespace, "svg")},
  void.
svg_namespace(DCG_Namespace) -->
  DCG_Namespace.
