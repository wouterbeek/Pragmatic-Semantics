:- module(
  svg_dom,
  [
    svg_document//3, % -Tree:compound
                     % :DCG_Namespace
                     % ?SVG_DCGs:list(dcg)
    svg_fragment//2, % -Tree:compound
                     % +SVG_DCGs:list(dcg)
    svg_fragment//3 % -Tree:compound
                    % :DCG_Namespace
                    % +SVG_DCGs:list(dcg)
  ]
).

/** <module> SVG DOM

Predictaes that allow vector graphics to be drawn according to
the SVG standards.
Including DCG rules implementing the SVG 1.1 Second Edition standard.

SVG is a language for describing two-dimensional graphics in XML.

SVG allows for three types of graphic objects:
  1. Vector graphic shapes
  2. Images
  3. Text

The use of DTDs for validating XML documents is known to be problematic.
In particular, DTDs do not handle namespaces gracefully.
It is *not* recommended that a DOCTYPE declaration be included
in SVG documents.

# Using SVG in Web pages

SVG can be included in Web pages in the following ways:
  1. Stand-alone SVG Web page.
  2. Embed by reference:
    1. Element `img`
    2. Element `object`, allowing different formats to be given
       through nesting.
    3, Element `applet`.
  3. Embed inline.
  4. External link, using element `a`.
  5. Referenced from a CSS or XSL property (e.g., `background-image`,
     `list-style-image`).

# Graphics rendering

**Grouped elements** are rendered in the following steps:
  1. A temporary separate canvas is initialized in transparent black
     onto which child elements are painted.
  2. Filter effects are applied after painting.
  3. The temporary canvas is composited into the background.
     Clipping, masking, and opacity are taken into account.

Individual graphics elements are rendered as if they are singleton groups.

The fundamental graphics elements types:
  1. **Shapes**, lines and curves.
  2. **Text**, character glyphs.
  3. **Raster images**, array of values (paint color and opacity).

Shapes and text can be filled and/or stroked (along the outline,
after filling).

Shapes can contain  **marker symbols** at selected vertices (after stroking).

Raster images have their original sample resampled to the output device.

@author Wouter Beek
@see SVG 1.1 (Second Edition) http://www.w3.org/TR/2011/REC-SVG11-20110816/
@version 2012/10, 2013/01-2013/09
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(svg(svg_generic)).
:- use_remote_module(svg(svg_elements)).
:- use_remote_module(xml(xml_elements)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(svg, 'http://www.w3.org/2000/svg').

:- meta_predicate(svg_document(-,//,?,?,?)).
:- meta_predicate(svg_fragment(-,//,?,?)).
:- meta_predicate(svg_fragment(-,//,//,?,?)).



%! svg_document(-Tree:compound, :DCG_Namespace, ?SVG_DCGs:list(dcg))//

svg_document(T0, DCG_Namespace, SVG_DCGs) -->
  xml_header(T1, DCG_Namespace, version(1,0), true),
  xml_elements(Ts, svg_namespace(DCG_Namespace), SVG_DCGs),
  {parse_tree(document, [T1|Ts], T0)}.

%! svg_fragment(-Tree:compound, +SVG_DCGs:list(dcg))//
% An `xmlns` attribute without a namespace prefix could be specified on an
% `svg` element, which means that SVG is the default namespace for all
% elements within the scope of the element with the `xmlns` attribute.
%
% ~~~{.xml}
% <svg xmlns="http://www.w3.org/2000/svg" …>
%   <rect …/>
% </svg>
% ~~~

svg_fragment(T0, SVG_DCGs) -->
  svg_elements:svg_element(
    T1,
    word(svg),
    word(svg),
    [
      xml_namespace(
        http,
        authority(_User,[www,w3,org],_Port),
        [['2000'],[svg]],
        _Query,
        _Fragment
      )
    ]
  ),
  xml_elements(Ts, void, SVG_DCGs),
  {parse_tree(fragment, [T1|Ts], T0)}.

%! svg_fragment(-Tree:compound, :DCG_Namespace, +SVG_DCGs:list(dcg))//
% If a namespace prefix is specified on the `xmlns` attribute
% (e.g., =|xmlns:svg="http://www.w3.org/2000/svg"|=),
% then the corresponding namespace is not the default namespace,
% so an explicit namespace prefix must be assigned to the elements.
%
% ~~~{.xml}
% <svg:svg xmlns:svg="http://www.w3.org/2000/svg" …>
%   <svg:rect …/>
% </svg:svg>
% ~~~
%
% @arg DCG_Namespace An XML namespace prefix that is not `svg`.

svg_fragment(T0, DCG_Namespace, SVG_DCGs) -->
  % Directly go to XML entity (not via SVG entity).
  xml_element(
    word(svg),
    word(svg),
    [
      xml_attribute(
        word(xmlns),
        word(svg),
        rfc2396_uri_reference(
          T1,
          http,
          authority(_User,[www,w3,org],_Port),
          [['2000'],[svg]],
          _Query,
          _Fragment
        )
      )
    ]
  ),
  xml_elements(Ts, svg_namespace(DCG_Namespace), SVG_DCGs),
  {parse_tree(fragment, [T1|Ts], T0)}.
