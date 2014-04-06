:- module(
  svg_elements,
  [
    svg_definitions//3, % -Tree:compound
                        % :DCG_Namespace
                        % ?Attributes:list(compound)
    svg_description//3,
    svg_element//4, % -Tree:compound
                    % :DCG_Namespace
                    % :DCG_Name:atom
                    % ?Attributes:list(compound)
    svg_group//3,
    svg_image//3,
    svg_rectangle//3,
    svg_svg//3,
    svg_switch//3,
    svg_symbol//3,
    svg_title//3,
    svg_use//3
  ]
).

/** <module> SVG_ENTITIES

@author Wouter Beek
@version 2013/07-2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(parse_tree)).
:- use_module(library(plunit)).
:- use_module(svg(svg_generic)).
:- use_module(svg(svg_attributes)).
:- use_module(xml(xml_attributes)).
:- use_module(xml(xml_elements)).

:- meta_predicate(svg_definitions(-,//,?,?,?)).
:- meta_predicate(svg_description(-,//,?,?,?)).
:- meta_predicate(svg_element(-,//,//,?,?,?)).
:- meta_predicate(svg_group(-,//,?,?,?)).
:- meta_predicate(svg_image(-,//,?,?,?)).
:- meta_predicate(svg_rectangle(-,//,?,?,?)).
:- meta_predicate(svg_svg(-,//,?,?,?)).
:- meta_predicate(svg_switch(-,//,?,?,?)).
:- meta_predicate(svg_symbol(-,//,?,?,?)).
:- meta_predicate(svg_title(-,//,?,?,?)).
:- meta_predicate(svg_use(-,//,?,?,?)).



%! svg_definitions(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% SVG allows graphical objects to be defined for later reuse. To do this,
% it makes extensive use of IRI references [RFC3987] to these other objects.
% For example, to fill a rectangle with a linear gradient, you first define
% a `linearGradient` element and give it an ID, as in:
%
% ~~~{.xml}
% <linearGradient id="MyGradient">...</linearGradient>
% ~~~
%
% You then reference the linear gradient as the value of the `fill` property
% for the rectangle, as in:
%
% ~~~
% <rect style="fill:url(#MyGradient)"/>
% ~~~
%
% Some types of element, such as gradients, will not by themselves produce
% a graphical result. They can therefore be placed anywhere convenient.
% However, sometimes it is desired to define a graphical object and prevent
% it from being directly rendered. It is only there to be referenced
% elsewhere. To do this, and to allow convenient grouping defined content,
% SVG provides the `defs` element.
%
% It is recommended that, wherever possible, referenced elements be defined
% inside of a `defs` element. Among the elements that are always referenced:
%   * `altGlyphDef`
%   * `clipPath`
%   * `cursor`
%   * `filter`
%   * `linearGradient`
%   * `marker`
%   * `mask`
%   * `pattern`
%   * `radialGradient`
%   * `symbol`
%
% The `defs` element is a container element for referenced elements.
% For understandability and accessibility reasons, it is recommended that,
% whenever possible, referenced elements be defined inside of a `defs`.
%
% The content model for `defs` is the same as for the `g` element.
%
% To provide some SVG user agents with an opportunity to implement efficient
% implementations in streaming environments, creators of SVG content are
% encouraged to place all elements which are targets of local IRI references
% within a `defs` element which is a direct child of one of the ancestors
% of the referencing element.
%
% ## Example
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="8cm" height="3cm"
%      xmlns="http://www.w3.org/2000/svg" version="1.1">
%   <desc>Local URI references within ancestor's 'defs' element.</desc>
%   <defs>
%     <linearGradient id="Gradient01">
%       <stop offset="20%" stop-color="#39F" />
%       <stop offset="90%" stop-color="#F3F" />
%     </linearGradient>
%   </defs>
%   <rect x="1cm" y="1cm" width="6cm" height="1cm"
%         fill="url(#Gradient01)"  />
%
%   <!-- Show outline of canvas using 'rect' element -->
%   <rect x=".01cm" y=".01cm" width="7.98cm" height="2.98cm"
%         fill="none" stroke="blue" stroke-width=".02cm" />
%
% </svg>
% ~~~

svg_definitions(Tree, DCG_Namespace, Attrs) -->
  svg_element(Tree, DCG_Namespace, word(defs), Attrs).

%! svg_description(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% Each container element or graphics element in an SVG drawing can supply
% a text-only description string. When the current SVG document fragment
% is rendered as SVG on visual media, the description is not rendered
% as part of the graphics.
%
% ## Examples
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg SYSTEM "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg xmlns="http://www.w3.org/2000/svg"
%      version="1.1" width="4in" height="3in">
%   <g>
%     <title>Company sales by region</title>
%     <desc>
%       This is a bar chart which shows
%       company sales by region.
%     </desc>
%     <!-- Bar chart defined as vector data -->
%   </g>
% </svg>
%
% At most one description element may appear as a child of any particular
% element.
% All description elements must appear before any other child elements
% (except possibly `metadata` elements) or character data content.
%
% ## Markup
%
% Description and title elements can contain marked-up text from other
% namespaces. For example:
%
% ~~~{.xml}
% <?xml version="1.0" standalone="yes"?>
% <svg xmlns="http://www.w3.org/2000/svg"
%      version="1.1" width="4in" height="3in">
%   <desc xmlns:mydoc="http://example.org/mydoc">
%     <mydoc:title>This is an example SVG file</mydoc:title>
%     <mydoc:para>The global description uses markup from the
%       <mydoc:emph>mydoc</mydoc:emph> namespace.</mydoc:para>
%   </desc>
%   <g>
%     <!-- the picture goes here -->
%   </g>
% </svg>
% ~~~

svg_description(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(desc), Attrs),
  {parse_tree(description, Trees, Tree)}.

svg_element(Trees, DCG_Namespace, DCG_Name, Attrs1) -->
  {xml_inject_attributes(svg_namespace(DCG_Namespace), Attrs1, Attrs2, Trees)},
  xml_element(svg_namespace(DCG_Namespace), DCG_Name, Attrs2).

%! svg_group(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The `g` element is a container element for grouping together
% related graphics elements.
%
% Grouping constructs, when used in conjunction with the `desc` and `title`
% elements, provide information about document structure and semantics.
% Documents that are rich in structure may be rendered graphically, as speech,
% or as braille, and thus promote accessibility.
%
% A group of elements, as well as individual objects, can be given a name
% using the `id` attribute. Named groups are needed for several purposes
% such as animation and re-usable objects.
%
% ## Example
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg xmlns="http://www.w3.org/2000/svg"
%      version="1.1" width="5cm" height="5cm">
%   <desc>Two groups, each of two rectangles</desc>
%   <g id="group1" fill="red">
%     <rect x="1cm" y="1cm" width="1cm" height="1cm"/>
%     <rect x="3cm" y="1cm" width="1cm" height="1cm"/>
%   </g>
%   <g id="group2" fill="blue">
%     <rect x="1cm" y="3cm" width="1cm" height="1cm"/>
%     <rect x="3cm" y="3cm" width="1cm" height="1cm"/>
%   </g>
%
%   <!-- Show outline of canvas using 'rect' element -->
%   <rect x=".01cm" y=".01cm" width="4.98cm" height="4.98cm"
%         fill="none" stroke="blue" stroke-width=".02cm"/>
% </svg>
% ~~~

svg_group(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(group), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_image(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The image element indicates that the contents of a complete file are to be
% rendered into a given rectangle within the current user coordinate system.
% The image element can refer to raster image files such as PNG or JPEG or to
% files with MIME type of `image/svg+xml`. Conforming SVG viewers need to
% support at least PNG, JPEG and SVG format files.
%
% ## Color channels
%
% The result of processing an image is always a four-channel RGBA result.
% When an `image` element references a raster image file such as PNG or JPEG
% files which only have three channels (RGB), then the effect is as if the
% object were converted into a 4-channel RGBA image with the alpha channel
% uniformly set to `1`. For a single-channel raster image, the effect is as if
% the object were converted into a 4-channel RGBA image, where the single
% channel from the referenced object is used to compute the three color
% channels and the alpha channel is uniformly set to `1`.
%
% ## Viewport
%
% An `image` element establishes a new viewport for the referenced file.
% The bounds for the new viewport are defined by attributes `x`, `y`,
% `width` and `height`. The placement and scaling of the referenced image
% are controlled by the `preserveAspectRatio` attribute.
%
% ## Overruled SVG properties
%
% When an `image` element references an SVG image, the `clip` and `overflow`
% properties on the root element in the referenced SVG image are ignored
% (in the same manner as the `x`, `y`, `width` and `height` attributes are
% ignored). Unless the value of `preserveAspectRatio` on the `image` element
% starts with `defer`, the `preserveAspectRatio` attribute on the root
% element in the referenced SVG image is also ignored. Instead, the
% `preserveAspectRatio` attribute on the referencing `image` element defines
% how the SVG image content is fitted into the viewport and the `clip` and
% `overflow` properties on the `image` element define how the SVG image
% content is clipped relative to the viewport.
%
% ## Viewbox
%
% The value of the `viewBox` attribute to use when evaluating the
% `preserveAspectRatio` attribute is defined by the referenced content.
%    * For content that clearly identifies a viewbox (e.g. an SVG file with
%      the `viewBox` attribute on the outermost svg element) that value should
%      be used.
%    * For most raster content (PNG, JPEG) the bounds of the image should be
%      used.
%    * Where no value is readily available (e.g. an SVG file with no `viewBox`
%      attribute on the outermost svg element) the `preserveAspectRatio`
%      attribute is ignored, and only the translation due to the `x` and `y`
%      attributes of the viewport is used to display the content.
%
% The resource referenced by the `image` element represents a separate
% document which generates its own parse tree and DOM (if XML).
%
% ## Example
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="4in"
%      height="3in"
%      version="1.1"
%      xmlns="http://www.w3.org/2000/svg"
%      xmlns:xlink="http://www.w3.org/1999/xlink">
%   <desc>This graphic links to an external image
%   </desc>
%   <image x="200"
%          y="200"
%          width="100px"
%          height="100px"
%          xlink:href="myimage.png">
%     <title>My image</title>
%   </image>
% </svg>
% ~~~
%
% ## Attributes
%
% The following attributes are supported:
%   1. =|svg_height(Amount:float,Unit:atom)|=
%      The height of the rectangular region into which the referenced
%      document is placed. `0` does not render the image.
%   2. =|svg_preserve_aspect_ratio(?Defer:boolean,?Align:compound,?MeetOrSlice:oneof([meet,slice]))|=
%      Default `xMidyMid meet`.
%   3. =|svg_width(?Amount:float,?Unit:atom)|=
%      The width of the rectangular region into which the referenced
%      document is placed. `0` does not render the image.
%   4. =|svg_x(?Amount:float,?Unit:atom)|=
%      The x-axis coordinate of one corner of the rectangular region into
%      which the referenced document is placed. Default `0`.
%   5. =|svg_y(?Amount:float,?Unit:atom)|=
%      The y-axis coordinate of one corner of the rectangular region into
%      which the referenced document is placed. Default `0`.
%   6. =|xlink_href(?IRI:iri)|=

svg_image(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(image), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_rectangle(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The following attributes are supported:
%   1. =|svg_height(Amount:float,Unit:atom)|=
%   2. =|svg_fill(?Fill:oneof([none]))|=
%   3. =|svg_stroke(?Color:oneof([blue]))|=
%   4. =|svg_stroke_width(?Amount:float,?Unit:atom)|=
%   5. =|svg_width(?Amount:float,?Unit:atom)|=
%   6. =|svg_x(?Amount:float,?Unit:atom)|=
%   7. =|svg_y(?Amount:float,?Unit:atom)|=

svg_rectangle(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(rect), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_svg(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The following attrobutes are supported:
%   1. =|svg_base_profile(?ProfileName:atom)|=
%   2. =|svg_content_script_type(MediaType:atom)|=
%   3. =|svg_content_style_type(MediaType:atom)|=
%   4. =|svg_height(?Number:float,?Unit:atom)|=
%   5. =|svg_preserve_aspect_ratio(?Defer:boolean,?Align:compound,?MeetOrSlice:oneof([meet,slice]))|=
%   6. =|svg_stroke(?Color:atom)|=
%   7. =|svg_stroke_width(?Number:float,?Unit:atom)|=
%   1. =|svg_standalone(?Alone:boolean)|=
%   2. =|svg_version(?Major:integer,?Minor:integer)|=

svg_svg(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(svg), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_switch(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The switch element evaluates the `requiredFeatures`, `requiredExtensions`
% and `systemLanguage` attributes on its direct child elements in order,
% and then processes and renders the first child for which these attributes
% evaluate to `true`. All others will be bypassed and therefore not rendered.
% If the child element is a container element such as `g`, then the entire
% subtree is either processed/rendered or bypassed/not rendered.

svg_switch(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(switch), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_symbol(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The symbol element is used to define graphical template objects which can be
% instantiated by a `use` element.
%
% The key distinctions between symbol and a group (i.e., `g`) elements are:
%   1. A symbol element is not rendered. Only instances of a symbol element
%      (i.e., a reference to a symbol by a `use` element) are rendered.
%   2. A symbol element has attributes `viewBox` and `preserveAspectRatio`
%      which allow a symbol to scale-to-fit within a rectangular viewport
%      defined by the referencing `use` element.
%
% The following attributes are supported:
%   1. =|svg_preserve_aspect_ratio(?Defer:boolean,?Align:compound,?MeetOrSlice:oneof([meet,slice]))|=

svg_symbol(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(symbol), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_title(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The `title` child element to an `svg` element identifies the content
% of its direct parent SVG document fragment using textual content.
%
% Authors should always provide a `title` child element to the outermost
% SVG element within a stand-alone SVG document.
%
% At most one title element may appear as a child of any particular element.
% All title elements must appear before any other child elements
% (except possibly `metadata` elements) or character data content.
% Any title element should be the first child element of its parent.
%
% Since users often consult documents out of context, authors should provide
% context-rich titles. Example: not [1] but [2].
%
% ~~~{.txt}
% [1] Introduction
% [2] Introduction to Medieval Bee-Keeping
% ~~~
%
% For reasons of accessibility, user agents should always make the content
% of the `title` child element to the outermost svg element available
% to users.
%
% @see Has similarities with svg_description//3.

svg_title(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(title), Attrs),
  {parse_tree(description, Trees, Tree)}.

%! svg_use(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% Any `svg`, `symbol`, `g`, graphics element or other `use` is potentially
% a template object that can be re-used (i.e., instanced) in the SVG document
% via a `use` element. The `use` element references another element and
% indicates that the graphical contents of that element is included/drawn
% at that given point in the document.
%
% Unlike `image`, the `use` element cannot reference entire files.
%
% The optional attributes `x`, `y`, `width` and `height` map the graphical
% contents of the referenced element onto a rectangular region within the
% current coordinate system.
%
% ## DOM
%
% The effect of a `use` element is the inclusion of a clone of the referenced
% element into a separate non-exposed DOM tree which had the `use` element as
% its parent and all of the `use` element's ancestors as its higher-level
% ancestors. The SVG DOM does not contain the cloned content.
%
% ## Style
%
% CSS2 selectors can be applied to the original (i.e., referenced) elements
% because they are part of the formal document structure. CSS2 selectors
% cannot be applied to the (conceptually) cloned DOM tree because its
% contents are not part of the formal document structure.
%
% Property inheritance, however, works as if the referenced element had been
% textually included as a deeply cloned child of the `use` element
% (i.e., the referenced element inherits properties from the `use` element
% and its ancestors, but not from the referenced element's original
% ancestors).
%
% Therefore, two stylings apply:
%   1. The cascaded property values on the `use` element.
%   2. The cascaded property values on the referenced resource, that are
%      present in the inserted `g` element as the functional equivalent of
%      a `style` attribute.
%
% ## Events
%
% If event attributes are assigned to referenced elements, then the actual
% target for the event will be the `SVGElementInstance` object within the
% instance tree corresponding to the given referenced element.
%
% The event handling for the non-exposed tree works as if the referenced
% element had been textually included as a deeply cloned child of the `use`
% element, except that events are dispatched to the `SVGElementInstance`
% objects. The event's `target` and `currentTarget` attributes are set
% to the `SVGElementInstance` that corresponds to the target and current
% target elements in the referenced subtree. An event propagates through the
% exposed and non-exposed portions of the tree in the same manner as it would
% in the regular document tree: first going from the root element to the
% `use` element and then through non-exposed tree elements in the capture
% phase, followed by the target phase at the target of the event, then
% bubbling back through non-exposed tree to the use element and then back
% through regular tree to the root element in bubbling phase.
%
% An element and all its corresponding `SVGElementInstance` objects share
% an event listener list. The `currentTarget` attribute of the event can be
% used to determine through which object an event listener was invoked.
%
% ## Visibility
%
% The behavior of the `visibility` property conforms to this model of
% property inheritance. Thus, specifying `visibility:hidden` on a `use`
% element does not guarantee that the referenced content will not be rendered.
%
% ## Animation
%
% Animations on a referenced element will cause the instances to also be
% animated.
%
% ## Visual effects
%
% A `use` element has the same visual effect as if the `use` element were
% replaced by the following generated content:
%   1. **Symbol**
%      If the `use` element references a `symbol` element, then in the
%      generated content, the `use` will be replaced by `g`, where all
%      attributes from the `use` element except for `x`, `y`, `width`,
%      `height` and `xlink:href` are transferred to the generated `g`
%      element. An additional transformation `translate(x,y)` is appended
%      to the end (i.e., right-side) of the `transform` attribute on the
%      generated `g`, where `x` and `y` represent the values of the
%      `x` and `y` attributes on the `use` element. The referenced `symbol`
%      and its contents are deep-cloned into the generated tree, with the
%      exception that the `symbol` is replaced by an `svg`. This generated
%      `svg` will always have explicit values for attributes `width` and
%      `height`. If attributes `width` and/or `height` are provided on the
%      `use` element, then these attributes will be transferred to the
%      generated `svg`. Otherwise, the generated `svg` element will use
%      values of `100%` for these attributes.
%   2. **SVG**
%      If the `use` element references an `svg` element, then in the generated
%      content, the `use` will be replaced by `g`, where all attributes from
%      the `use` element except for `x`, `y`, `width`, `height` and
%      `xlink:href` are transferred to the generated `g` element.
%      An additional transformation `translate(x,y)` is appended to the end
%      (i.e., right-side) of the `transform` attribute on the generated `g`,
%      where `x` and `y` represent the values of the `x` and `y` attributes
%      on the `use` element. The referenced `svg` and its contents are
%      deep-cloned into the generated tree. If attributes `width` and/or
%      `height` are provided on the `use` element, then these values will
%      override the corresponding attributes on the `svg` in the generated
%      tree.
%   3. **Other**
%      In the generated content, the `use` will be replaced by `g`,
%      where all attributes from the `use` element except for `x`, `y`,
%      `width`, `height` and `xlink:href` are transferred to the generated
%      `g` element. An additional transformation `translate(x,y)` is appended
%      to the end (i.e., right-side) of the `transform` attribute on the
%      generated `g`, where `x` and `y` represent the values of the
%      `x` and `y` attributes on the `use` element. The referenced object
%      and its contents are deep-cloned into the generated tree.
%
% ## Examples
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="10cm" height="3cm" viewBox="0 0 100 30" version="1.1"
%      xmlns="http://www.w3.org/2000/svg"
%      xmlns:xlink="http://www.w3.org/1999/xlink">
%   <desc>Example Use01 - Simple case of 'use' on a 'rect'</desc>
%   <defs>
%     <rect id="MyRect" width="60" height="10"/>
%   </defs>
%   <rect x=".1" y=".1" width="99.8" height="29.8"
%         fill="none" stroke="blue" stroke-width=".2" />
%   <use x="20" y="10" xlink:href="#MyRect" />
% </svg>
% ~~~
%
% ~~~
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="10cm" height="3cm" viewBox="0 0 100 30" version="1.1"
%      xmlns="http://www.w3.org/2000/svg"
%      xmlns:xlink="http://www.w3.org/1999/xlink">
%   <desc>Example Use02 - 'use' on a 'symbol'</desc>
%   <defs>
%     <symbol id="MySymbol" viewBox="0 0 20 20">
%       <desc>MySymbol - four rectangles in a grid</desc>
%       <rect x="1" y="1" width="8" height="8"/>
%       <rect x="11" y="1" width="8" height="8"/>
%       <rect x="1" y="11" width="8" height="8"/>
%       <rect x="11" y="11" width="8" height="8"/>
%     </symbol>
%   </defs>
%   <rect x=".1" y=".1" width="99.8" height="29.8"
%         fill="none" stroke="blue" stroke-width=".2" />
%   <use x="45" y="10" width="10" height="10"
%        xlink:href="#MySymbol" />
% </svg>
% ~~~
%
% ~~~{.xml}
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="10cm" height="3cm" viewBox="0 0 100 30" version="1.1"
%      xmlns="http://www.w3.org/2000/svg"
%      xmlns:xlink="http://www.w3.org/1999/xlink">
%   <desc>Example Use03 - 'use' with a 'transform' attribute</desc>
%   <defs>
%     <rect id="MyRect" x="0" y="0" width="60" height="10"/>
%   </defs>
%   <rect x=".1" y=".1" width="99.8" height="29.8"
%         fill="none" stroke="blue" stroke-width=".2" />
%   <use xlink:href="#MyRect"
%        transform="translate(20,2.5) rotate(10)" />
% </svg>
% ~~~
%
% <?xml version="1.0" standalone="no"?>
% <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
%   "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
% <svg width="12cm" height="3cm" viewBox="0 0 1200 300" version="1.1"
%      xmlns="http://www.w3.org/2000/svg"
%      xmlns:xlink="http://www.w3.org/1999/xlink">
%   <desc>Example Use04 - 'use' with CSS styling</desc>
%   <defs style=" /* rule 9 */ stroke-miterlimit: 10" >
%     <path id="MyPath"
%           d="M300 50 L900 50 L900 250 L300 250"
%           class="MyPathClass"
%           style=" /* rule 10 */ stroke-dasharray:300,100" />
%   </defs>
%   <style type="text/css">
%     <![CDATA[
%       /* rule 1 */ #MyUse { fill: blue }
%       /* rule 2 */ #MyPath { stroke: red }
%       /* rule 3 */ use { fill-opacity: .5 }
%       /* rule 4 */ path { stroke-opacity: .5 }
%       /* rule 5 */ .MyUseClass { stroke-linecap: round }
%       /* rule 6 */ .MyPathClass { stroke-linejoin: bevel }
%       /* rule 7 */ use > path { shape-rendering: optimizeQuality }
%       /* rule 8 */ g > path { visibility: hidden }
%     ]]>
%   </style>
%   <rect x="0"
%         y="0"
%         width="1200"
%         height="300"
%         style="fill:none; stroke:blue; stroke-width:3"/>
%   <g style=" /* rule 11 */ stroke-width:40">
%     <use id="MyUse"
%          xlink:href="#MyPath"
%          class="MyUseClass"
%          style="/* rule 12 */ stroke-dashoffset:50" />
%   </g>
% </svg>
% ~~~
%
% The rules which do not affect the generated content are:
%   * Rules 7 and 8: CSS selectors only apply to the formal document tree,
%     not on the generated tree.
%   * Rule 9: The generated tree only inherits from the ancestors of the
%     `use` element and does not inherit from the ancestors of the referenced
%     element.
%
% ## Attributes
%
% The following attributes are supported:
%   1. =|svg_height(Amount:float,Unit:atom)|=
%      The height of the rectangular region into which the referenced element
%      is placed. `0` disables rendering.
%   2. =|xlink_href(?IRI)|=
%      An IRI reference to an element/fragment within an SVG document.
%   3. =|svg_width(?Amount:float,?Unit:atom)|=
%      The width of the rectangular region into which the referenced element
%      is placed. `0` disables rendering.
%   4. =|svg_x(?Amount:float,?Unit:atom)|=
%      The x-axis coordinate of one corner of the rectangular region into
%      which the referenced element is placed. Default `0`.
%   5. =|svg_y(?Amount:float,?Unit:atom)|=
%      The y-axis coordinate of one corner of the rectangular region into
%      which the referenced element is placed. Default `0`.

svg_use(Tree, DCG_Namespace, Attrs) -->
  svg_element(Trees, DCG_Namespace, word(use), Attrs),
  {parse_tree(description, Trees, Tree)}.
