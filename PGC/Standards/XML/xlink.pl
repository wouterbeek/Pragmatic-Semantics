:- module(
  xlink,
  [
    xlink_href//3 % -Tree:compound
                  % :DCG_Namespace
                  % +IRI:iri
  ]
).

/** <module> XLink

Support for the XML Linking Language.

Create and describe links between resources from within XML documents.

## Concepts

  * *Arc*
    Information about how to traverse a pair or resources
    (e.g. direction of traversal, application behavior information).
  * *|Ending resource|*
    The destination where traversal ends.
  * *|Extended link|*
    A link that associates an arbitrary number of resources.
  * *Hyperlink*
    A link that is intended primarily for human presentation.
  * *Link*
    An explicit relationship between (portions of) resources.
  * *Linkbases*
  * *|Linking element|*
    An XLink-conforming XML element that asserts the existence of a link.
  * *|Local resource|*
  * *|Remote resource|*
  * *Resource*
    Any addressable unit of information or service.
  * *|Starting resource|*
    The source from which traversal is begun.
  * *Traversal*
    Using or following a link for any purpose.

## Simple link

~~~{.dtd}
<ELEMENT xlink:type="simple" xlink:href="URI">CONTENT</ELEMENT>
~~~

@author Wouter Beek
@see http://www.w3.org/TR/2010/REC-xlink11-20100506/
@version 2013/05, 2013/07, 2013/09
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(uri(rfc3987_dcg)).
:- use_remote_module(xml(xml_attributes)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').

:- meta_predicate(xlink_attribute(//,+,//,?,?)).
:- meta_predicate(xlink_href(-,//,+,?,?)).
:- meta_predicate(xlink_namespace(//,?,?)).



xlink_attribute(DCG_Namespace, DCG_Name, DCG_Value) -->
  xml_attribute(xlink_namespace(DCG_Namespace), DCG_Name, DCG_Value).

%! xlink_href(-Tree:compound, :DCG_Namespace, +IRI:iri)//

xlink_href(href(iri(IRI)), DCG_Namespace, IRI) -->
  xlink_attribute(
    xlink_namespace(DCG_Namespace),
    xml_name(href),
    'IRI-reference'(IRI)
  ).

xlink_namespace(DCG_Namespace) -->
  {phrase(DCG_Namespace, "xlink")},
  void.
xlink_namespace(DCG_Namespace) -->
  DCG_Namespace.

