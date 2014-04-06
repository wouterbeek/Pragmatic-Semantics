:- module(
  xml_base,
  [
    xml_base//7 % -Tree:compound
                % :DCG_Namespace
                % ?Scheme:atom
                % ?Authority:compound
                % ?Path:list(list(atom))
                % ?Query:atom
                % ?Fragment:atom
  ]
).

/** <module> XML_BASE

A facility, similar to that of HTML BASE, for defining base URIs for parts
of XML documents.

HTML  BASE allows authors to explicitly specify a document's base URI
for the purpose of resolving relative URIs in links to external images,
applets, form-processing programs, style sheets, and so on.

The attribute =|xml:base|= may be inserted in XML documents to specify
a base URI other than the base URI of the document or external entity.

The value of this attribute is a Legacy Extended IRI (LEIRI)
and may contain characters not allowed in URIs.
(However, some characters allowed in LEIRIs are not legal
XML characters, and cannot therefore appear in xml:base values.)

=|xml:base|= can also be used by non-namespace-aware processors.

### Example

~~~{.xml}
<?xml version="1.0"?>
<doc xml:base="http://example.org/today/"
     xmlns:xlink="http://www.w3.org/1999/xlink">
  <head>
    <title>Virtual Library</title>
  </head>
  <body>
    <paragraph>See <link xlink:type="simple" xlink:href="new.xml">what's
      new</link>!</paragraph>
    <paragraph>Check out the hot picks of the day!</paragraph>
    <olist xml:base="/hotpicks/">
      <item>
        <link xlink:type="simple" xlink:href="pick1.xml">Hot Pick #1</link>
      </item>
      <item>
        <link xlink:type="simple" xlink:href="pick2.xml">Hot Pick #2</link>
      </item>
      <item>
        <link xlink:type="simple" xlink:href="pick3.xml">Hot Pick #3</link>
      </item>
    </olist>
  </body>
</doc>
~~~

The full URIs:
  * "what's new" to =|http://example.org/today/new.xml|=
  * "Hot Pick #1" to =|http://example.org/hotpicks/pick1.xml|=
  * "Hot Pick #2" to =|http://example.org/hotpicks/pick2.xml|=
  * "Hot Pick #3" to =|http://example.org/hotpicks/pick3.xml|=

--

@author Wouter Beek
@see XML Base 1-2 http://www.w3.org/TR/2009/REC-xmlbase-20090128/
@version 2013/08, 2014/03
*/

:- use_module(uri(rfc2396_dcg)).

:- meta_predicate(xml_base(-,//,?,?,?,?,?,?,?)).



%! xml_base(
%!   -Tree:compound,
%!   :DCG_Namespace,
%!   ?Scheme:atom,
%!   ?Authority:compound,
%!   ?Path:list(list(atom)),
%!   ?Query:atom,
%!   ?Fragment:atom
%! )//
% Specifies a base IRI other than the base IRI of the document or external
% entity.
%
% ~~~{.bnf}
% xml:base = "<iri>"
% ~~~
%
% @tbd Implement the XML Base specification.

xml_base(
  xml_base(T1),
  DCG_Namespace,
  Scheme,
  Authority,
  Path,
  Query,
  Fragment
) -->
  xml_attributes:xml_attribute_(
    DCG_Namespace,
    xml_name(base),
    rfc2396_uri_reference(T1, Scheme, Authority, Path, Query, Fragment)
  ).

