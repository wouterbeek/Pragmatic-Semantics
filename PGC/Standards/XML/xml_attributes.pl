:- module(
  xml_attributes,
  [
    xml_attribute//3, % :DCG_Namespace
                      % :DCG_Name
                      % :DCG_Value
    xml_attribute//4, % :DCG_Namespace
                      % :DCG_Name
                      % :DCG_Value
                      % :DCG_Sepatator
    xml_id//3, % -Tree:compound
               % :DCG_Namespace
               % ?Name:atom
    xml_inject_attributes/4, % :DCG_Namespace,
                             % +Attributes1:list(dcg),
                             % -Attributes2:list(dcg),
                             % -Trees:list(compound)
    xml_language//3, % -Tree:compound
                     % :DCG_Namespace
                     % ?LanguageTag
    xml_standalone//3, % -Tree:compound
                       % :DCG_Namespace
                       % ?Standalone:boolean
    xml_version//3 % -Tree:compound
                   % :DCG_Namespace
                   % ?Version:compound
  ]
).

/** <module> XML_ATTRIBUTES

DCG rules for XML attributes.

@author Wouter Beek
@see XML 1.0-5 http://www.w3.org/TR/2008/REC-xml-20081126/
@version 2013/07, 2014/03
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(lang(rfc3066)).
:- use_remote_module(uri(rfc2396_dcg)).
:- use_remote_module(xml(xml_datatypes)).
:- use_remote_module(xml(xml)).

:- meta_predicate(xml_attribute(//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,//,?,?)).
:- meta_predicate(xml_attribute_(//,//,//,?,?)).
:- meta_predicate(xml_attribute_list(-,//,//,?,?,?)).
:- meta_predicate(xml_id(-,//,?,?,?)).
:- meta_predicate(xml_inject_attributes(//,+,-,-)).
:- meta_predicate(xml_language(-,//,?,?,?)).
:- meta_predicate(xml_standalone(-,//,?,?,?)).
:- meta_predicate(xml_value(//,?,?)).
:- meta_predicate(xml_version(-,//,?,?,?)).



%! xml_attribute(:DCG_Name, :DCG_Value)//
% Processes an XML attribute based on DCG rules for the head and the value.
% XML attributes have an equals signs in between the name and value parts.

xml_attribute(DCG_Name, DCG_Value) -->
  DCG_Name,
  equals_sign,
  xml_value(DCG_Value).

xml_attribute(DCG_Namespace, DCG_Name, DCG_Value) -->
  xml_attribute(xml_namespaced_name(DCG_Namespace, DCG_Name), DCG_Value).

xml_attribute(DCG_Namespace, DCG_Name, DCG_Value1, DCG_Separator) -->
  {DCG_Value1 =.. [P, Trees, A1s], DCG_Value2 =.. [P]},
  xml_attribute(
    xml_namespaced_name(DCG_Namespace, DCG_Name),
    xml_attribute_list(Trees, DCG_Value2, A1s, DCG_Separator)
  ).

xml_attribute_(DCG_Namespace, DCG_Name, DCG_Value) -->
  xml_attribute(xml_namespace(DCG_Namespace), DCG_Name, DCG_Value).

xml_attribute_list([], _DCG_Value, [], _DCG_Separator) --> [].
xml_attribute_list([Tree], DCG_Value1, [A1], _DCG_Separator) -->
  {DCG_Value1 =.. [P], DCG_Value2 =.. [P, Tree, A1]},
  phrase(DCG_Value2).
xml_attribute_list([Tree|Trees], DCG_Value1, [A1|A1s], DCG_Separator) -->
  {DCG_Value1 =.. [P], DCG_Value2 =.. [P, Tree, A1]},
  phrase(DCG_Value2),
  phrase(DCG_Separator),
  xml_attribute_list(Trees, DCG_Value1, A1s, DCG_Separator).

%! xml_id(-Tree:compound, :DCG_Namespace, ?Name:atom)//
% Standard XML attribute for assigning a unique name to an element.
%
% =ID= is a =TokenizedType= of an =AttType=.
% Values of type =ID= MUST match the =Name= production. A name MUST NOT
% appear more than once in an XML document as a value of this type;
% i.e., =ID= values MUST uniquely identify the elements which bear them.
%
% An element type MUST NOT have more than one =ID= attribute specified.
%
% An =ID= attribute MUST have a declared default of
% =|#IMPLIED|= or =|#REQUIRED|=.
%
% ~~~{.bnf}
% id = "name"
% ~~~

xml_id(id(Name), DCG_Namespace, Name) -->
  xml_attribute_(DCG_Namespace, xml_name(id), xml_name(Name)).

%! xml_inject_attributes(
%!   :DCG_Namespace,
%!   +Attributes1:list(dcg),
%!   -Attributes2:list(dcg),
%!   -Trees:list(compound)
%! ) is det.
% Prepares the given XML attributes for being passed to xml_attribute//2.

xml_inject_attributes(_DCG_Namespace, [], [], []).
xml_inject_attributes(DCG_Namespace, [H1|T1], [H2|T2], [Tree|Trees]):-
  H1 =.. [P | Args],
  H2 =.. [P, Tree, DCG_Namespace | Args],
  xml_inject_attributes(DCG_Namespace, T1, T2, Trees).

%! xml_language(-Tree:compound, :DCG_Namespace, ?LanguageTag:atom)//
% Specifies the language used in the contents and attribute values of any
% element in an XML document. In valid documents, this attribute, like any
% other, MUST be declared if it is used.
%
% ~~~{.bnf}
% xml:base = "<iri>"
% ~~~
%
% The values of the attribute are language identifiers as defined by
% IETF BCP 47, "Tags for the Identification of Languages"; in addition,
% the empty string may be specified.
%
% Examples of declarations:
% ~~~{.dtd}
% <!ATTLIST example xml:lang CDATA #IMPLIED>
% <!ATTLIST poem    xml:lang CDATA 'fr'    >
% <!ATTLIST gloss   xml:lang CDATA 'en'    >
% <!ATTLIST note    xml:lang CDATA 'en'    >
% ~~~
%
% @see The XML 1.0-5 specification references IETF RFC 4646 and RFC 4647
%      for the valid values of this attribute.
%      The XML 1.1-2 specification references IETF RFC 3066, a precursor of
%      RFC 4646 and RFC 4647, instead.
%      This module implements RFC 5646, the successor to 4646.
% @tbd Implement RFC 4647.

xml_language(xml_language(T1), DCG_Namespace, LanguageTag) -->
  xml_attribute_(
    DCG_Namespace,
    xml_name(lang),
    rfc3066_language_tag(T1, Primary, Secondary)
  ),
  {atomic_list_concat([Primary,Secondary], '-', LanguageTag)}.

%! xml_standalone(-Tree:compound, :DCG_Namespace, ?Standalone:boolean)//
% Processes the XML standalone attribute.

xml_standalone(standalone(T1), DCG_Namespace, Standalone) -->
  xml_attribute_(
    DCG_Namespace,
    xml_name(standalone),
    xml_yes_no(T1, Standalone)
  ).

%! xml_value(:DCG_Body)//
% Processes an XML value based on a DCG rule.
% XML values are enclosed in double quotes.

xml_value(DCG_Body) -->
  double_quote,
  DCG_Body,
  double_quote.

%! xml_version(-Tree:compound, :DCG_Namespace, ?Version:compound)//
% Processes the XML version attribute.
%
% @arg Tree A compound term representing a parse tree.
% @arg DCG_Namespace A DCG_Rule.
% @arg Version A compound term of the form
%        =|version(Major:positive_integer,Minor:positive_integer)|=.

xml_version(
  version(major(Major),minor(Minor)),
  DCG_Namespace,
  version(Major,Minor)
) -->
  xml_attribute_(
    DCG_Namespace,
    xml_name(version),
    (decimal_number(Major), dot, decimal_number(Minor))
  ).

