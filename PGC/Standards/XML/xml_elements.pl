:- module(
  xml_elements,
  [
    xml_elements//3, % -Trees:list(compound)
                     % :DCG_Namespace
                     % +ElementRules:list(dcg)
    xml_element//3, % :DCG_Namespace
                   % :DCG_Name
                   % ?DCG_Attributes
    xml_header//4 % -Tree:compound
                  % :DCG_Namespace
                  % ?Version:compound
                  % ?Standalone:boolean
  ]
).

/** <module> XML_ENTITIES

DCG rules for XML entities.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(xml(xml_attributes)).
:- use_module(xml(xml_datatypes)).

:- meta_predicate(xml_elements(-,//,//,?,?)).
:- meta_predicate(xml_elements(-,//,+,+,?,?)).
:- meta_predicate(xml_element(//,//,//,?,?)).
:- meta_predicate(xml_element(//,//,//,//,//,?,?)).
:- meta_predicate(xml_element_q(//,//,?,?)).
:- meta_predicate(xml_element_q(//,//,//,?,?)).
:- meta_predicate(xml_header(-,//,?,?,?,?)).



%! xml_elements(
%!   -Trees:list(compound),
%!   :DCG_Namespace,
%!   +ElementRules:list(dcg)
%! )
% Processes the given list of XML element rules.

xml_elements(Trees, DCG_Namespace, Mod:L) -->
  xml_elements(Trees, DCG_Namespace, Mod, L).

xml_elements([], _DCG_Namespace, _Mod, []) --> [].
xml_elements([Tree], DCG_Namespace, Mod, [H]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,DCG_Namespace|Args]).
xml_elements([Tree|Trees], DCG_Namespace, Mod, [H|T]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,DCG_Namespace|Args]),
  xml_elements(Trees, DCG_Namespace, Mod, T).

%! xml_element(:DCG_Namespace, :DCG_Name, +DCG_Attributes:list(dcg))//
% Processes a regular XML entity (i.e., one that does not use
% question marks in its tags).

xml_element(DCG_Namespace, DCG_Name, DCG_Attributes) -->
  xml_element(
    less_than_sign,
    DCG_Namespace,
    DCG_Name,
    DCG_Attributes,
    (forward_slash, greater_than_sign)
  ).

%! xml_element(
%!   :DCG_Open,
%!   :DCG_Namespace,
%!   :DCG_Name,
%!   :DCG_Attributes,
%!   :DCG_Close
%! )//
% Processes generic XML entities, with explicitly set opening and closing
% tags. This is normally called via xml_element//3 or xml_element_q//3.

xml_element(DCG_Open, DCG_Namespace, DCG_Name, DCG_Attributes, DCG_Close) -->
  DCG_Open,
  xml_namespaced_name(DCG_Namespace, DCG_Name),
  space,
  dcg_calls(DCG_Attributes, space),
  DCG_Close.

%! xml_element_q(:DCG_Name, :DCG_Attributes)//
% @see Like xml_element_q//3 but without a namespace.

xml_element_q(DCG_Name, DCG_Attributes) -->
  xml_element_q(void, DCG_Name, DCG_Attributes).

%! xml_element_q(:DCG_Namespace, :DCG_Name, :DCG_Attributes)//
% Processes an XML entity that uses question marks in its tags.

xml_element_q(DCG_Namespace, DCG_Name, DCG_Attributes) -->
  xml_element(
    (less_than_sign, question_mark),
    DCG_Namespace,
    DCG_Name,
    DCG_Attributes,
    (question_mark, greater_than_sign)
  ).

%! xml_header(
%!   -Tree:compound,
%!   :DCG_Namespace,
%!   ?Version:compound,
%!   ?Standalone:boolean
%! )//
% Processes an XML header tag.
%
% @arg Tree A compound term representing the parse tree.
% @arg DCG_Namespace
% @arg Version A compound term of the form
%      `version(?Major:integer,?Minor:integer)`.
% @arg Standalone A boolean (i.e, either `true` or `false`).

xml_header(header(T1,T2), DCG_Namespace, Version, Standalone) -->
  xml_element_q(
    % Note that this cannot be processed by xml_name//1.
    word(xml),
    [
      xml_version(T1, DCG_Namespace, Version),
      xml_standalone(T2, DCG_Namespace, Standalone)
    ]
  ).

