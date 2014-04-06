:- module(
  tms_print,
  [
    tms_print_justification//3 % +Options:list(nvpair)
                               % +TMS:atom
                               % +Justification:iri
  ]
).

/** <module> TMS print

Support for printing (aspects of) a TMS.

@author Wouter Beek
@version 2013/05, 2013/09-2013/10, 2014/01
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(generics(option_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(tms(tms)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').



%! tms_print_justification(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Justification:iri
%! ) is det.
% The following options are supported:
%   * =|indent(+Indent:nonneg)|=
%   * =|lang(+LangTag:atom)|=

:- rdf_meta(tms_print_justification(+,+,r,?,?)).
tms_print_justification(O1, TMS, J) -->
  {once(tms_justification(TMS, As, R, C, J))},
  
  % Write the reason.
  {add_default_option(O1, indent, 0, I, O2)},
  indent(I),
  bracketed(square, atom(R)),
  space,
  
  % Write the consequent.
  tms_print_node(O2, TMS, C),
  nl,
  
  % Write the antecendents.
  {update_option(O2, indent, succ, _, O3)},
  tms_print_nodes(O3, TMS, As).


tms_print_nodes(_, _, []) --> !, [].
tms_print_nodes(O1, TMS, [H|T]) -->
  tms_print_node(O1, TMS, H),
  tms_print_nodes(O1, TMS, T).
  

%! tms_print_node(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Conclusion:iri
%! )// is det.
% The following options are supported:
%   * =|indent(+Indent:nonneg)|=
%   * =|lang(+LangTag:atom)|=

tms_print_node(O1, TMS, C) -->
  {add_default_option(O1, indent, 0, O2)},
  
  ({
    tms_node(TMS, C),
    rdf_has(J, tms:has_consequent, C)
  } ->
    tms_print_justification(O2, TMS, J)
  ;
    {option(indent(I), O2, 0)},
    indent(I),
    bracketed(square, `rdf`),
    space,
    tms_print_node_dead_end(O2, C),
    nl
  ).


%! tms_print_node_dead_end(+Options:list(nvpair), +Node:iri)// is det.
% The following options are supported:
%   * =|lang(+LangTag:atom)|=

tms_print_node_dead_end(O1, N) -->
  {
    option(lang(Lang), O1, en),
    rdfs_preferred_label([Lang], N, L, _, _)
  },
  atom(L).

