:- module(
  rdf_explain,
  [
    rdf_explain//4 % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Object:or([bnode,iri,literal])
                   % ?RdfGraph:atom
  ]
).

/** <module> RDF Web

Web predicates for RDF graphs.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/09, 2013/11-2014/01,
         2014/03-2014/04
*/

:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(generics(typecheck)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(gv(gv_file)).
:- use_remote_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_meta_auto_expand)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf(rdf_namespace)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(rdf_term(rdf_term)).
:- use_remote_module(rdf_reasoning(rdf_bnode_map)).
:- use_remote_module(rdf_reasoning(rdf_mat)).
:- use_remote_module(rdf_web(rdf_term_html)).
:- use_remote_module(server(app_ui)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(tms(tms)).
:- use_remote_module(tms(tms_export)).
:- use_remote_module(xml(xml_dom)).
:- use_remote_module(xml(xml_namespace)).

% This allows a user to type `rdf:type` in the Web console and
% have it translated to a full URI.
%%%%:- rdf_meta_expand(rdf_web:rdf_explain_web(e,e,e,i)).

:- rdf_meta(rdf_explain(r,r,o,+,?,?)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(explain), rdf_explain, []).

user:web_module('RDF explain', rdf_explain).



rdf_explain(_Request):-
  reply_html_page(app_style, title('RDF explain'), rdf_explain).


%! rdf_explain(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?RdfGraph:atom
%! ) is det.

rdf_explain(S, P, O, G) -->
  {
    aggregate_all(
      set([S,P,O,G]),
      rdf(S, P, O, G),
      Quadruples
    )
  },
  rdf_explain_web(Quadruples).


%! rdf_explain_web(+Quadruples:list(ground))// is det.

rdf_explain_web([]) --> !, [].
rdf_explain_web([[S,P,O,G]|T]) -->
  {
    dcg_with_output_to(atom(TripleName), rdf_triple_name(S, P, O, G)),
    tms_create_node_iri(TripleName, N),
    tms_export_node([], N, Gif),
    graph_to_svg_dom([], Gif, SvgDom)
  },
  html([
    h1(\rdf_triple_html(rdf(explain), S, P, O, G)),
    \xml_dom_as_atom(SvgDom),
    \rdf_explain_web(T)
  ]).

