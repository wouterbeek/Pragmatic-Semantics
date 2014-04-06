:- module(
  qsim_mod,
  [
    export_mod/2 % +Graph:atom
                 % -GIF:compound
  ]
).

/** <module> QSIM model

Exporting of QSIM models to the GIF intermediary format.

@author Wouter Beek
@version 2013/09
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_os)).
:- use_module(dcg(dcg_wrap)).
:- use_module(generics(list_ext)).
:- use_module(generics(pair_ext)).
:- use_module(gv(gv_dcg)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(owl(owl_read)).
:- use_module(qsim(qsim_read)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(rdfs(rdfs_read)).
:- use_module(server(user_db)).
:- use_module(webqr(webqr_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').



export_mod(G, graph(V_Terms,E_Terms,G_Attrs)):-
  % Vertices and edges for the QSIM resources.
  aggregate_all(
    set(X-Y),
    (
      rdf(X, qsim:d_rel,    Y, G)
    ;
      rdf(X, qsim:m_neg,    Y, G)
    ;
      rdf(X, qsim:m_pos,    Y, G)
    ;
      rdf(X, qsim:quantity, Y, G)
    ;
      sum_relation(G, Q1, Q2, Q3),
      X = [Q1,Q2,Q3],
      member(Y, [Q1,Q2,Q3])
    ),
    Es
  ),
  pairs_to_members(Es, Vs),

  maplist(mod_edge_term(G, Vs), Es, E_Terms),
  maplist(mod_vertex_term(G, Vs), Vs, V_Terms),

  G_Attrs = [dir(forward),label(G)].

mod_edge_term(_G, Vs, [Q1,Q2,Q3]-Q, edge(FromId,ToId,[label(L)])):-
  mod_vertex_id(Vs, [Q1,Q2,Q3], SumId),
  (
    Q == Q1
  ->
    mod_vertex_id(Vs, Q1, FromId),
    L = arg1,
    ToId = SumId
  ;
    Q == Q2
  ->
    mod_vertex_id(Vs, Q2, FromId),
    L = arg2,
    ToId = SumId
  ;
    Q == Q3
  ->
    FromId = SumId,
    L = '=',
    mod_vertex_id(Vs, Q3, ToId)
  ).

mod_edge_term(G, Vs, FromV-ToV, edge(FromV_Id,ToV_Id,[label(L)])):-
  nth0chk(FromV_Id, Vs, FromV),
  nth0chk(ToV_Id, Vs, ToV),
  once((
    rdf(FromV, P, ToV, G),
    rdfs_label(P, L)
  )).

% Sums
mod_vertex_color(_G, [_,_,_], orange):- !.
% Entities.
mod_vertex_color(G, V, green):-
  rdfs_individual(m(t,f,f), V, qsim:'Entity', G), !.
% Quantities.
mod_vertex_color(G, V, purple):-
  rdfs_individual(m(t,f,f), V, qsim:'Quantity', G), !.

%! mod_vertex_label(
%!   +Graph:atom,
%!   +Vertex:or([bnode,iri,literal]),
%!   -Label:atom
%! ) is det.
% Returns a GraphViz HTML-like label for the given resource.
%
% @tbd Alignment does not seem to work for the table element.

mod_vertex_label(_G, [_,_,_], sum):- !.
mod_vertex_label(G, V, L):-
  mod_vertex_label_first_row(G, V, Row0),
  % Vertices and edges for the related DBPedia resources.
  aggregate_all(
    set(tag(tr,[],[tag(td,[],PName),tag(td,[],OName)])),
    (
      owl_resource_identity(V, W, G),
      rdf_filtered_triple(W, P, O, G),
      resource_to_list(P, PName),
      resource_to_list(O, OName)
    ),
    Rows
  ),
  Rows \== [], !,
  dcg_with_output_to(
    atom(L),
    gv_html_like_label([tag(table,[align=left],[Row0|Rows])])
  ).
mod_vertex_label(_G, V, L):-
  once(rdfs_label(V, L)).

%! mod_vertex_label_first_row(
%!   +Graph:atom,
%!   +Vertex:iri,
%!   -FirstRow:compound
%! ) is det.
% Returns the first row in a GraphViz HTML-like label.
% This row contains the type of vertex and the prefered RDFS label.

mod_vertex_label_first_row(
  G,
  V,
  tag(tr,[],[tag(td,[],[V_Type2]),tag(td,[],[V_Name2])])
):-
  mod_vertex_type(G, V, V_Type1),
  atom_codes(V_Type1, V_Type2),
  user_property(G, natural_languages, PrefLangTags),
  rdfs_preferred_label(V, PrefLangTags, _PrefLangTag, V_Name1),
  atom_codes(V_Name1, V_Name2).

mod_vertex_shape(_G, [_,_,_], diamond):- !.
mod_vertex_shape(_G, _V, none).

mod_vertex_id(Vs, [Q1,Q2,Q3], SumId):- !,
  nth0chk(Q1_Id, Vs, Q1),
  nth0chk(Q2_Id, Vs, Q2),
  nth0chk(Q3_Id, Vs, Q3),
  format(atom(SumId), '~w_~w_~w', [Q1_Id,Q2_Id,Q3_Id]).
mod_vertex_id(Vs, V, V_Id):-
  nth0chk(V_Id, Vs, V).

mod_vertex_term(G, Vs, V, vertex(V_Id,V,[color(C),label(L),shape(S)])):-
  mod_vertex_id(Vs, V, V_Id),
  mod_vertex_color(G, V, C),
  mod_vertex_label(G, V, L),
  mod_vertex_shape(G, V, S).

mod_vertex_type(G, V, 'Entity'):-
  rdfs_individual(m(t,f,f), V, qsim:'Entity', G).
mod_vertex_type(G, V, 'Quantity'):-
  rdfs_individual(m(t,f,f), V, qsim:'Quantity', G).

rdf_filtered_triple(S, P, O2, G):-
  rdf(S, P, O1, G),
  (
    % We only want a single RDF label, based on out language preference.
    rdf_global_id(rdfs:label, P)
  ->
    user_property(G, natural_languages, PrefLangTags),
    rdfs_preferred_label(PrefLangTags, S, O2, _)
  ;
    % For specific predicates with a literal object,
    % we only want a single row, based on the language preference.
    rdf_memberchk(P, [dbo:abstract,rdfs:comment,skos:prefLabel])
  ->
    user_property(G, natural_languages, PrefLangTags),
    rdf_preferred_language_tagged_string(PrefLangTags, S, P, O2, _)
  ;
    % Triples with some properties are left out.
    rdf_memberchk(
      P,
      [dbo:wikiPageID,dbo:wikiPageRevisionID,dbp:auto,dbp:date,owl:sameAs]
    )
  ->
    fail
  ;
    O2 = O1
  ).

resource_to_list(R, L4):-
  phrase(rdf_term_name(R), L1),
  phrase(
    dcg_wrap(
      [padding(false),separator(newline),wrap_margin(80),wrap_mode(word)]
    ),
    L1,
    L2
  ),
  phrase(dcg_separated_list(newline, L3), L2),
  append_intersperse(L3, tag(br,[]), L4).

