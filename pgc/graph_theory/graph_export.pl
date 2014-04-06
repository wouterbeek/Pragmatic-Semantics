:- module(
  graph_export,
  [
    export_graph/3, % +Options:list(nvpair)
                    % +Graph:oneof([dgraph,rdf_graph,ugraph])
                    % -GraphTerm:compound
    export_graph/4, % +Options:list(nvpair)
                    % :CoordFunc
                    % +Graph:oneof([dgraph,rdf_graph,ugraph])
                    % -GraphTerm:compound
    export_vertex/4, % +Options:list(nvpair)
                     % :N_P
                     % +Vertex
                     % -GraphTerm:compound
    shared_attributes/3 % +Terms:list(compound)
                        % -SharedAttributes:list(nvpair)
                        % -NewTerms:list(compound)
  ]
).

/** <module> GRAPH_EXPORT

Generic graph export module.

# Graph Interchange Format

Better known as the GIF format :-).

~~~{.pl}
graph(VertexTerms, EdgeTerms, GraphAttributes)
~~~

~~~{.pl}
vertex(VertexId, Vertex, VertexAttributes)
~~~

~~~{.pl}
edge(FromVertexId, ToVertexId, EdgeAttributes)
~~~

@author Wouter Beek
@tbd Too many uses of `=..` in this module. Consider using [option_ext].
@version 2012/12-2013/04, 2013/07
*/

:- use_remote_module(generics(option_ext)).
:- use_remote_module(graph_theory(graph_generic)).
:- use_remote_module(graph_theory(random_vertex_coordinates)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_export)).
:- use_remote_module(ugraph(ugraph_ext)).
:- use_remote_module(ugraph(ugraph_export)).

:- meta_predicate(export_graph(+,4,+,-)).
:- meta_predicate(export_vertex(+,3,+,-)).

:- rdf_meta(export_vertex(+,:,r,-)).



%! export_graph(+Options:list(nvpair), +Graph, -GraphTerm:compound) is det.
% @see A simplified version of export_graph/4.

export_graph(O, G, G_Term):-
  export_graph(O, random_vertex_coordinate, G, G_Term).

%! export_graph(
%!   +Options:list(nvpair),
%!   :CoordinateFunction,
%!   +Graph,
%!   -GraphTerm:compound
%! ) is det.
% The following options are used for exporting graphs:
%   1. `colorscheme(+Colorscheme:atom)`
%      The colorscheme for the colors assigned to vertices and edges.
%      Supported values are `svg`, `x11` (default), and the
%      Brewer colorschemes (see module [brewer.pl].
%   2. `directed(+Directedness:boolean)`
%      Whether or not the directionality of the edges is taken into account.
%   3. `edge_labels(oneof([all,none,replace])`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).
%   4. `language(+Language:atom)`
%      The preferred language that is used for natural language content.
%      The default value is `en` (English).
%   5. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%   6. `rdf_list(+Included:boolean)`
%      Whether vertices that are part of an RDF list should be
%      included (`true`, default) or not (`false`).
%   7. `uri_desc(+URI_Description:oneof([uri_only,with_literals,with_preferred_label])`
%      The way in which URI vertices are descibed.
%
% @tbd Enforce that language codes belong to the official RFC standard.

%export_graph(O, CoordFunc, G, G_Term):-
%  is_dgraph(G),
%  export_dgraph(O, CoordFunc, G, G_Term).
export_graph(O, CoordFunc, G, G_Term):-
  atomic(G), rdf_graph(G), !,
  export_rdf_graph(O, CoordFunc, G, G_Term).
export_graph(O, CoordFunc, G, G_Term):-
  is_ugraph(G), !,
  export_ugraph(O, CoordFunc, G, G_Term).

%! export_vertex(
%!   +Options:list(nvpair),
%!   :N_P,
%!   +Vertex,
%!   -GraphTerm:compound
%! ) is det.

export_vertex(O1, N_P, V, G_Term):-
  select_option(depth(Depth), O1, O2, 1),
  merge_options([directed(false)], O2, O3),
  depth(O3, N_P, V, Depth, Vs, Es),
  export_graph(O1, ugraph(Vs, Es), G_Term).

remove_attribute(Attrs, T, NewT):-
  T =.. L,
  append(L1, [T_Attrs], L),
  subtract(T_Attrs, Attrs, NewT_Attrs),
  append(L1, [NewT_Attrs], NewL),
  NewT =.. NewL.

shared_attribute([T1|Ts], N=V):-
  T1 =.. L1,
  last(L1, Attrs1),
  member(Attr1, Attrs1),
  Attr1 =.. [N,V],
  % The colorscheme cannot be part of this, apparently.
  N \== colorscheme,
  forall(
    member(T2, Ts),
    (
      T2 =.. L2,
      last(L2, Attrs2),
      member(Attr2, Attrs2),
      Attr2 =.. [N,V]
    )
  ).

shared_attributes(Terms, SharedAttrs, NewTerms):-
  findall(
    SharedAttr,
    shared_attribute(Terms, SharedAttr),
    SharedAttrs
  ),
  maplist(remove_attribute(SharedAttrs), Terms, NewTerms).

