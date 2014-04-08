:- module(
  webqr_build,
  [
    webqr_create_model/1, % +User:atom
    webqr_create_circle/3, % +User:atom
                           % +GlobalURI1:atom
                           % -GlobalURI2:iri
    webqr_create_circle/4, % +User:atom
                           % +LocalURI:iri
                           % +GlobalURI1:atom
                           % -GlobalURI2:iri
    webqr_create_line/4, % +User:atom
                         % +FromLocalURI:iri
                         % +Predicate:iri
                         % +ToLocalURI:iri
    webqr_possible_relations/4, % +Options:list(nvpair)
                                % +FromLocalURI:iri
                                % +ToLocalURI:iri
                                % -Predicates:list(iri)
    webqr_remove_resource/2, % +User:atom
                             % +LocalURI:iri
    webqr_update_circle/4 % +User:atom
                          % +LocalURI:iri
                          % +GlobalURI1:atom
                          % -GlobalURI2:iri
  ]
).

/** <module> WebQR Build

@author Wouter Beek
@version 2013/07, 2013/09, 2013/11-2014/02
*/

:- use_remote_module(dbpedia(dbpedia_eq)).
:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(lod(cache_it)).
:- use_remote_module(lod(lod_query)).
:- use_remote_module(owl(owl_build)).
:- use_remote_module(owl(owl_read)).
:- use_remote_module(qsim(qsim_build)).
:- use_remote_module(qsim(qsim_vocabulary)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(rdfs(rdfs_build)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(rdfs(rdfs_read)).
:- use_remote_module(server(user_db)).
:- use_remote_module(sparql(sparql_find)).
:- use_remote_module(webqr(webqr_generic)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(webqr, 'http://www.wouterbeek.com/webqr#').



% CREATE MODEL %

webqr_create_model(User):-
  user(User), !.
webqr_create_model(User):-
  % Set the preferred natural languages.
  % @tbd Use the `Accept-Languages` HTTP header.
  NaturalLanguages = [nl,en],
  add_user(User, [roles([modeler])]),

  % Set the modeling language.
  ModelingLanguage = qsim,
  add_user_property(User, modeling_language, ModelingLanguage),
  webqr_global_graph(User, GlobalGraph),
  webqr_assert_vocabulary(ModelingLanguage, GlobalGraph),
  add_user_property(User, natural_languages, NaturalLanguages).

webqr_assert_vocabulary(qsim, GlobalGraph):-
  rdf_assert_property(webqr:relation, GlobalGraph),
  qsim_assert_vocabulary(GlobalGraph).



% CREATE CIRCLE %

%! webqr_create_circle(+User:atom, +Suggestion:atom, -Resource:iri) is det.
% Debug variant of webqr_create_circle/4,
%  simulating the local/global IRI distinction
%  from the Web UI.

webqr_create_circle(User, GlobalURI1, GlobalURI2):-
  flag(local_uris, Id, Id + 1),
  rdf_global_id(qsim:Id, LocalURI),
  webqr_create_circle(User, LocalURI, GlobalURI1, GlobalURI2).

webqr_create_circle(User, LocalURI, GlobalURI1, GlobalURI2):-
  webqr_local_graph(User, LocalGraph),

  % Make sure the internal identifier is kept locally.
  rdfs_assert_individual(LocalURI, LocalGraph),

  webqr_update_circle(User, LocalURI, GlobalURI1, GlobalURI2).



% CREATE LINE %

webqr_create_line(User, FromLocalURI, Predicate, ToLocalURI):-
  % Find the IRIs that denote the same resources in the global graph.
  % These identity statements are stored in the local graph.
  webqr_local_graph(User, LocalGraph),
  owl_resource_identity(FromLocalURI, FromGlobalURI, LocalGraph),
  owl_resource_identity(ToLocalURI, ToGlobalURI, LocalGraph),

  % Now that we have found the global URIs,
  %  we can assert the new relation in the global graph.
  webqr_global_graph(User, GlobalGraph),
  rdf_assert(FromGlobalURI, Predicate, ToGlobalURI, GlobalGraph).

%! webqr_possible_relations(
%!   +Options:list(nvpair),
%!   +FromLocalURI:uri,
%!   +ToLocalURI:uri,
%!   -Predicates:ordset(iri)
%! ) is det.
% Returns the QSIM modeling relations that can be added between
% the given resource (in that order).
%
% The following options are supported:
%   * =|graph(+Graph:atom)|=
%     The atomic name of an RDF graph.
%     The default is =user=.
%   * =|modeling_language(+ModelingLanguage:oneof([qsim]))|=
%     The modeling language for which relations are retrieved.
%     The default is =qsim=.

webqr_possible_relations(_, FromLocalURI, ToLocalURI, Ps):-
  rdf(FromLocalURI, owl:sameAs, FromGlobalURI),
  dbpedia_eq(FromGlobalURI, FromEQ),
  rdf(ToLocalURI, owl:sameAs, ToGlobalURI),
  dbpedia_eq(ToGlobalURI, ToEQ),

  % The top relation for the given modeling language.
  rdf_global_id(webqr:relation, TopP),

  aggregate_all(
    set(P),
    (
      % We only consider properties that occur at the lowest level
      %  in the property hierarchy.
      rdfs_subproperty_of(P, TopP),
      \+ ((
        rdfs_subproperty_of(BottomP, P),
        \+ rdf_equal(BottomP, P)
      )),
      (check_p(P, FromEQ, ToEQ), ! ; check_p(P, ToEQ, FromEQ))
    ),
    Ps
  ).
check_p(P, FromEQ, ToEQ):-
  % Then we look at *all* the domain restrictions.
  forall(
    rdfs_domain(m(t,f,f), P, Domain, _),
    rdf_equiv(FromEQ, Domain)
  ),
  % Then we look at all range restrictions.
  forall(
    rdfs_range(m(t,f,f), P, Range, _),
    rdf_equiv(ToEQ, Range)
  ).



% REMOVE CIRCLE AND LINE %

%! webqr_remove_resource(+User:atom, +LocalURI:iri) is det.
% @tbd Remove resources under identity closure.

webqr_remove_resource(User, LocalURI):-
  rdf_remove_resource(User, LocalURI).



% UPDATE CIRCLE %

webqr_update_circle(User, LocalURI, GlobalURI1, GlobalURI2):-
  webqr_global_graph(User, GlobalGraph),
  webqr_local_graph(User, LocalGraph),

  % From atom to IRI with content asserted in DBpedia.
  sparql_find(dbpedia, GlobalURI1, GlobalURI2),

  % Make sure the resource is mentioned in the global graph.
  % This means that it is included in the user's QR model.
  rdfs_assert_individual(GlobalURI2, GlobalGraph),

  % Store the direct facts about the concept in the user graph.
  % This is a cache of sorts, since the direct facts will often
  %  provide us with material to base feedback on.
  cache_it1(_, lod_cache, _, GlobalURI2),

  % Decouple any previous concepts.
  owl_retractall_resource_identity(LocalURI, _OldGlobalURI, LocalGraph),

  % Connect the local with the global URI.
  % This also connects the local with the global RDF graph.
  owl_assert_resource_identity(LocalURI, GlobalURI2, LocalGraph),

  % Make sure the resource has an RDFS label.
  (
    rdfs_label(GlobalURI2, _Label), !
  ;
    rdfs_assert_label(GlobalURI2, GlobalURI1, GlobalGraph)
  ).

