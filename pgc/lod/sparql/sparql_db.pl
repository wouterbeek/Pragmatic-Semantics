:- module(
  sparql_db,
  [
    sparql_register_remote/4, % +SparqlRemote:atom
                              % +Server:atom
                              % +Port:or([oneof([default]),nonneg])
                              % +Path:atom
    sparql_current_remote/4, % ?SparqlRemote:atom
                             % ?Server:atom
                             % ?Port:or([oneof([default]),nonneg])
                             % ?Path:atom
    sparql_remove_remote/1, % +SparqlRemote:atom
    sparql_current_remote_domain/2, % ?SparqlRemote:atom
                                    % ?Domain:atom
    sparql_register_remote_domain/2, % +SparqlRemote:atom
                                     % +Domain:atom
    sparql_remote_domain/2 % +SparqlRemote:atom
                           % +Domain:atom
  ]
).

/** <module> SPARQL database

Persistency store for SPARQL-related information.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
*/

:- use_remote_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(lod(lod_location)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(xml(xml_namespace)).

%! sparql_remote(
%!   ?Remote:atom,
%!   ?Server:atom,
%!   ?Port:atomic,
%!   ?Path:atom
%! ) is nondet.

:- dynamic(sparql_remote/4).
:- dynamic(sparql_remote_domain/2).



% SPARQL remote

sparql_register_remote(Remote, Domain, Port, Path):-
  sparql_current_remote(Remote, Domain, Port, Path), !,
  debug(sparql_db, 'SPARQL remote ~w is already set. No changes.', [Remote]).
sparql_register_remote(Remote, _, _, _):-
  sparql_current_remote(Remote, _, _, _), !,
  debug(
    sparql_db,
    'SPARQL remote ~w is already set DIFFERENTLY. First remove.',
    [Remote]
  ).
sparql_register_remote(Remote, Domain, Port, Path):-
  db_add_novel(sparql_remote(Remote, Domain, Port, Path)).


sparql_current_remote(Remote, Domain, Port, Path):-
  sparql_remote(Remote, Domain, Port, Path).


sparql_remove_remote(Remote):-
  once(sparql_remote(Remote, Domain, Port, Path)), !,
  retractall(sparql_remote(Remote, Domain, Port, Path)).
sparql_remove_remote(Remote):-
  existence_error('SPARQL remote', Remote).

sparql_current_remote_domain(SparqlRemote, Domain):-
  sparql_current_remote(SparqlRemote, Domain, _, _).
sparql_current_remote_domain(SparqlRemote, Domain):-
  sparql_remote_domain(SparqlRemote, Domain).

sparql_register_remote_domain(SparqlRemote, Domain):-
  sparql_current_remote(SparqlRemote, _, _, _),
  db_add_novel(sparql_remote_domain(SparqlRemote, Domain)).



% Registrations

% HTML
:- db_add_novel(user:prolog_file_type(htm,  html)).
:- db_add_novel(user:prolog_file_type(html, html)).

% Image
:- db_add_novel(user:prolog_file_type(bmp,  image)).
:- db_add_novel(user:prolog_file_type(gif,  image)).
:- db_add_novel(user:prolog_file_type(jpeg, image)).
:- db_add_novel(user:prolog_file_type(jpg,  image)).
:- db_add_novel(user:prolog_file_type(png,  image)).


% DBpedia
:- sparql_register_remote(dbpedia, 'dbpedia.org', default, '/sparql').
:- lod_register_header(dbpedia, 'Accept', 'application/rdf+xml').
:- lod_register_location(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia ontology
%:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').
:- xml_register_namespace('dbpedia-owl', 'http://dbpedia.org/ontology/').

% DBpedia property
%:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').
:- xml_register_namespace(dbpprop, 'http://dbpedia.org/property/').

% DBpedia resource
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').

% DBpedia localizations
:- sparql_register_remote_domain(dbpedia, 'ace.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'af.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'als.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'am.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'an.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ang.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ar.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'arc.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'arz.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'as.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ast.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'av.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ay.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'az.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ba.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bar.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bat_smg.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bcl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bcl_smg.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'be.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'be-x-old.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'be_x_old.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bg.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bi.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bjn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bm.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bo.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bpy.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'br.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bs.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'bxr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ca.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'cdo.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ce.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ceb.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'chr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'chy.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ckb.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'co.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'commons.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'cr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'crh.dbpedia.org').
:- sparql_register_remote('cs.dbpedia', 'cs.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'cy.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'da.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'diq.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'dv.dbpedia.org').
:- sparql_register_remote('el.dbpedia', 'el.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'eo.dbpedia.org').
:- sparql_register_remote('es.dbpedia', 'es.dbpedia.org', default, '/sparql').
:- sparql_register_remote('eu.dbpedia', 'eu.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'fa.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'fi.dbpedia.org').
:- sparql_register_remote('fr.dbpedia', 'fr.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'frp.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'fy.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ga.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gan.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gd.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'got.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gu.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'gv.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ha.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'hak.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'he.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'hi.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'hif.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ht.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'hu.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'hy.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ia.dbpedia.org').

:- xml_register_namespace('id-dbpprop', 'http://id.dbpedia.org/property/').
:- xml_register_namespace('id-dbpedia', 'http://id.dbpedia.org/resource/').
:- sparql_register_remote('id.dbpedia', 'id.dbpedia.org', default, '/sparql').

:- sparql_register_remote_domain(dbpedia, 'ig.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'io.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'is.dbpedia.org').
:- sparql_register_remote('it.dbpedia', 'it.dbpedia.org', default, '/sparql').
:- sparql_register_remote('ja.dbpedia', 'ja.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'jv.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kaa.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kab.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kbd.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ki.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kk.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'km.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'kn.dbpedia.org').
:- sparql_register_remote('ko.dbpedia', 'ko.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'la.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'lbe.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'lez.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'li.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ln.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'lt.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'lv.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'mhr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'mk.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ml.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'mr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'mrj.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ms.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'my.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'na.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'nah.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ne.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'new.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'nn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'no.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'nrm.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'nv.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'oc.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'pnb.dbpedia.org').
:- sparql_register_remote('pl.dbpedia', 'pl.dbpedia.org', default, '/sparql').
:- sparql_register_remote('pt.dbpedia', 'pt.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'qu.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ro.dbpedia.org').
:- sparql_register_remote('ru.dbpedia', 'ru.dbpedia.org', default, '/sparql').
:- sparql_register_remote_domain(dbpedia, 'rw.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sco.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'se.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'simple.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sq.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'srn.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'su.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sv.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'sw.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'szl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ta.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'te.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'tg.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'th.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'tl.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'tr.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'tt.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'tum.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'udm.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'ug.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'uk.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'vi.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'wa.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'war.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'wo.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'xal.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'yi.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'yo.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'yoh.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'zh.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'zh_min_nan.dbpedia.org').
:- sparql_register_remote_domain(dbpedia, 'zh_yue.dbpedia.org').

% Dublin Core elements
:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- lod_register_location(dc, 'http://dublincore.org/2012/06/14/dcelements.rdf').

% Dublin Core terms
:- xml_register_namespace(dcterms, 'http://purl.org/dc/terms/').
:- lod_register_location(dcterms, 'http://dublincore.org/2012/06/14/dcterms.rdf').

% Dublin core ?
:- xml_register_namespace(eor, 'http://dublincore.org/2000/03/13/eor#').

% Freebase
:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').

% FOAF
:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

% OWL
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_set_predicate(owl:sameAs, symmetric(true)).
:- rdf_set_predicate(owl:sameAs, transitive(true)).

% ?
:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').

% RDF
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% RDFS
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_set_predicate(rdfs:subClassOf, transitive(true)).
:- rdf_set_predicate(rdfs:subPropertyOf, transitive(true)).

% Schema
:- xml_register_namespace(schema, 'http://schema.org/').
:- lod_register_location(schema, 'http://schema.rdfs.org/all.ttl').

% SERQL
:- xml_register_namespace(serql, 'http://www.openrdf.org/schema/serql#').

% SKOS
:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').

% UMBEL
:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').

% XSD
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

% YAGO resource
:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').

