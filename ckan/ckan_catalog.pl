:- module(
  ckan_catalog,
  [
    ckan_download_catalog/2, % +Site:atom
                             % -File:atom
    ckan_load_catalog/2, % +Site:atom
                         % -RdfGraph:atom
    ckan_lod_resources/2 % + Site:atom
                         % -Resources:ordset(iri)
  ]
).

/** <module> CKAN catalog

Support for the CKAN catalog in RDF.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(rdf_term(rdf_string)).

:- use_remote_module(prasem, ckan(ckan_db)).
:- use_remote_module(prasem, ckan(ckan_to_rdf)).



ckan_download_catalog(Site, File):-
  ckan_file(Site, catalog, ttl, File),
  access_file(File, read), !.
ckan_download_catalog(Site, File):-
  setup_call_cleanup(
    ckan_graph(Site, G),
    (
      ckan_to_rdf(Site, G),
      ckan_file(Site, catalog, ttl, File),
      rdf_save([], G, File)
    ),
    rdf_unload_graph(G)
  ).


ckan_load_catalog(Site, Graph):-
  ckan_download_catalog(Site, File),
  rdf_load([], Graph, File).


ckan_lod_resources(G, Resources):-
  aggregate_all(
    set(Resource),
    (
      rdfs_individual_of(Resource, ckan:'Resource'),
      is_ckan_lod_resource(G, Resource)
    ),
    Resources
  ).


ckan_rdf_format('RDF').
ckan_rdf_format('XML').
ckan_rdf_format('application/n-triples').
ckan_rdf_format('application/rdf+xml').
ckan_rdf_format('application/x-nquads').
ckan_rdf_format('application/x-ntriples').
ckan_rdf_format('example/n3').
ckan_rdf_format('example/ntriples').
ckan_rdf_format('example/rdf xml').
ckan_rdf_format('example/rdf+json').
ckan_rdf_format('example/rdf+json').
ckan_rdf_format('example/rdf+ttl').
ckan_rdf_format('example/rdf+xml').
ckan_rdf_format('example/rdfa').
ckan_rdf_format('example/turtle').
ckan_rdf_format('example/x-turtle').
ckan_rdf_format('html+rdfa').
ckan_rdf_format('linked data').
ckan_rdf_format('mapping/owl').
ckan_rdf_format('meta/owl').
ckan_rdf_format('meta/rdf-schema').
ckan_rdf_format('meta/void').
ckan_rdf_format(owl).
ckan_rdf_format('rdf-n3').
ckan_rdf_format('rdf-turtle').
ckan_rdf_format('rdf-xml').
ckan_rdf_format('rdf/n3').
ckan_rdf_format('rdf/turtle').
ckan_rdf_format('rdf/xml, html, json').
ckan_rdf_format('text/n3').
ckan_rdf_format('text/turtle').


ckan_rdf_mimetype('application/rdf+n3').
ckan_rdf_mimetype('application/rdf+xml').
ckan_rdf_mimetype('application/turtle').
ckan_rdf_mimetype('text/n3').
ckan_rdf_mimetype('text/json').
ckan_rdf_mimetype('text/rdf+n3').
ckan_rdf_mimetype('text/turtle').
%ckan_rdf_mimetype('text/xml').


is_ckan_lod_resource(G, Resource):-
  rdf_string(Resource, ckan:format, Format, G),
  ckan_rdf_format(Format), !.
is_ckan_lod_resource(G, Resource):-
  rdf_string(Resource, ckan:mimetype, Mimetype, G),
  ckan_rdf_mimetype(Mimetype).

