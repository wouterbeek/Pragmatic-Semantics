:- module(
  ckan_ap,
  [
    ckan_ap/0,
    ckan_ap/1, % +ExtraStages:list(compound)
    ckan_ap/2 % +File:atom
              % +ExtraStages:list(compound)
  ]
).

/** <module> CKAN AP

Automated processes for CKAN data.

@author Wouter Beek
@version 2014/01-2014/03
*/                                        

:- use_module(ap(ap)).
:- use_module(ap(ap_archive_ext)). % Used in AP stage.
:- use_module(ap(ap_download)).
:- use_module(ap(ap_db)).
:- use_module(ap(ap_file_mime)). % Used in AP stage.
:- use_module(ap(ap_file_size)). % Used in AP stage.
:- use_module(ap(ap_rdf_serial)). % Used in AP stage.
:- use_module(ap(ap_void_fetch)). % Used in AP stage.
:- use_module(ap(ap_void_stat)). % Used in AP stage.
:- use_module(ckan(ckan_scrape)).
:- use_module(generics(uri_ext)). % Used in AP stage.
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % MD5
:- use_module(library(semweb/rdfs)).
:- use_module(library(threads)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_name)). % Used in meta-DCG.
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdfs(rdfs_label_ext)).



ckan_ap_site(AP_Collection, ExtraStages, Resource):-
  once(rdf_string(Resource, ckan:url, Url, _)),

  % The directory name is based on the URL.
  rdf_atom_md5(Url, 1, Hash),
  create_nested_directory(data(Hash), Dir),
  assert(user:file_search_path(Hash, Dir)),

  create_ap(ApCollection, AP), %AP
  rdf_assert(AP, ap:resource, Resource, ap), %AP
  rdf_assert_string(AP, ap:alias, Hash, ap), %AP
  rdf_assert_string(AP, ap:graph, Graph, ap), %AP
  
  
  
  ap(
    [leave_trail(false),reset(true)],
    AP,
    [
      ckan_ap:ap_stage([name('Download')], ckan_download_to_directory),
      ckan_ap:ap_stage([name('Arch')], extract_archives),
      %ckan_ap:ap_stage([name('FetchVoID')], void_fetch),
      ckan_ap:ap_stage(
        [name('toNTriples'),args(['application/n-triples'])],
        ap_rdf_merge_directory
      ),
      ckan_ap:ap_stage([name('FileSize')], file_size),
      ckan_ap:ap_stage([name('VoID')], void_statistics)
    | ExtraStages]
  ).


ckan_download_to_directory(_, ToDir, ApStage):-
  ap_stage_resource(ApStage, Resource, _),
  rdf_string(Resource, ckan:url, URL, _),
  (
    rdf_string(Resource, ckan:mimetype, MIME, _)
  ->
    format(atom(Accept), '~w; q=0.9', [MIME])
  ;
    Accept = ''
  ),
  ap_download_to_directory(ApStage, ToDir, URL, Accept).


rdf_format('RDF').
rdf_format('XML').
rdf_format('application/n-triples').
rdf_format('application/rdf+xml').
rdf_format('application/x-nquads').
rdf_format('application/x-ntriples').
rdf_format('example/n3').
rdf_format('example/ntriples').
rdf_format('example/rdf xml').
rdf_format('example/rdf+json').
rdf_format('example/rdf+json').
rdf_format('example/rdf+ttl').
rdf_format('example/rdf+xml').
rdf_format('example/rdfa').
rdf_format('example/turtle').
rdf_format('example/x-turtle').
rdf_format('html+rdfa').
rdf_format('linked data').
rdf_format('mapping/owl').
rdf_format('meta/owl').
rdf_format('meta/rdf-schema').
rdf_format('meta/void').
rdf_format(owl).
rdf_format('rdf-n3').
rdf_format('rdf-turtle').
rdf_format('rdf-xml').
rdf_format('rdf/n3').
rdf_format('rdf/turtle').
rdf_format('rdf/xml, html, json').
rdf_format('text/n3').
rdf_format('text/turtle').


rdf_mimetype('application/rdf+n3').
rdf_mimetype('application/rdf+xml').
rdf_mimetype('application/turtle').
rdf_mimetype('text/n3').
rdf_mimetype('text/json').
rdf_mimetype('text/rdf+n3').
rdf_mimetype('text/turtle').
%rdf_mimetype('text/xml').

