:- module(
  su_btc,
  [
    su_btc/0
  ]
).

/** <module> SemanticURIs Billion Triple Challenge

@author Wouter Beek
@version 2014/02-2014/04
*/

:- use_remote_module(ap(ap)).
:- use_remote_module(ap(ap_archive_ext)). % AP stage.
:- use_remote_module(ap(ap_db)).
:- use_remote_module(ap(ap_download)). % AP stage.
:- use_remote_module(ap(ap_rdf_serial)). % AP stage.
:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(uri_ext)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(su(rdf_compress)). % AP stages.



% Datahub
btc_url('http://km.aifb.kit.edu/projects/btc-2012/datahub/data-1.nq.gz').
% Rest
btc_url('http://km.aifb.kit.edu/projects/btc-2012/rest/data-2.nq.gz').
% Timbl
btc_url('http://km.aifb.kit.edu/projects/btc-2012/timbl/data-4.nq.gz').

su_btc:-
  create_ap_collection(ApCollection),
  rdfs_assert_label(ApCollection, btc, ap),
  forall(
    btc_url(Url),
    su_btc(ApCollection, Url)
  ).

su_btc(ApCollection, Url):-
  create_ap(ApCollection, Ap),
  
  url_nested_directory(data(.), Url, Dir),
  Alias = Url,
  db_add_novel(user:file_search_path(Alias, Dir)),
  rdf_assert_string(Ap, ap:alias, Alias, ap),
  
  ap(
    [leave_trail(false),reset(true)],
    Ap,
    [
      su_btc:ap_stage([name('Download'),args([Url])], btc_download_to_directory),
      su_btc:ap_stage([name('Arch')], extract_archives),
      su_ap:ap_stage(
        [name('toTurtle'),args(['application/x-turtle'])],
        ap_rdf_convert_directory
      ),
      su_btc:ap_stage([name('Compress')], compress),
      su_btc:ap_stage([name('Entropies')], entropy_and_mutual_information)
    ]
  ).

btc_download_to_directory(_, ToDir, _, Url):-
  is_absolute_file_name(Url), !,
  file_name(ToFile, ToDir, temp, ttl),
  copy_file(Url, ToFile).
btc_download_to_directory(_, ToDir, ApStage, Url):-
  ap_download_to_directory(ApStage, ToDir, Url, '').

