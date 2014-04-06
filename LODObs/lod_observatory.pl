:- module(lod_observatory, [lod_observatory/0]).

/** <module> LOD observatory

Script for creating an LOD observatory.

@author Wouter Beek
@version 2014/02
*/

:- use_remote_module(ap(ap_file_size)).
:- use_remote_module(ap(ap_rdf_serial)).
:- use_remote_module(ap(ap_void_stat)).
:- use_remote_module(ckan(ckan_ap)).
:- use_remote_module(ckan(ckan_table)). % Debug tool.
:- use_remote_module(ckan(ckan_file_size)). % Debug tool.
:- use_module(library(apply)).
:- use_remote_module(lodobs(lodobs_five_star)). % Debug tool.
:- use_remote_module(os(dir_ext)).
:- use_remote_module(rdf(rdf_build)).

:- use_module(library(debug)).
:- debug(ap).

lod_observatory:-
  ckan_ap([lod_observatory:ap_stage([name('VoID')], void_statistics)]).

