:- module(
  opendefinition_licenses,
  [
    enrich_license/2, % +License:iri
                      % +Graph:atom
    enrich_licenses/1 % +Graph:atom
  ]
).

/** <module> OpenDefinition Licenses

Support for the OpenDefinition licenses and their descriptions.

@author Wouter Beek
@see http://licenses.opendefinition.org/
@version 2014/02-2014/04
*/

:- use_module(ckan(ckan_legend)). % Legend declarations.
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_conv(json_to_rdf)).



enrich_license(Graph, License):-
  license_descriptions(JsonDescription),
  enrich_license(JsonDescription, Graph, License).

enrich_license(JsonDescription, Graph, License):-
  rdf_global_id(_:LocalName, License),
  atomic_list_concat([_,Tmp], '/', LocalName),
  downcase_atom(Tmp, Key),
  memberchk(Key=JSON_Description, JsonDescription), !,
  json_to_rdf(temp, ckan_legend, ckan, JSON_Description, Dummy),
  forall(
    rdf(Dummy, P, O, temp),
    rdf_assert(License, P, O, Graph)
  ),
  rdf_unload_graph(temp).
enrich_license(_, Graph, License):-
  rdf_assert_string(License, rdf:comment, 'Not described by OpenDefinition.', Graph),
  debug(ckan, 'Could not find license ~w in OpenDefinition descriptions.', [License]).


enrich_licenses(G):-
  license_descriptions(JsonDescription),
  aggregate_all(
    set(License),
    (
      rdfs_individual_of(License, ckan:'License'),
      rdf_term(License, G)
    ),
    Licenses
  ),
  maplist(enrich_license(JsonDescription, G), Licenses).


license_descriptions(Reply):-
  URL = 'http://licenses.opendefinition.org/licenses/groups/all.json',
  http_open(URL, Stream, []),
  json_read(Stream, json(Reply)).

