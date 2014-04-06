:- module(
  ckan_license,
  [
    ckan_clean_license/1 % +Graph:atom
  ]
).

/** <module> CKAN licenses

Based on the datahub.io JSON scrape we end up with 18 (out of 33) licenses
 that are underdefined (i.e., with no semantic description),
 impacting 207 datasets (5%).
Using case-insensitive matching of the license string with
 the repository of OpenDefinition/OKF license descriptions
 we are able to add descriptions for 14 underdefined licenses
 and additional properties for 4 licenses that were already defined.
We manually assert three additional identities in order
 to identify 1 underdefined license as a typographic variant of
 a defined license (this is `cc-by`) and to identify the remaining
 2 underdefined licenses as `ckan:None`.
After these operations all 4053 datasets have fully described
 licensing conditions.

| *License* | *Number of RDF datasets* | *Number of triples from `datahub.io`* | *Additional number of triples from OpenDefinition* | *`owl:sameAs` assertions |
| ckan:License/6                                | 1   | 1  |   | ckan:License/None  |
| ckan:License/65                               | 1   | 1  |   | ckan:License/None  |
| ckan:License/CreativeCommonsAttributionCCBY25 | 1   | 1  |   | ckan:License/cc-by |
| ckan:License/None                             | 12  | 1  |   |                    |
| ckan:License/W3C                              | 2   | 1  | 9 |                    |
| ckan:License/apache                           | 1   | 1  | 8 |                    |
| ckan:License/bsd-license                      | 3   | 1  | 9 |                    |
| ckan:License/canada-crown                     | 6   | 1  | 8 |                    |
| ckan:License/cc-by                            | 364 | 11 | 1 |                    |
| ckan:License/cc-by-sa                         | 233 | 11 |   |                    |
| ckan:License/cc-nc                            | 187 | 11 |   |                    |
| ckan:License/cc-zero                          | 203 | 11 | 1 |                    |
| ckan:License/geogratis                        | 100 | 1  | 9 |                    |
| ckan:License/gfdl                             | 41  | 11 |   |                    |
| ckan:License/gpl-2.0                          | 22  | 1  | 9 |                    |
| ckan:License/gpl-3.0                          | 3   | 1  | 9 |                    |
| ckan:License/lgpl-2.1                         | 2   | 1  | 9 |                    |
| ckan:License/mit-license                      | 4   | 1  | 9 |                    |
| ckan:License/notspecified                     | 985 | 10 |   |                    |
| ckan:License/odc-by                           | 68  | 11 |   |                    |
| ckan:License/odc-odbl                         | 118 | 11 |   |                    |
| ckan:License/odc-pddl                         | 170 | 11 | 1 |                    |
| ckan:License/other-at                         | 85  | 10 |   |                    |
| ckan:License/other-closed                     | 189 | 10 |   |                    |
| ckan:License/other-nc                         | 69  | 10 |   |                    |
| ckan:License/other-open                       | 855 | 10 |   |                    |
| ckan:License/other-pd                         | 221 | 10 |   |                    |
| ckan:License/real                             | 1   | 1  | 9 |                    |
| ckan:License/sunpublic                        | 1   | 1  | 9 |                    |
| ckan:License/uk-ogl                           | 68  | 11 | 3 |                    |
| ckan:License/ukclickusepsi                    | 22  | 1  | 8 |                    |
| ckan:License/ukcrown                          | 11  | 1  | 8 |                    |
| ckan:License/ukcrown-withrights               | 14  | 1  | 8 |                    |

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_remote_module(ckan(ckan_db)).
:- use_remote_module(ckan(opendefinition_licenses)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(owl(owl_read)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(rdf_term(rdf_boolean)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(rdfs(rdfs_build)).
:- use_remote_module(server(web_modules)).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(license_is_none(r)).

http:location(ckan, root(ckan), []).
:- http_handler(ckan(licenses), ckan_licenses, []).

user:web_module('CKAN Licenses', ckan_licenses).



ckan_licenses(_Request):-
  reply_html_page(
    app_style,
    title('CKAN Licenses'),
    html([
      h1('CKAN Licenses'),
      \ckan_license_table
    ])
  ).

ckan_license_table -->
  {
    findall(
      NumberOfResources-License,
      (
        rdfs_individual_of(License, ckan:'License'),
        aggregate_all(
          count,
          rdf(_, ckan:license_id, License),
          NumberOfResources
        )
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [NumberOfResources,LicenseName,OKD,OSI,License],
      (
        member(NumberOfResources-License, Pairs3),
        (
          rdf_string(License, ckan:title, LicenseName, _)
        ->
          true
        ;
          LicenseName = '∅'
        ),
        rdf_datatype(License, ckan:is_okd_compliant, OKD, xsd:boolean, _),
        rdf_datatype(License, ckan:is_osi_compliant, OSI, xsd:boolean, _)
      ),
      Rows
    ),

    % Extract the graph for use in the RDF HTML table.
    (
      Rows == []
    ->
      % Graph stays uninstantiated.
      true
    ;
      once((
        member(_-License, Pairs3),
        rdf([graph_mode(no_index)], License, _, _, Graph)
      ))
    )
  },
  html(
    \rdf_html_table(
      [graph(Graph)],
      html('Overview of licenses'),
      [['Number of datasets','Name','OKD Compliant','OSI Compliant','License']
          |Rows]
    )
  ).



ckan_clean_license(Graph):-
  enrich_licenses(Graph),
  
  forall(
    rdf(S, P, ckan:'License/6', Graph),
    rdf_assert(S, P, ckan:'License/None', Graph)
  ),
  rdf_retractall(S, P, ckan:'License/6', Graph),
  
  forall(
    rdf(S, P, ckan:'License/65', Graph),
    rdf_assert(S, P, ckan:'License/None', Graph)
  ),
  rdf_retractall(S, P, ckan:'License/65', Graph),
  
  forall(
    rdf(S, P, ckan:'License/CreativeCommonsAttributionCCBY25', Graph),
    rdf_assert(S, P, ckan:'License/cc-by', Graph)
  ),
  rdf_retractall(S, P, ckan:'License/CreativeCommonsAttributionCCBY25', Graph),
  
  rdfs_assert_subclass(ckan:'NoLicense', ckan:'License', Graph),
  rdfs_assert_subclass(ckan:'OpenLicense', ckan:'License', Graph),
  rdfs_assert_subclass(ckan:'ClosedLicense', ckan:'License', Graph),
  
  forall(
    rdfs_individual_of(License, ckan:'License'),
    (
      license_is_none(License)
    ->
      rdf_assert_individual(License, ckan:'NoLicense', Graph)
    ;
      license_is_open(License)
    ->
      rdf_assert_individual(License, ckan:'OpenLicense', Graph)
    ;
      rdf_assert_individual(License, ckan:'ClosedLicense', Graph)
    )
  ),
  
  forall(
    rdfs_individual_of(Package, ckan:'Package'),
    (
      rdf(Package, ckan:license_id, _)
    ->
      true
    ;
      rdf_assert(Package, ckan:license_id, ckan:'License/None', Graph)
    )
  ).

license_is_none(ckan:'License/6').
license_is_none(ckan:'License/65').
license_is_none(ckan:'License/None').
license_is_none(ckan:'License/notspecified').

license_is_open(License):-
  rdf_true(License, ckan:is_okd_compliant, _), !.
license_is_open(License):-
  rdf_true(License, ckan:is_osi_compliant, _), !.
license_is_open(License1):-
  % Using symmetry here would cause loops.
  rdf(License1, owl:sameAs, License2),
  license_is_open(License2).

