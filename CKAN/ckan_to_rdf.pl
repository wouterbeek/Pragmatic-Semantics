:- module(
  ckan_to_rdf,
  [
    ckan_to_rdf/2 % +Site:atom
                  % ?RdfGraph:atom
  ]
).

/** <module> CKAN to RDF conversion

Automated CKAN to RDF conversion.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(ckan(ckan_api)).
:- use_module(ckan(ckan_db)).
:- use_module(ckan(ckan_licenses)).
:- use_module(ckan(ckan_mime)).
:- use_module(generics(meta_ext)).
:- use_module(generics(list_script)).
:- use_module(library(debug)).
:- use_module(library(option)).



%! ckan_to_rdf(+Site:atom, ?RdfGraph:atom) is det.
% Scrapes the given CKAN site and asserts all of its contents in RDF.

ckan_to_rdf(Site, G):-
  % Options are passed to the predicates in module [ckan].
  default_goal(ckan_graph(Site), G),
  ckan_properties(Site, OPl),
  merge_options([graph(G)], OPl, ORdf),

  % Conversion to RDF requires presence of graph option.
  % Conversion to PL requires absence of graph option.
  debug(ckan, 'Begin CKAN-to-RDF conversion.\n', []),

  % Make sure the CKAN site is online.
  site_read(OPl), !,

  % Groups
  group_list(OPl, _, _, _, _, Groups),
  list_script(group_show(ORdf), 'Groups', Groups, GroupsRemaining),
  debug(ckan, 'Remaining groups: ~w', [GroupsRemaining]),

  % Licenses.
  license_list(ORdf, _),
  % Enrich the licenses with information from OpenDefinition/OKF.
  ckan_clean_license(G),
  ckan_clean_mime(G),

  % Organizations.
  organization_list(OPl, _, _, _, _, Organizations),
  list_script(
    organization_show(ORdf),
    'Organizations',
    Organizations,
    RemainingOrganizations
  ),
  debug(ckan, 'Remaining organizations: ~w', [RemainingOrganizations]),

  % Packages.
  package_list(OPl, _, _, Packages),
  list_script(package_show(ORdf), 'Packages', Packages, RemainingPackages),
  debug(ckan, 'Remaining packages: ~w', [RemainingPackages]),

  % Revisions.
  % @tbd The request for revisions crashes the site.
  %%%%revision_list(OPl, Revisions),
  %%%%list_script(revision_show(ORdf), 'Revisions', Revisions,
  %%%%    RemainingRevisions),
  %%%%debug(ckan, 'Remaining revisions: ~w', [RemainingRevisions]),

  % Tags.
  tag_list(OPl, _, _, _, Tags),
  list_script(tag_show(ORdf), 'Tags', Tags, RemainingTags),
  debug(ckan, 'Remaining tags: ~w', [RemainingTags]),

  % Users.
  % @tbd The request for users crashes the site.
  %%%%user_list(OPl, _, _, Users),
  %%%%list_script(user_show(ORdf, _), 'Users', Users, RemainingUsers),
  %%%%debug(ckan, 'Remaining users: ~w', [RemainingUsers]),

  debug(ckan, 'End CKAN-to-RDF conversion.\n', []).

