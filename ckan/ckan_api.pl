:- module(
  ckan_api,
  [
    current_package_list_with_resources/3, % +Options:list(nvpair)
                                           % +Limit:positive_integer
                                           % +Offset:positive_integer
    current_package_list_with_resources/4, % +Options:list(nvpair)
                                           % +Limit:positive_integer
                                           % +Offset:positive_integer
                                           % -Resources:list
    format_autocomplete/3, % +Options:list(nvpair)
                           % +Limit:integer
                           % +Q:atom
    format_autocomplete/4, % +Options:list(nvpair)
                           % +Limit:integer
                           % +Q:atom
                           % -Formats:list(atom)
    group_list/5, % +Options:list(nvpair)
                  % +AllFields:boolean
                  % +Field:oneof([name,packages])
                  % +GroupNames:list(atom)
                  % +Order:atom
    group_list/6, % +Options:list(nvpair)
                  % +AllFields:boolean
                  % +Field:oneof([name,packages])
                  % +GroupNames:list(atom)
                  % +Order:atom
                  % -Groups:or([list(atom),list(compound)])
    group_list_authz/3, % +Options:list(nvpair)
                        % +AmMember:boolean
                        % +AvailableOnly:boolean
    group_list_authz/4, % +Options:list(nvpair)
                        % +AmMember:boolean
                        % +AvailableOnly:boolean
                        % -Groups:list(compound)
    group_package_show/3, % +Options:list(nvpair)
                          % +Limit:integer
                          % +IdOrName:atom
    group_package_show/4, % +Options:list(nvpair)
                          % +Limit:integer
                          % +IdOrName:atom
                          % -Packages:list(compound)
    group_revision_list/2, % +Options:list(nvpair),
                           % +NameOrId:atom,
    group_revision_list/3, % +Options:list(nvpair),
                           % +NameOrId:atom,
                           % -Revisions:list(compound)
    group_show/2, % +Options:list(nvpair)
                  % +IdOrName:atom
    group_show/3, % +Options:list(nvpair)
                  % +IdOrName:atom
                  % -Group:compound
    license_list/1, % +Options:list(nvpair)
    license_list/2, % +Options:list(nvpair)
                    % -Licenses:list(compound)
    member_list/4, % +Options:list(nvpair)
                   % +Capacity:atom
                   % +ObjectType:atom
                   % +IdOrName:atom
    member_list/5, % +Options:list(nvpair)
                   % +Capacity:atom
                   % +ObjectType:atom
                   % +IdOrName:atom
                   % -Triples:list(triple(atom,atom,atom))
    organization_list/5, % +Options:list(nvpair)
                         % +AllFields:boolean
                         % +Field:oneof([name,packages])
                         % +Order:atom
                         % +OrganizationsFilter:list(atom)
    organization_list/6, % +Options:list(nvpair)
                         % +AllFields:boolean
                         % +Field:oneof([name,packages])
                         % +Order:atom
                         % +OrganizationsFilter:list(atom)
                         % -Organizations:list(atom)
    organization_list_for_user/2, % +Options:list(nvpair),
                                  % +Permission:atom,
    organization_list_for_user/3, % +Options:list(nvpair),
                                  % +Permission:atom,
                                  % -Organizations:list(compound)
    organization_show/2, % +Options:list(nvpair)
                         % +IdOrName:atom
    organization_show/3, % +Options:list(nvpair)
                         % +IdOrName:atom
                         % -Organization:compound
    package_autocomplete/3, % +Options:list(nvpair)
                            % +Limit:integer
                            % +Q:atom
    package_autocomplete/4, % +Options:list(nvpair)
                            % +Limit:integer
                            % +Q:atom
                            % -Packages:list(compound)
    package_list/3, % +Options:list(nvpair)
                    % +Limit:integer
                    % +Offset:integer
    package_list/4, % +Options:list(nvpair)
                    % +Limit:integer
                    % +Offset:integer
                    % -Packages:list(atom)
    package_relationships_list/4, % +Options:list(nvpair)
                                  % +Id1:atom
                                  % +Id2:atom
                                  % +Rel:atom
    package_relationships_list/5, % +Options:list(nvpair)
                                  % +Id1:atom
                                  % +Id2:atom
                                  % +Rel:atom
                                  % -Relationships:list(compound)
    package_revision_list/2, % +Options:list(nvpair)
                             % +Package:atom
    package_revision_list/3, % +Options:list(nvpair)
                             % +Package:atom
                             % -Revisions:list(compound)
    package_show/2, % +Options:list(nvpair)
                    % +IdOrName:atom
    package_show/3, % +Options:list(nvpair)
                    % +IdOrName:atom
                    % -Package:compound
    related_list/6, % +Options:list(nvpair)
                    % ?Dataset:compound
                    % ?Featured:boolean
                    % ?IdOrName:atom
                    % ?Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc])
                    % ?TypeFilter:atom
    related_list/7, % +Options:list(nvpair)
                    % ?Dataset:compound
                    % ?Featured:boolean
                    % ?IdOrName:atom
                    % ?Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc])
                    % ?TypeFilter:atom
                    % -Related:list(compound)
    related_show/2, % +Options:list(nvpair)
                    % +Id:atom
    related_show/3, % +Options:list(nvpair)
                    % +Id:atom
                    % -Out:compound
    resource_show/2, % +Options:list(nvpair)
                     % +Id:atom
    resource_show/3, % +Options:list(nvpair)
                     % +Id:atom
                     % -Resource:compound
    resource_status_show/2, % +Options:list(nvpair)
                            % +Id:atom
    resource_status_show/3, % +Options:list(nvpair)
                            % +Id:atom
                            % -Statuses:list(list)
    revision_list/1, % +Options:list(nvpair)
    revision_list/2, % +Options:list(nvpair)
                     % -Revisions:list(atom)
    revision_show/2, % +Options:list(nvpair)
                     % +Id:atom
    revision_show/3, % +Options:list(nvpair)
                     % +Id:atom
                     % -Revision:compound
    site_read/1, % +Options:list(nvpair)
    tag_list/4, % +Options:list(nvpair)
                % +AllFields:boolean
                % +Query:atom
                % +VocabularyId:atom
    tag_list/5, % +Options:list(nvpair)
                % +AllFields:boolean
                % +Query:atom
                % +VocabularyId:atom
                % -Tags:or([list(atom),list(compound)])
    tag_show/2, % +Options:list(nvpair)
                % +IdOrName:atom
    tag_show/3, % +Options:list(nvpair)
                % +IdOrName:atom
                % -Tag:compound
    user_autocomplete/3, % +Options:list(nvpair)
                         % +Limit:integer
                         % +Q:atom
    user_autocomplete/4, % +Options:list(nvpair)
                         % +Limit:integer
                         % +Q:atom
                         % -Users:list(compound)
    user_list/3, % +Options:list(nvpair)
                 % +OrderBy:atom
                 % +Q:atom
    user_list/4, % +Options:list(nvpair)
                 % +OrderBy:atom
                 % +Q:atom
                 % -Users:list(compound)
    user_show/3, % +Options:list(nvpair),
                 % +UserObject:compound,
                 % +IdOrName:atom,
    user_show/4 % +Options:list(nvpair),
                % +UserObject:compound,
                % +IdOrName:atom,
                % -User:compound
  ]
).

/** <module> CKAN API

Querying the CKAN API.

For each CKAN API function there are two predicates in this module:
  1. For asserting the output in RDF, without the last return parameter,
     requiring option `graph(Graph:atom)`.
  2. For retrieving the result in Prolog compound terms.

The following options are API-wide supported:
  * =|deprecated(Deprecated:boolean)|=
    Use the deprecated API.
  * =|paginated(Paginated:boolean)|=
    Use pagination in order to retrieve all results.

The following depretations (v.2.0.3) are supported:
  * For: current_package_list_with_resources/4
    Use parameter name `page` i.o. `offset`.
  * For: license_list/2
    Use licence_list/2 instead.

The following API call are not supported:
  * Solr querying with `package_search`, `resource_search`, `tag_search`.
  * `tag_autocomplete`

@author Wouter Beek
@version 2014/01
*/

:- use_remote_module(ckan, ckan(ckan)).
:- use_remote_module(pgc,  generics(meta_ext)).
:- use_remote_module(pgc,  generics(option_ext)).
:- use_remote_module(pgc,  standards(json_ext)).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).



%! current_package_list_with_resources(
%!   +Options:list(nvpair),
%!   +Limit:positive_integer,
%!   +Offset:positive_integer
%! ) is det.
%! current_package_list_with_resources(
%!   +Options:list(nvpair),
%!   +Limit:positive_integer,
%!   +Offset:positive_integer,
%!   -Packages:list(compound)
%! ) is det.
% Return a list of the site's datasets (packages) and their resources.
%
% @arg Options
% @arg Limit If given, the list of datasets will be broken into
%      pages of at most `limit` datasets per page and only one page
%      will be returned at a time.
% @arg Offset If `limit` is given, the offset to start returning packages
%      from.
% @arg Packages

current_package_list_with_resources(O1, Limit, Offset):-
  current_package_list_with_resources(O1, Limit, Offset, _).

current_package_list_with_resources(O1, Limit, Offset, Packages):-
  select_option(paginated(true), O1, O2), !,
  default(10, Limit),
  default(1, Offset),
  paginated_current_package_list_with_resources(
    O2,
    Limit,
    Offset,
    Packages
  ).
current_package_list_with_resources(O1, Limit, Offset, Packages):-
  process_limit_offset(O1, Limit, Offset, P1),
  ckan(O1, current_package_list_with_resources, P1, Packages).

paginated_current_package_list_with_resources(O1, Limit, Offset1, L3):-
  current_package_list_with_resources(O1, Limit, Offset1, L1), !,
  debug(ckan, 'Offset: ~:d\n~w\n\n\n', [Offset1,L1]),
  Offset2 is Offset1 + Limit,
  paginated_current_package_list_with_resources(O1, Limit, Offset2, L2),
  append(L1, L2, L3).
paginated_current_package_list_with_resources(_, _, _, []).


%! format_autocomplete(+Options:list(nvpair), +Limit:integer, +Q:atom) is det.
%! format_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Formats:list(compound)
%! ) is det.
% Returns a list of resource formats whose names contain a string.
%
% @arg Options
% @arg Q The string to search for.
% @arg Limit The maximum number of resource formats to return
%      Default: 5.
% @arg Formats A list of format strings.

format_autocomplete(O1, Limit, Q):-
  format_autocomplete(O1, Limit, Q, _).

format_autocomplete(O1, Limit, Q, Formats):-
  default(5, Limit),
  ckan(O1, format_autocomplete, [limit=Limit,q=Q], Formats).


%! group_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +GroupNames:list(atom),
%!   +Order:atom
%! ) is det.
%! group_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +GroupNames:list(atom),
%!   +Order:atom,
%!   -Groups:list(atom)
%! ) is det.
% Return a list of the names of the site's groups.
%
% @arg Options
% @arg AllFields Whether full group dictionaries should be returned
%      instead of just names.
%      Default: `false`.
% @arg Field Sorting of the search results based on this field.
%      The allowed fields are `name` and `packages`.
%      Default: `name`.
% @arg GroupNames A list of names of the groups to return, if given
%      only groups whose names are in this list will be returned.
%      Optional.
% @arg Order The sort-order used.
%      Default: `asc`
% @arg Groups A list of the atomic names of the site's groups.

group_list(O1, AllFields, Field, GroupNames, Order):-
  group_list(O1, AllFields, Field, GroupNames, Order, _).

group_list(O1, AllFields1, Field, GroupNames, Order, Groups):-
  default(false, AllFields1),
  json_boolean(AllFields1, AllFields2),
  process_field_order_sort(Field, Order, Sort),
  add_option([all_fields=AllFields2,sort=Sort], groups, GroupNames, P1),
  ckan(O1, group_list, P1, Groups).


%! group_list_authz(
%!   +Options:list(nvpair),
%!   +AmMember:boolean,                        
%!   +AvailableOnly:boolean
%! ) is det.
%! group_list_authz(
%!   +Options:list(nvpair),
%!   +AmMember:boolean,
%!   +AvailableOnly:boolean,
%!   -Groups:list(compound)
%! ) is det.
% Return the list of groups that the user is authorized to edit.
%
% @arg Options
% @arg AmMember If `true` return only the groups the logged-in user
%      is a member of, otherwise return all groups that the user
%      is authorized to edit (for example, sysadmin users
%      are authorized to edit all groups) (optional, default: `false`).
% @arg AvailableOnly Remove the existing groups in the package
%      (optional, default: `false`).
% @arg Groups List of dictized groups that the user is authorized to edit.

group_list_authz(O1, AmMember, AvailableOnly):-
  group_list_authz(O1, AmMember, AvailableOnly, _).

group_list_authz(O1, AmMember1, AvailableOnly1, Groups):-
  default(false, AvailableOnly1),
  json_boolean(AvailableOnly1, AvailableOnly2),
  default(false, AmMember1),
  json_boolean(AmMember1, AmMember2),
  ckan(
    O1,
    group_list_authz,
    [am_member=AmMember2,available_only=AvailableOnly2],
    Groups
  ).


%! group_package_show(
%!   +Options:list(nvpair)
%!   +Limit:integer
%!   +IdOrName:atom
%! ) is det.
%! group_package_show(
%!   +Options:list(nvpair)
%!   +Limit:integer
%!   +IdOrName:atom
%!   -Packages:list(compound)
%! ) is det.
% Returns the datasets (packages) of a group.
%
% @arg Options
% @arg Limit The maximum number of datasets to return (optional).
% @arg IdOrName The id or name of the group.
% @arg Packages A list of packages.

group_package_show(O1, Limit, IdOrName):-
  group_package_show(O1, Limit, IdOrName, _).

group_package_show(O1, Limit, IdOrName, Packages):-
  add_option([id=IdOrName], limit, Limit, P1),
  ckan(O1, group_package_show, P1, Packages).


%! group_revision_list(+Options:list(nvpair), +NameOrId:atom) is det.
%! group_revision_list(
%!   +Options:list(nvpair),
%!   +NameOrId:atom,
%!   -Revisions:list(compound)
%! ) is det.
% Return a group's revisions.
%
% @arg Options
% @arg NameOrId The name or id of the group.
% @arg Revisions List of dictionaries.

group_revision_list(O1, NameOrId):-
  group_revision_list(O1, NameOrId, _).

group_revision_list(O1, NameOrId, Revisions):-
  ckan(O1, group_revision_list, [id=NameOrId], Revisions).


%! group_show(+Options:list(nvpair), +IdOrName:atom) is det.
%! group_show(+Options:list(nvpair), +IdOrName:atom, -Group:compound) is det.
% Returns the details of a group.
%
% @arg Options
% @arg IdOrName The id or name of the group.
% @arg Group A group.

group_show(O1, Id):-
  group_show(O1, Id, _).

group_show(O1, Id, Group):-
  ckan(O1, group_show, [id=Id], Group).


%! license_list(+Options:list(nvpair)) is det.
%! license_list(+Options:list(nvpair), -Licenses:list(compound)) is det.
% Return the list of licenses available for datasets on the site.
%
% @arg Options
% @arg Licenses List of dictionaries.

license_list(O1):-
  license_list(O1, _).

license_list(O1, Licenses):-
  (
    option(deprecated(true), O1)
  ->
    FunctionName = licence_list
  ;
    FunctionName = license_list
  ),

  ckan(O1, FunctionName, [], Licenses).


%! member_list(
%!   +Options:list(nvpair),
%!   +Capacity:atom,
%!   +ObjectType:atom,
%!   +IdOrName:atom
%! ) is det.
%! member_list(
%!   +Options:list(nvpair),
%!   +Capacity:atom,
%!   +ObjectType:atom,
%!   +IdOrName:atom,
%!   -Triples:list(triple(atom,atom,atom))
%! ) is det.
% Return the members of a group.
%
% The user must have permission to ‘get’ the group.
%
% @arg Capacity Restrict the members returned to those with a given capacity,
%      e.g. `member`, `editor`, `admin`, `public`, `private`
%      (optional, default: `None`)
% @arg ObjectType Restrict the members returned to those of a given type,
%      e.g. `user` or `package` (optional, default: `None`).
% @arg IdOrName The id or name of the group.
% @arg Triples A list of <id,type,capacity>-triples.
%
% @throw ckan.logic.NotFound If the group does not exist.

member_list(O1, Capacity, ObjectType, IdOrName):-
  member_list(O1, Capacity, ObjectType, IdOrName, _).

member_list(O1, Capacity, ObjectType, IdOrName, Triples):-
  P1 = [id=IdOrName],
  add_option(P1, capacity, Capacity, P2),
  add_option(P2, object_type, ObjectType, P3),
  ckan(O1, member_list, P3, Triples).


%! organization_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +Order:atom,
%!   +OrganizationsFilter:list(atom)
%! ) is det.
%! organization_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Field:oneof([name,packages]),
%!   +Order:atom,
%!   +OrganizationsFilter:list(atom),
%!   -Organizations:list(atom)
%! ) is det.
% Return a list of the names of the site’s organizations.
%
% @arg AllFields Return full group dictionaries instead of just names
%      (optional, default: `false`).
% @arg Field Sorting of the search results based on this field.
%      The allowed fields are `name` and `packages`.
%      Default: `name`.
% @arg Order The sort-order used.
%      Default: `asc`
% @arg OrganizationsFilter A list of names of the groups to return,
%      if given only groups whose names are in this list
%      will be returned (optional).
% @arg Organizations A list of organization names.

organization_list(O1, AllFields, Field, Order, OrganizationsFilter):-
  organization_list(O1, AllFields, Field, Order, OrganizationsFilter, _).

organization_list(
  O1,
  AllFields1,
  Field,
  Order,
  OrganizationsFilter,
  Organizations
):-
  default(false, AllFields1),
  json_boolean(AllFields1, AllFields2),
  process_field_order_sort(Field, Order, Sort),
  add_option(
    [all_fields=AllFields2,sort=Sort],
    organizations,
    OrganizationsFilter,
    P1
  ),
  ckan(O1, organization_list, P1, Organizations).


%! organization_list_for_user(+Options:list(nvpair), +Permission:atom) is det.
%! organization_list_for_user(
%!   +Options:list(nvpair),
%!   +Permission:atom,
%!   -Organizations:list(compound)
%! ) is det.
% Return the list of organizations that the user is a member of.
%
% @arg Options
% @arg Permission The permission the user has against
%      the returned organizations (optional, default: `edit_group`).
% @arg Organizations List of dictized organizations
%      that the user is authorized to edit.

organization_list_for_user(O1, Permission):-
  organization_list_for_user(O1, Permission, _).

organization_list_for_user(O1, Permission, Organizations):-
  default(edit_group, Permission),
  ckan(
    O1,
    organization_list_for_user,
    [permission=Permission],
    Organizations
  ).


%! organization_show(+Options:list(nvpair), +IdOrName:atom) is det.
%! organization_show(
%!   +Options:list(nvpair),
%!   +IdOrName:atom,
%!   -Organization:compound
%! ) is det.
% Returns the details of an organization.
%
% @arg Options
% @arg IdOrName The id or name of the organization.
% @arg Organization An organization.

organization_show(O1, IdOrName):-
  organization_show(O1, IdOrName, _).

organization_show(O1, IdOrName, Organization):-
  ckan(O1, organization_show, [id=IdOrName], Organization).


%! package_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom
%! ) is det.
%! package_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Packages:list(compound)
%! ) is det.
% Returns a list of datasets (packages) that match a string.
%
% Datasets with names or titles that contain the query string
%  will be returned.
%
% @arg Options
% @arg Limit The maximum number of resource formats to return
%      Default: 10.
% @arg Q The string to search for.
% @arg Package A list of packages.

package_autocomplete(O1, Limit, Q):-
  package_autocomplete(O1, Limit, Q, _).

package_autocomplete(O1, Limit, Q, Packages):-
  default(10, Limit),
  ckan(O1, package_autocomplete, [limit=Limit,q=Q], Packages).


%! package_list(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Offset:integer
%! ) is det.
%! package_list(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Offset:integer,
%!   -Packages:list(atom)
%! ) is det.
% Return a list of the names of the site's datasets (packages).
%
% @arg Options
% @arg Limit If given, the list of datasets will be broken into pages
%      of at most `Limit` datasets per page and only one page
%      will be returned at a time (optional).
% @arg Offset If `limit` is given, the offset to start returning packages
%      from.
% @arg Packages A list of atomic package/dataset names.
%      The list is sorted most-recently-modified first.

package_list(O1, Limit, Offset):-
  package_list(O1, Limit, Offset, _).

package_list(O1, Limit, Offset, Packages):-
  process_limit_offset(O1, Limit, Offset, P2),
  ckan(O1, package_list, P2, Packages).


%! package_relationships_list(
%!   +Options:list(nvpair),
%!   +Id1:atom,
%!   +Id2:atom,
%!   +Rel:atom
%! ) is det.
%! package_relationships_list(
%!   +Options:list(nvpair),
%!   +Id1:atom,
%!   +Id2:atom,
%!   +Rel:atom,
%!   -Relationships:list(compound)
%! ) is det.
% Returns a dataset (package)'s relationships.
%
% @arg Options
% @arg Id1 The id or name of the first package.
% @arg Id2 The id or name of the second package.
% @arg Rel Relationship as string,
%      see package_relationship_create/4 (optional).
%      [Is this a filter by relation type?]
% @arg Relationships A list of relationships.
%
% @see package_relationship_create/4 for the relationship types.

package_relationships_list(O1, Id1, Id2, Rel):-
  package_relationships_list(O1, Id1, Id2, Rel, _).

package_relationships_list(O1, Id1, Id2, Rel, Relationships):-
  add_option([id=Id1,id2=Id2], rel, Rel, P1),
  ckan(O1, package_relationships_list, P1, Relationships).


%! package_revision_list(+Options:list(nvpair), +Package:atom) is det.
%! package_revision_list(
%!   +Options:list(nvpair),
%!   +Package:atom,
%!   -Revisions:list(compound)
%! ) is det.
% Return a dataset (package)'s revisions as a list of dictionaries.
%
% @arg Options
% @arg Package The id or name of the dataset.
% @arg Revisions A list of revision terms.

package_revision_list(O1, Package):-
  package_revision_list(O1, Package, _).

package_revision_list(O1, Package, Revisions):-
  ckan(O1, package_revision_list, [id(Package)], Revisions).


%! package_show(+Options:list(nvpair), +IdOrName:atom) is det.
%! package_show(
%!   +Options:list(nvpair),
%!   +IdOrName:atom,
%!   -Package:compound
%! ) is det.

package_show(O1, IdOrName):-
  package_show(O1, IdOrName, _).

package_show(O1, IdOrName, Package):-
  ckan(O1, package_show, [id(IdOrName)], Package).


%! related_list(
%!   +Options:list(nvpair),
%!   +Dataset:compound,
%!   +Featured:boolean,
%!   +IdOrName:atom,
%!   +Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc]),
%!   +TypeFilter:atom
%! ) is det.
%! related_list(
%!   +Options:list(nvpair),
%!   +Dataset:compound,
%!   +Featured:boolean,
%!   +IdOrName:atom,
%!   +Sort:oneof([created_asc,created_desc,view_count_asc,view_count_desc]),
%!   +TypeFilter:atom,
%!   -Related:list(compound)
%! ) is det.
% Return a dataset's related items.
%
% Either the `IdOrName` or the `Dataset` parameter must be instantiated.
%
% @arg Options
% @arg Dataset Dataset dictionary of the dataset (optional).
% @arg Featured Whether or not to restrict the results
%      to only featured related items (optional, default: `false`)
% @arg IdOrName Id or name of the dataset (optional).
% @arg TypeFilter The type of related item to show
%      (optional, default: None, showing all items).
% @arg Sort The order to sort the related items in.
%      Possible values are `view_count_asc`, `view_count_desc`,
%      `created_asc` or `created_desc` (optional).
% @arg Related A list of dictionaries

related_list(O1, Dataset, Featured, IdOrName, TypeFilter, Sort):-
  related_list(O1, Dataset, Featured, IdOrName, TypeFilter, Sort, _).

related_list(O1, Dataset, Featured1, IdOrName, TypeFilter, Sort, Related):-
  default(false, Featured1),
  json_boolean(Featured1, Featured2),

  % Either `dataset` or `id` must be instantiated.
  \+ maplist(nonvar, [Dataset,IdOrName]),
  \+ maplist(var, [Dataset,IdOrName]),

  add_option([featured=Featured2], dataset, Dataset, P2),
  add_option(P2, id, IdOrName, P3),

  % Optional `type_filter`
  add_option(P3, type_filter, TypeFilter, P4),

  % Optional `sort`
  add_option(P4, sort, Sort, P5),

  ckan(O1, related_list, P5, Related).


%! related_show(+Options:list(nvpair), +Id:atom) is det.
%! related_show(+Options:list(nvpair), +Id:atom, -Out:compound) is det.
% Return a single related item.
%
% @arg Options
% @arg Id the id of the related item to show
% @arg Out

related_show(O1, Id):-
  related_show(O1, Id, _).

related_show(O1, Id, Out):-
  ckan(O1, related_show, [id(Id)], Out).


%! resource_show(+Options:list(nvpair), +Id:atom) is det.
%! resource_show(+Options:list(nvpair), +Id:atom, -Resource:compound) is det.
% Returns the metadata of a resource.
%
% @arg Options
% @arg Id The id of the resource.
% @arg Resource A resource.

resource_show(O1, Id):-
  resource_show(O1, Id, _).

resource_show(O1, Id, Resource):-
  ckan(O1, resource_show, [id=Id], Resource).


%! resource_status_show(+Options:list(nvpair), +Id:atom) is det.
%! resource_status_show(
%!   +Options:list(nvpair),
%!   +Id:atom,
%!   -Statuses:list(list)
%! ) is det.
% Returns the statuses of a resource's tasks.
%
% @arg Options
% @arg Id The id of the resource.
% @arg Statuses A list of
%      =|<status,date_done,traceback,task_status>|=-dictionaries.

resource_status_show(O1, Id):-
  resource_status_show(O1, Id, _).

resource_status_show(O1, Id, Statuses):-
  ckan(O1, resource_status_show, [id=Id], Statuses).


%! revision_list(+Options:list(nvpair)) is det.
%! revision_list(+Options:list(nvpair), -Revisions:list(atom)) is det.
% Return a list of the IDs of the site’s revisions.
%
% @arg Options
% @arg Revisions A list of IDs of the site's revisions.

revision_list(O1):-
  revision_list(O1, _).

revision_list(O1, Revisions):-
  ckan(O1, revision_list, [], Revisions).


%! revision_show(+Options:list(nvpair), +Id:atom) is det.
%! revision_show(+Options:list(nvpair), +Id:atom, -Revision:compound) is det.
% Returns the details of a revision.
%
% @arg Options
% @arg Id The id of the revision.
% @arg Revision A revision.

revision_show(O1, Id):-
  revision_show(O1, Id, _).

revision_show(O1, Id, Revision):-
  ckan(O1, revision_show, [id=Id], Revision).


%! site_read(+Options:list(nvpair)) is semidet.
% Suceeds if the CKAN site is readable?

site_read(O1):-
  ckan(O1, site_read, [], true).


%! tag_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Query:atom,
%!   +VocabularyId:atom
%! ) is det.
%! tag_list(
%!   +Options:list(nvpair),
%!   +AllFields:boolean,
%!   +Query:atom,
%!   +VocabularyId:atom,
%!   -Tags:or([list(atom),list(compound)])
%! ) is det.
% Returns a list of the site's tags.
%
% By default only free tags (tags that don't belong to a vocabulary)
%  are returned.
% If the `VocabularyId` argument is given then only tags belonging to
%  that vocabulary will be returned.
%
% @arg Options
% @arg AllFields Return full tag dictionaries instead of just names
%      (optional, default: `false`).
% @arg Query A tag name query to search for, if given only tags whose
%      names contain this string will be returned.
% @arg VocabularyId The id or name of a vocabulary, if give only tags
%      that belong to this vocabulary will be returned.
% @arg Tags A list of tags.

tag_list(O1, AllFields1, Query, VocabularyId):-
  tag_list(O1, AllFields1, Query, VocabularyId, _).

tag_list(O1, AllFields1, Query, VocabularyId, Tags):-
  default(false, AllFields1),
  json_boolean(AllFields1, AllFields2),
  add_option([all_fields=AllFields2], query, Query, P1),
  add_option(P1, vocabulary_id, VocabularyId, P2),
  ckan(O1, tag_list, P2, Tags).


%! tag_show(+Options:list(nvpair), +IdOrName:atom) is det.
%! tag_show(+Options:list(nvpair), +IdOrName:atom, -Tag:compound) is det.
% Returns the details of a tag and all its datasets.
%
% @arg Options
% @arg IdOrName The name or id of the tag.
% @arg Tag The details of the tag, including a list of
%      all the tag's datasets and their details

tag_show(O1, IdOrName):-
  tag_show(O1, IdOrName, _).

tag_show(O1, IdOrName, Tag):-
  ckan(O1, tag_show, [id=IdOrName], Tag).


%! user_autocomplete(+Options:list(nvpair), +Limit:integer, +Q:atom) is det.
%! user_autocomplete(
%!   +Options:list(nvpair),
%!   +Limit:integer,
%!   +Q:atom,
%!   -Users:list(compound)
%! ) is det.
% Returns a list of user names that contain a string.
%
% @arg Options
% @arg Q The string to search for.
% @arg Limit The maximum number of user names to return.
%      Default: 20.
% @arg Users A list of user dictionaries each with keys
%      `name`, `fullname`, and `id`.

user_autocomplete(O1, Limit, Q):-
  user_autocomplete(O1, Limit, Q, _).

user_autocomplete(O1, Limit, Q, Users):-
  default(20, Limit),
  ckan(O1, user_autocomplete, [limit=Limit,q=Q], Users).


%! user_list(
%!   +Options:list(nvpair),
%!   +OrderBy:atom,
%!   +Q:atom,
%!   -Users:list(compound)
%! ) is det.
% Returns a list of the site's user accounts.
%
% @arg Options
% @arg OrderBy Which field to sort the list by.
%      Default: `name`.
% @arg Q Restrict the users returned to those whose names contain
%      a[=this?] string (optional).
% @arg Users A list of users.

user_list(O1, OrderBy, Q):-
  user_list(O1, OrderBy, Q, _).

user_list(O1, OrderBy, Q, Users):-
  default(name, OrderBy),
  add_option([order_by=OrderBy], q, Q, P1),
  ckan(O1, user_list, P1, Users).


%! user_show(
%!   +Options:list(nvpair),
%!   +UserObject:compound,
%!   +IdOrName:atom
%! ) is det.
%! user_show(
%!   +Options:list(nvpair),
%!   +UserObject:compound,
%!   +IdOrName:atom,
%!   -User:compound
%! ) is det.
% Returns a user account.
%
% Either `IdOrName` or `UserObject` must be instantiated.
%
% @arg Options
% @arg UserObject The user dictionary of the user (optional).
% @arg IdOrName The id or name of the user (optional).
% @arg User A user.

user_show(O1, UserObject, IdOrName):-
  user_show(O1, UserObject, IdOrName, _).

user_show(O1, UserObject, IdOrName, User):-
  % Either `IdOrName` or `UserObject` must be instantiated.
  \+ maplist(nonvar, [IdOrName,UserObject]),
  \+ maplist(var, [IdOrName,UserObject]),

  add_option([], id, IdOrName, P1),
  add_option(P1, user_obj, UserObject, P2),

  ckan(O1, user_show, P2, User).



% CKAN API HELPERS %

%! process_field_order_sort(
%!   +Field:oneof([name,packages]),
%!   +Order:atom,
%!   -Sort:atom
%! ) is det.

process_field_order_sort(Field, Order, Sort):-
  default(name, Field),
  default(asc, Order),
  atomic_list_concat([Field,Order], ' ', Sort).


%! process_limit_offset(
%!   +Options:list(nvpair),
%!   ?Limit:integer,
%!   ?Offset:integer,
%!   -Parameters:list(nvpair)
%! ) is det.
% The `offset` option is meaningless if there is no `limit` option.

process_limit_offset(_, Limit, Offset, []):-
  nonvar(Offset),
  var(Limit), !.
process_limit_offset(O1, Limit, Offset, P2):-
  % Parameter `limit`.
  add_option([], limit, Limit, P1),

  % Parameter `offset`.
  (
    option(deprecated(true), O1, false)
  ->
    ParameterName = page
  ;
    ParameterName = offset
  ),
  add_option(P1, ParameterName, Offset, P2).

