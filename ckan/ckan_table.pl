:- module(ckan_table, []).

/** <module> CKAN table

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_remote_module(ap(ap_table)).
:- use_remote_module(ckan(ckan_db)).
:- use_remote_module(rdf(rdf_container)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(server(web_modules)).

:- use_module(library(aggregate)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).

http:location(ckan, root(ckan), []).
:- http_handler(ckan(table), ckan_table, []).

user:web_module('CKAN Table', ckan_table).



ckan_table(_Request):-
  ap_table(ckan_header, ckan_row).

ckan_header(
  Header,
  ['Resource','Name','Title','Organization','Users','Tags'|Header]
).

ckan_row([H|T], [Resource,Name,Title,Organization,Users,Tags,H|T]):-
  rdf_collection_member(H, AP, ap),
  rdf(AP, ap:resource, Resource, ap),
  once(rdf(Package, ckan:resources, Resource, _)),
  once(rdf_string(Package, ckan:name, Name, _)),
  (
    rdf_string(Package, ckan:title, Title, _), !
  ;
    Title = notitle
  ),

  % Organization.
  once(rdf(Package, ckan:organization, X, _)),
  once(rdf_string(X, ckan:display_name, Organization, _)),

  % Users.
  aggregate_all(
    set(UserName),
    (
      rdf(X, ckan:users, User, _),
      rdf_string(User, ckan:fullname, UserName, _)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', Users),

  % Tags.
  aggregate_all(
    set(TagName),
    (
      rdf(Package, ckan:tags, Tag, _),
      rdf_string(Tag, ckan:name, TagName, _)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', Tags).

