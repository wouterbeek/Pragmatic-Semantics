:- module(
  parser,
  [
    is_anchor/1, % +URI:resource
    is_javascript/1, % +URI:resource
    is_mail/1, % +URI:resource
    is_partial_link/1, % +URI:resource
    parse_dom/1 % +DOM:dom
  ]
).

/** <module> Parser

Parser for HTML Web sites.

@author Wouter Beek
@version 2012/09, 2013/01-2013/02
*/

:- use_module(generic(atom_ext)).
:- use_module(library(http/http_open)).
:- use_module(server(crawler)).
:- use_module(server(link_collection)).



is_anchor(Link):-
  first_char(Link, '#').

is_javascript(Link):-
  atom_concat('javascript:', _, Link).

is_mail(Link):-
  atom_concat('mailto:', _, Link).

is_partial_link(Link):-
  first_char(Link, '/').
is_partial_link(Link):-
  first_char(Link, '?').

parse_attribute(LinkAttributes):-
  member(href=Remote, LinkAttributes),
  !,
  unvisited(Local),
  store_link(Local, Remote).
parse_attribute(_LinkAttributes).

parse_dom([]):-
  !.
parse_dom(List):-
  is_list(List),
  !,
  maplist(parse_dom, List).
parse_dom(Element):-
  parse_element(Element).

parse_element(element(a, LinkAttributes, _LinkName)):-
  !,
  parse_attribute(LinkAttributes).
parse_element(element(_Tag, _Attributes, Content)):-
  !,
  parse_dom(Content).
parse_element(Atom):-
  atomic(Atom).

store_link(_Local, Remote):-
  is_anchor(Remote),
  !.
store_link(_Local, Remote):-
  is_javascript(Remote),
  !.
store_link(_Local, Remote):-
  is_mail(Remote),
  !.
store_link(_Local, Remote):-
  visited(Remote),
  !.
store_link(_Local, Remote):-
  unvisited(Remote),
  !.
store_link(Local, Remote):-
  is_partial_link(Remote),
  !,
  atomic_list_concat([Local, Remote], FullRemote),
  store_link(Local, FullRemote).
store_link(Local, Remote):-
  assert(unvisited(Remote)),
  store_new_uri(Remote),
  store_new_link(Local, Remote),
  increment_indegree(Remote),
  increment_outdegree(Local).

