:- module(
  crawler,
  [
    crawl/1, % +Local:uri
    reset_crawler/0,
    unvisited/1, % -Link:uri
    visited/1 % -Link:uri
  ]
).

/** <module> Crawler

Crawler for HTML sites.

@author Wouter Beek
@version 2012/09
*/

:- use_remote_module(server(link_collection)).
:- use_remote_module(server(parser)).
:- use_remote_module(standards(html)).

:- dynamic(unvisited(_Link)).
:- dynamic(visited(_Link)).



crawl:-
  unvisited(URI),
  store_new_uri(URI),
  download_html([], URI, HTML),
  parse_dom(HTML),
  retract(unvisited(URI)),
  assert(visited(URI)),
  crawl.

crawl(Local):-
  reset_crawler,
  assert(unvisited(Local)),
  crawl.

reset_crawler:-
  retractall(unvisited(_Link1)),
  retractall(visited(_Link2)).

