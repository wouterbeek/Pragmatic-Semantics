:- module(
  crawler_web,
  [
    crawl_web/1, % -Markup:list
    crawl_web/2 % +Start:uri
                % -Markup:list
  ]
).

/** <module> Crawler Web

Web front-end to the crawler methods.

@author Wouter Beek
@version 2012/12
*/

:- use_module(server(crawler)).



default_start('http://www.wouterbeek.com').

crawl_web(Markup):-
  default_start(Start),
  crawl_web(Start, Markup).

crawl_web(
  Start,
  [
    title([], ['Crawler status'])
  ]/[
    element(p, [], ['Crawler', ID, 'started.'])
  ]
):-
  thread_create(
    crawl(Start),
    ID,
    []
  ).

