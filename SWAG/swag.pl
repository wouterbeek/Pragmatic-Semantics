:- module(swag, []).

/** <module> SWAG

The Social Web of the Avant-Garde.
Web front-end for the Social Web of the Avant-Garde.

@author Wouter Beek
@version 2013/04, 2014/01, 2014/03-2014/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(uri_ext)).
:- use_module(html(html_image)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(server(web_modules)).
:- use_module(swag(sa_clean)).
:- use_module(swag(swag_db)).

:- http_handler(root(swag), swag, [prefix]).

user:web_module('SWAG', swag).

% /css
http:location(css, root(css), []).
:- db_add_novel(user:file_search_path(css, server(css))).
%:- db_add_novel(user:file_search_path(css, swag(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix,priority(10)]).
:- html_resource(css('image.css'), []).
:- html_resource(css('page.css'), []).

% /img
http:location(img, root(img), []).
:- db_add_novel(user:file_search_path(img, data(.))).
:- http_handler(
  img(.),
  serve_files_in_directory(data),
  [prefix,priority(10)]
).



swag(_Request):-
  reply_html_page(app_style, \swag_head('Grid'), \swag_body).


swag_body -->
  {
    site_name(Site),
    random_pairs(25, Pairs)
  },
  html([
    div(id=page_title, Site),
    \html_image_thumbnail_box_grid(5, 5, 350, 350, Pairs)
  ]).


swag_head(Section) -->
  {site_name(Site)},
  html([
    title([Site,' -- ',Section]),
    \html_requires(css('image.css')),
    \html_requires(css('page.css'))
  ]).


random_pairs(N, Pairs):-
  aggregate_all(count, rdfs_individual_of(_, swag:'Entry'), Max),
  random_pairs(N, Max, Pairs).

random_pairs(0, _, []):- !.
random_pairs(N1, Max, Pairs1):-
  random_between(1, Max, I),
  rdf_datatype(Entry, swag:original_id, I, xsd:integer, _),
  (
    rdf(Entry, swag:image, Url),
    rdf_string(Entry, swag:author, Caption, _)
  ->
    url_nested_file(data(.), Url, File),
    N2 is N1 - 1,
    Pairs1 = [Caption-File|Pairs2],
    random_pairs(N2, Max, Pairs2)
  ;
    random_pairs(N1, Max, Pairs1)
  ).


site_name('The Social Web of the Avant-Garde (pre-alpha)').

