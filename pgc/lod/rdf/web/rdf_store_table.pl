:- module(
  rdf_store_table,
  [
    rdf_store_rows/1, % +Rows:list(list(ground))
    rdf_store_rows/3, % :Caption
                      % +Header:list(ground)
                      % +Rows:list(list(ground))
    rdf_store_rows/4 % ?Subject:or([bnode,iri])
                      % ?Predicate:iri
                      % ?Object:or([bnode,iri,literal])
                      % ?Graph:atom
  ]
).

/** <module> RDF store table

Predicates that allows RDF tables to be asserted
 in the Prolog console and displayed in the Web browser.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_remote_module(generics(uri_query)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(www_browser)).
:- use_remote_module(rdf_web(rdf_html_table)).

:- meta_predicate(rdf_store_rows(:,+,+)).

:- rdf_meta(rdf_store_rows(t)).
:- rdf_meta(rdf_store_rows(:,+,t)).
:- rdf_meta(rdf_store_rows(r,r,r,+)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(store_table), rdf_store_table, []).

%! rdf_store_rows_(
%!   ?Timestamp:positive_integer,
%!   :Caption,
%!   ?Header:list(list(ground)),
%!   ?Rows:list(list(ground))
%! ) is nondet.

:- dynamic(rdf_store_rows_/4).



%! rdf_store_rows(+Rows:list(list(ground))) is det.
% Stores the given number of rows into Prolog memory
% in a form that allows easy retrieval for Web table construction.
% Then, opens the default Web browser (if any) to display
% this table.
%
% @see rdf_store_rows/4 is a predicate that produces
%      such rows (and that are S-P-O-G quadruples).

rdf_store_rows(Rows):-
  rdf_store_rows(_, _, Rows).

rdf_store_rows(Caption, Header, Rows):-
  get_time(Timestamp),
  assert(rdf_store_rows_(Timestamp, Caption, Header, Rows)),
  http_absolute_uri(rdf(store_table), Link1),
  uri_query_add(Link1, timestamp, Timestamp, Link2),
  www_open_url(Link2).


%! rdf_store_rows(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:atom
%! ) is det.
% Stores all quandruples that match the given instantiation pattern.
% Each of the quadruple elements can be left uninstantiated
% -- or partially instantiated in the case of a literal object term --
% in order to match more ground quadruples.

rdf_store_rows(S, P, O, G):-
  aggregate_all(
    set([S,P,O,G]),
    rdf(S, P, O, G),
    Quadruples
  ),
  rdf_store_rows(Quadruples).


%! rdf_store_table(+Request:list(nvpair)) is det.
% Generates an HTML page describing the most recently asserted RDF table.
%
% @see rdf_store_rows/[1,4] for asserting RDF tables from the Prolog console.

rdf_store_table(Request):-
  request_query_read(Request, timestamp, Timestamp), !,
  once(rdf_store_rows_(Timestamp, Caption, Header, Rows)),
  format_time(atom(FormattedTime), '%FT%T%:z', Timestamp),
  reply_html_page(
    app_style,
    title(['RDF store table - ',FormattedTime]),
    \rdf_html_table(
      [header_row(true),index(true)],
      Caption,
      [Header|Rows])
  ).
rdf_store_table(_Request):-
  aggregate_all(
    set([Timestamp,Length]),
    (
      rdf_store_rows_(Timestamp, _, _, Rows),
      length(Rows, Length)
    ),
    Rows
  ),
  reply_html_page(
    app_style,
    title('RDF store table'),
    \rdf_html_table(
      [header_row(true),indexed(true)],
      html('Overview of RDF stored tables'),
      Rows
    )
  ).

