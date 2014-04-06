:- module(lodobs_waterfall, []).

/** <module> LOD Observator Waterfall

@author Wouter Beek
@version 2014/02
*/

:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(server(web_modules)).
:- use_module(xml(xml_namespace)).

http:location(lodobs, root(lodobs), []).
:- http_handler(lodobs(waterfall), lodobs_waterfall, []).

user:web_module('LODObs Waterfall', lodobs_waterfall).



lodobs_waterfall(_Request):-
  reply_html_page(
    app_style,
    title('LOD Observator - Waterfall overview'),
    html([
      h1('Waterfall table'),
      \waterfall_table(
        `Five star data waterfall`,
        Resources,
        [
          'Is computed for this LOD Observatory'-is_computed_by_lodobs,
          'Has URL string'-has_url_string,
          'Has URL string that parses according to RFC xxx'-has_rfc_url,
          'Host was found'-host_found,
          'Connection was accepted'-connection_accepted,
          'No HTTP 4xx status code'-does_not_have_http_4xx,
          'No HTTP 5xx status code'-does_not_have_http_5xx,
          'Has a license sting'-has_license_string,
          'Has a recognized license'-has_license,
          'Has an open license'-has_open_license,
          'Has MIME string'-has_mime_string,
          'Has MIME string that is registered with IANA'-has_iana_mime
        ]
      )
    ])
  ).

waterfall_table(Caption, Resources, Pairs) -->
  {waterfall_table_rows(Resources, Pairs, Rows)},
  html(
    \html_table(
      [header_row(true),indexed(true)],
      Caption,
      [['Property','Number of datasets']|Rows]
    )
  ).

% No more resources left.
waterfall_table_rows([], _, []):- !.
% No more pairs left.
waterfall_table_rows(_, [], []):- !.
waterfall_table_rows(
  Resources,
  [Label-Goal|T],
  [[Label,NumberOfResourcesIn]|Rows]
):-
  partition(Goal, Resources, ResourcesIn, _ResourcesOut),
  length(ResourcesIn, NumberOfResourcesIn),
  waterfall_table_rows(ResourcesIn, T, Rows).

