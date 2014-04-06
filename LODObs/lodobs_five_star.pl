:- module(lodobs_five_star, []).

/** <module> LOD Observatory -- Five Star Data

Web-based overview of five star LOD.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ckan(ckan_mime)).
:- use_module(html(html_table)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(uri)).
:- use_module(math(math_ext)).
:- use_module(pl_web(html_pl_term)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_term_html)).
:- use_module(server(web_modules)).
:- use_module(uri(uri_scheme)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(license_blacklist(r)).
:- rdf_meta(licensing(+,r,+,?,?)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

http:location(lodobs, root(lodobs), []).
:- http_handler(lodobs(five_star), lodobs_five_star, []).

user:web_module('LODObs', lodobs_five_star).



lodobs_five_star(_Request):-
  aggregate_all(
    set(Resource),
    rdfs_individual_of(Resource, ckan:'Resource'),
    Resources
  ),
  include(is_computed_by_lodobs, Resources, CheckedResources),
  length(CheckedResources, M0),
  reply_html_page(
    app_style,
    title('LOD Observatory - Five Star Data'),
    html([
      \star1(Resources, CheckedResources, M0),
      \star2,
      \star3(Resources, CheckedResources, M0)
    ])
  ).

star1(Resources, CheckedResources, M0) -->
  html([
    h1('First star: Make your stuff available on the Web (whatever format) under an open license'),
    \universal_locator(Resources),
    \connection(CheckedResources, M0),
    \http_status(CheckedResources, M0),
    \unarchive(M0),
    \licensing(CheckedResources, M0),
    \one_star(CheckedResources, M0)
  ]).

star2 -->
  html([
    h1('Second star: Make it available as structured data (e.g., Excel instead of image scan of a table)'),
    h2('All resources'),
    \(ckan_mime:ckan_mime_table)
  ]).

star3(_, CheckedResources, M0) -->
  html([
    h1('Third star: Use non-proprietary formats (e.g., CSV instead of Excel)'),
    \(ckan_mime:ckan_mime_table_structured_open),
    h2('Resources that could qualify as RDF'),
    \mime(CheckedResources, M0)
  ]).


mime(R0s, M0) -->
  {
    aggregate_all(
      set(MIME),
      (
        member(R1, R0s),
        rdf_string(R1, ckan:mimetype, MIME, _)
      ),
      MIMEs
    ),
    findall(
      [MIME,N1,M1],
      (
        member(MIME, MIMEs),
        findall(
          R1,
          (
            member(R1, R0s),
            rdf_string(R1, ckan:mimetype, MIME, _)
          ),
          R1s
        ),
        length(R1s, N1),
        div_zero(N1, M0, M1)
      ),
      Rows
    )
  },
  html(
    \html_table(
      [header_row(true),indexed(true)],
      html('MIME types'),
      [['MIME','Number of datasets adhering','Percentage of datasets adhering']
      |Rows]
    )
  ).


one_star(R1, M0) -->
  {
    include(host_found,                        R1, R2),
    include(connection_accepted,               R2, R3),
    include(establishing_connection_timed_out, R3, R4),
    include(permission_error_redirection_loop, R4, R5),
    include(data_reading_connection_timed_out, R5, R6),
    has_http_error('Download',                 R6, R7, _),
    exclude(could_not_unarchive,               R7, R8),
    include(has_open_license,                  R8, R9),
    length(R9, N9),
    div_zero(N9,M0,M9)
  },
  html(p([N9,' resources (',M9,'%) receive the first star.'])).


universal_locator(Resources) -->
  {
    length(Resources, M0),

    P1 = 'Resources with URL string',
    include(has_url_string, Resources, Resources1),
    length(Resources1, N1),
    div_zero(N1, M0, M1),

    P2 = 'Resources with parseable URL string',
    include(has_rfc_url, Resources, Resources2),
    length(Resources2, N2),
    div_zero(N2, M0, M2)
  },
  html([
    h2('Universal locator'),
    \html_table(
      [header_row(true),indexed(true)],
      html('Universal locator'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering'],
       [P1,N1,M1],[P2,N2,M2]]
    )
  ]).


connection(Resources, M0) -->
  {
    P1 = 'Cannot connect to resource\'s host',
    exclude(host_found, Resources, ResourcesOut1),
    length(ResourcesOut1, N1),
    div_zero(N1, M0, M1),

    P2 = 'Host was found but connection was rejected',
    exclude(connection_accepted, Resources, ResourcesOut2),
    length(ResourcesOut2, N2),
    div_zero(N2, M0, M2),

    P3 = 'Establishing connection to resource\'s host timed out',
    exclude(establishing_connection_timed_out, Resources, ResourcesOut3),
    length(ResourcesOut3, N3),
    div_zero(N3, M0, M3),

    P4 = 'Permission error: caught in redirection loop',
    exclude(permission_error_redirection_loop, Resources, ResourcesOut4),
    length(ResourcesOut4, N4),
    div_zero(N4, M0, M4),

    P5 = 'Existing connection timed out while reading data',
    exclude(data_reading_connection_timed_out, Resources, ResourcesOut5),
    length(ResourcesOut5, N5),
    div_zero(N5, M0, M5),

    P6 = 'Try again?',
    exclude(try_again, Resources, ResourcesOut6),
    length(ResourcesOut6, N6),
    div_zero(N6, M0, M6)
  },
  html([
    h2('Connection'),
    \html_table(
      [header_row(true),indexed(true)],
      html('Connection'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering'],
       [P1,N1,M1],[P2,N2,M2],[P3,N3,M3],[P4,N4,M4],[P5,N5,M5],[P6,N6,M6]]
    )
  ]).


http_status(Resources, M0) -->
  {
    has_http_error('Download', Resources, _ResourcesOut, Pairs),
    findall(
      [Label,N,M],
      (
        member(Label-ResourcesIn, Pairs),
        length(ResourcesIn, N),
        div_zero(N, M0, M)
      ),
      Rows
    )
  },
  html([
    h2('HTTP Access'),
    \html_table(
      [header_row(true),indexed(true)],
      html('HTTP Status'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering']
       |Rows]
    )
  ]).

has_http_error(AP_StageName, RIn, ROut, Results):-
  has_http_error(AP_StageName, 400, RIn, ROut, Results).

has_http_error(_, 600, Resources, Resources, []):- !.
has_http_error(
  AP_StageName,
  HTTP_Status1,
  RIn,
  ROut,
  Results2
):-
  (
    'Status-Code'(HTTP_Status1, HTTP_Description), !
  ;
    HTTP_Description = 'No description'
  ),
  format(atom(Label), '~a (~d)', [HTTP_Description,HTTP_Status1]),
  partition(
    has_error('Download', error(http_status(HTTP_Status1),_)),
    RIn,
    RInOut,
    RInIn
  ),
  (
    RInOut == []
  ->
    Results2 = Results1
  ;
    Results2 = [Label-RInOut|Results1]
  ),
  HTTP_Status2 is HTTP_Status1 + 1,
  has_http_error(
    AP_StageName,
    HTTP_Status2,
    RInIn,
    ROut,
    Results1
  ).


unarchive(M0)  -->
  {
    findall(
      Label-Resource,
      (
        rdf_string(ApStage, ap:name, 'Arch', ap),
        (
          rdf_string(ApStage, ap:has_modifier, Task, ap), !
        ;
          rdf_string(ApStage, ap:error, Atom, ap),
          read_term_from_atom(Atom, Error, []),
          Error = error(process_error(Process,_),_)
        ),
        archiver(Process, Task, Label),
        rdf_collection_member(ApStage, AP, ap),
        rdf(AP, ap:resource, Resource)
      ),
      Pairs
    ),
    group_pairs_by_key(Pairs, Joined),
    findall(
      [Label,N1,M1,Success],
      (
        member(Label-Resources1, Joined),
        length(Resources1, N1),
        div_zero(N1, M0, M1),
        include(could_not_unarchive, Resources1, Resources2),
        length(Resources2, N2),
        div_zero(N2, N1, Success)
      ),
      Rows
    )
  },
  html([
    h2('Unpacking archives'),
    \html_table(
      [header_row(true),indexed(true)],
      html('Unpacking archives'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering','Success rate for this archive format']
       |Rows]
    )
  ]).

%! archiver(?Process:atom, ?Task:atom, ?Label:atom) is nondet.

archiver('/usr/bin/unzip',  gunzipped, gunzip).
archiver('/usr/bin/gunzip', gunzipped, gunzip).

could_not_unarchive(Resource):-
  once((
    rdf(AP, ap:resource, Resource),
    rdf_collection_member(ApStage, AP, ap),
    rdf_string(ApStage, ap:name, 'Arch', ap),
    rdf_string(ApStage, ap:error, _, ap)
  )).


licensing(Resources, M0) -->
  html(h2('Licensing')),
  licensing(Resources, ckan:'NoLicense', M0),
  licensing(Resources, ckan:'ClosedLicense', M0),
  licensing(Resources, ckan:'OpenLicense', M0),
  html(h3('Open license, but non-open archive')),
  nonopen_archive.

nonopen_archive -->
  {
    findall(
      Resource,
      (
        rdf_string(ApStage, ap:name, 'Arch', ap),
        rdf_string(ApStage, ap:file, File, ap),
        file_name_extension(_, rar, File),
        rdf_collection_member(ApStage, AP, ap),
        rdf(AP, ap:resource, Resource)
      ),
      Resources
    ),
    length(Resources, N)
  },
  html(p([N,' resources'])).


licensing(Resources0, TopClass, M0) -->
  % Collect all resources under this class of licenses.
  {
    aggregate_all(
      set(Resource),
      (
        member(Resource, Resources0),
        rdf(Package, ckan:resources, Resource),
        rdf(Package, ckan:license_id, License),
        rdfs_individual_of(License, TopClass)
      ),
      Resources1
    ),
    length(Resources1, N1),
    div_zero(N1, M0, M1)
  },
  html([
    h2(\rdf_term_html(lodobs(five_star), TopClass)),
    p([
      'There are ',
      N1,
      ' resources (',
      M1,
      '%) with license ',
      \rdf_term_html(lodobs(five_star), TopClass),
      '.'
    ])
  ]),

  % Enumerate the resources per individual license.
  {
    aggregate_all(
      set(License),
      rdfs_individual_of(License, TopClass),
      Licenses
    ),
    findall(
      [P,N,M],
      (
        member(License, Licenses),
        aggregate_all(
          set(Resource),
          (
            member(Resource, Resources1),
            rdf(Package, ckan:resources, Resource),
            rdf(Package, ckan:license_id, License)
          ),
          Resources
        ),
        length(Resources, N),
        N \== 0,
        div_zero(N, M0, M),
        (
          rdf_string(License, ckan:title, Title, _)
        ->
          true
        ;
          rdf_global_id(ckan:Title, License)
        ),
        format(atom(P), 'Has license ~a', [Title])
      ),
      Rows
    )
  },
  html(
    \html_table(
      [header_row(true),indexed(true)],
      html_pl_term(TopClass),
      [['Property','Number of datasets adhering','Percentage of datasets adhering']
       |Rows]
    )
  ).



% Is computed for this LOD Observatory.
is_computed_by_lodobs(Resource):-
  once(rdf(_, ap:resource, Resource)).


% Has URL string
has_url_string(Resource):-
  once((
    rdf_string(Resource, ckan:url, URL, _),
    URL \== ''
  )).


% Has URL string that parses according to RFC xxx
% @tbd
has_rfc_url(Resource):-
  once((
    rdf_string(Resource, ckan:url, URL, _),
    uri_components(URL, uri_components(Scheme, _, _, _, _)),
    uri_scheme(Scheme)
  )).


% Host was found
host_found(Resource):-
  \+ has_error(
    'Download',
    error(socket_error('Host not found'),_),
    Resource
  ).


% Connection was accepted
connection_accepted(Resource):-
  \+ has_error(
    'Download',
    error(socket_error('Connection refused'),_),
    Resource
  ).


% Connection times out: two variants...
establishing_connection_timed_out(Resource):-
  \+ has_error(
    'Download',
    error(socket_error('Connection timed out'),_),
    Resource
  ).


% Permission error: redirection loop.
permission_error_redirection_loop(Resource):-
  \+ has_error(
    'Download',
    error(permission_error(redirect,http,_URL),context(_,'Redirection loop')),
    Resource
  ).


% The connection timed out while reading data.
data_reading_connection_timed_out(Resource):-
  \+ has_error(
    'Download',
    error(timeout_error(read,_Stream),_),
    Resource
  ).


% Try again?
try_again(Resource):-
  \+ has_error(
    'Download',
    error(socket_error('Try Again'),_),
    Resource
  ).


% No HTTP 4xx status code
does_not_have_http_4xx(Resource):-
  \+ ((
    has_error('Download', error(http_status(HTTP_Status),_), Resource),
    between(400, 499, HTTP_Status)
  )).


% No HTTP 5xx status code
does_not_have_http_5xx(Resource):-
  \+ ((
    has_error('Download', error(http_status(HTTP_Status),_), Resource),
    between(500, 599, HTTP_Status)
  )).


% Has a license sting.
has_license_string(Resource):-
  once((
    rdf(Package, ckan:resources, Resource),
    rdf(Package, ckan:license_id, _)
  )).


% Has a recognized license.
% @tbd
has_license(Resource):-
  once((
    rdf(Package, ckan:resources, Resource),
    rdf(Package, ckan:license_id, License),
    is_license(License)
  )).

is_license(License):-
  rdfs_individual_of(License, ckan:'License'),
  \+ license_blacklist(License).

license_blacklist(ckan:'License/6').
license_blacklist(ckan:'License/65').
license_blacklist(ckan:'License/None').
license_blacklist(ckan:'License/notspecified').


% Has an open license.
has_open_license(Resource):-
  once((
    rdf(Package, ckan:resources, Resource),
    rdf(Package, ckan:license_id, License, _),
    rdfs_individual_of(License, ckan:'OpenLicense')
  )).


% Has MIME string.
has_mime_string(Resource):-
  rdf_string(Resource, ckan:mimetype, MIME, _),
  MIME \== ''.


% Has MIME string that is registered with IANA.
has_iana_mime(Resource):-
  once((
    rdf_string(Resource, ckan:mimetype, MIME, _),
    atomic_list_concat([Type,Subtype], '/', MIME),
    rdfs_label(MIME, Subtype),
    rdf(MIME, rdf:type, MIME_Class, mime),
    rdfs_label(MIME_Class, Type)
  )).



%! has_error(+AP_StageName:atom, +Error:compound, +Resource:iri) is semidet.
% Succeeds if the given error term was thrown for the given resource
%  in an AP stage with the given name.

has_error(AP_StageName, Error, Resource):-
  rdf(AP, ap:resource, Resource),
  rdf_collection_member(ApStage, AP, _),
  rdf_string(ApStage, ap:name, AP_StageName, _),
  rdf_string(ApStage, ap:error, Atom, ap),
  read_term_from_atom(Atom, Error, []).

