:- module(lodobs_stats, []).

/** <module> LOD Statistics

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(ckan(ckan_ap)).
:- use_module(ckan(ckan_mime)).
:- use_module(dcg(dcg_generic)).
:- use_module(html(html_table)).
:- use_module(http(rfc2616_status_line)).
:- use_module(http_parameters(rfc2616_media_type)).
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

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

http:location(lodobs, root(lodobs), []).
:- http_handler(lodobs(stats), lodobs_stats, []).

user:web_module('LOD Stats', lodobs_stats).



lodobs_stats(_Request):-
  reply_html_page(
    app_style,
    title('LOD Statistics'),
    \lodobs_stats
  ).

lodobs_stats -->
  html([
    h2('Relative to all resources'),
    \number_of_resources(NumberOfResources),
    \number_of_resources_with_mime(NumberOfResources),
    \number_of_resources_with_format(NumberOfResources),
    \number_of_resources_with_mime_or_format(NumberOfResources),
    \number_of_sampled_resources(NumberOfResources, SampledResources)
  ]),
  {
    length(SampledResources, NumberOfSampledResources),
    partition(has_rfc_url, SampledResources, _, NoURLResources)
  },
  html([
    h2('Relative to the sampled resources.'),
    p([
      \number_of_resources(NoURLResources, NumberOfSampledResources),
      ' with no IANA-registered scheme.'
    ]),
    \connection(SampledResources, NumberOfSampledResources, _Include1),
    \http_status(SampledResources, NumberOfSampledResources, _Include2),
    \number_of_resources_with_content_type(SampledResources, NumberOfSampledResources),
    \number_of_conflicting_content_types(SampledResources, NumberOfSampledResources),

    % @tbd

    \number_of_no_license_resources(SampledResources, NumberOfSampledResources),
    \number_of_closed_license_resources(SampledResources, NumberOfSampledResources),
    \number_of_open_license_resources(SampledResources, NumberOfSampledResources, _Include3),
    \number_of_archives(SampledResources, NumberOfSampledResources, _Exclude)
  ]),
  {
    aggregate_all(
      set(X),
      (
        rdfs_individual_of(ApStage, ap:'AP-Stage'),
        ap_stage_resource(ApStage, X, _),
        rdf_string(ApStage, ap:name, 'FileSize', _),
        rdf_string(ApStage, ap:status, succeed, _)
      ),
      Xs
    )
  },
  html(
    p([
      \number_of_resources(Xs, NumberOfSampledResources),
      ' that were succesfully downloaded.'
    ])
  ),
  % Intersect.
  {
    aggregate_all(
      set(ApStage),
      (
        rdf_string(ApStage, ap:name, 'FileSize', _),
        rdf_string(ApStage, ap:status, succeed, _),
        ap_stage_resource(ApStage, Resource, _),
        rdf_string(Resource, rfc2616:'Content-Type', _, _),
        rdf(Package, ckan:resources, Resource),
        rdf(Package, ckan:license_id, License),
        rdfs_individual_of(License, ckan:'OpenLicense')
      ),
      Intersection
    ),
    length(Intersection, IntersectionSize)
  },
  html(
    p([
      'There are ',
      html_pl_term(IntersectionSize),
      ' resources in the intersection.'
    ])
  ),

  /*% Try it out.
  {
    aggregate_all(
      set(AP_Stage1),
      (
        rdf_string(AP_Stage1, ap:name, 'Filter', _),
        rdf_string(AP_Stage1, ap:status, succeed, _),
        ap_stage_resource(AP_Stage1, Resource, _),
        rdf_string(Resource, rfc2616:'Content-Type', _, _)
        %rdf(Package, ckan:resources, Resource),
        %rdf(Package, ckan:license_id, License),
        %rdfs_individual_of(License, ckan:'OpenLicense')
      ),
      AP_Stages1
    ),
    aggregate_all(
      set(AP_Stage2),
      (
        rdf_string(AP_Stage2, ap:name, 'VoID', _),
        rdf_string(AP_Stage2, ap:status, succeed, _)
      ),
      AP_Stages2
    ),
    maplist(length, [AP_Stages1,AP_Stages2], [L1,L2]),
    Perc is L2 / L1
  },
  html([
    p([
      'There are ',
      \html_pl_term(L1),
      ' filtered resources with LOD MIME type.'
    ]),
    p([
      \html_pl_term(L2),
      ' of these actually contain triples (',
      Perc,
      '%).'
    ])
  ]),*/
{true}.

http_status(Resources, M0, ResourcesOut) -->
  {
    has_http_error('Download', Resources, ResourcesOut, Pairs),
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
    h3('HTTP Access'),
    \html_table(
      [header_row(true),indexed(true)],
      html('HTTP Status'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering']
       |Rows]
    ),
    p([
      \number_of_resources(ResourcesOut, M0),
      ' for which the HTTP response is denotes succesful communication.'
    ])
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


connection(ResourcesIn0, M0, ResourcesOut6) -->
  {
    P1 = 'Cannot connect to resource\'s host',
    partition(host_found, ResourcesIn0, ResourcesIn1, ResourcesOut1),
    length(ResourcesOut1, N1),
    div_zero(N1, M0, M1),

    P2 = 'Host was found but connection was rejected',
    partition(connection_accepted, ResourcesIn1, ResourcesIn2, ResourcesOut2),
    length(ResourcesOut2, N2),
    div_zero(N2, M0, M2),

    P3 = 'Establishing connection to resource\'s host timed out',
    partition(establishing_connection_timed_out, ResourcesIn2, ResourcesIn3, ResourcesOut3),
    length(ResourcesOut3, N3),
    div_zero(N3, M0, M3),

    P4 = 'Permission error: caught in redirection loop',
    partition(permission_error_redirection_loop, ResourcesIn3, ResourcesIn4, ResourcesOut4),
    length(ResourcesOut4, N4),
    div_zero(N4, M0, M4),

    P5 = 'Existing connection timed out while reading data',
    partition(data_reading_connection_timed_out, ResourcesIn4, ResourcesIn5, ResourcesOut5),
    length(ResourcesOut5, N5),
    div_zero(N5, M0, M5),

    P6 = 'Try again?',
    partition(try_again, ResourcesIn5, ResourcesIn6, ResourcesOut6),
    length(ResourcesOut6, N6),
    div_zero(N6, M0, M6)
  },
  html([
    h3('Connection'),
    \html_table(
      [header_row(true),indexed(true)],
      html('Connection'),
      [['Property','Number of datasets adhering','Percentage of datasets adhering'],
       [P1,N1,M1],[P2,N2,M2],[P3,N3,M3],[P4,N4,M4],[P5,N5,M5],[P6,N6,M6]]
    ),
    p([
      \number_of_resources(ResourcesIn6, M0),
      ' that can be connected to.'
    ])
  ]).


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


%! has_error(+AP_StageName:atom, +Error:compound, +Resource:iri) is semidet.
% Succeeds if the given error term was thrown for the given resource
%  in an AP stage with the given name.

has_error(AP_StageName, Error, Resource):-
  rdf(AP, ap:resource, Resource),
  rdf_collection_member(ApStage, AP, _),
  rdf_string(ApStage, ap:name, AP_StageName, _),
  rdf_string(ApStage, ap:error, Atom, ap),
  read_term_from_atom(Atom, Error, []).


number_of_archives(Resources, NumberOfResources, ErrorArchivedResources) -->
  {
    aggregate_all(
      set(ArchivedResource),
      (
        member(ArchivedResource, Resources),
        ap_stage_resource(ApStage, ArchivedResource, _),
        rdf_string(ApStage, ap:name, 'Arch', _),
        once((
          rdfs_individual_of(ApStage, ap:'Error')
        ;
          rdf(ApStage, ap:has_modifier, _, _)
        ))
      ),
      ArchivedResources
    )
  },
  html(
    p([
      \number_of_resources(ArchivedResources, NumberOfResources),
      ' that are archived.'
    ])
  ),
  {
    aggregate_all(
      set(ErrorArchivedResource),
      (
        member(ErrorArchivedResource, ArchivedResources),
        ap_stage_resource(ApStage, ErrorArchivedResource, _),
        rdf_string(ApStage, ap:name, 'Arch', _),
        rdfs_individual_of(ApStage, ap:'Error')
      ),
      ErrorArchivedResources
    ),
    length(ArchivedResources, NumberOfArchivedResources)
  },
  html(
    p([
      \number_of_resources(ErrorArchivedResources, NumberOfArchivedResources),
      ' that are archived but cannot be unarchived.'
    ])
  ).


number_of_conflicting_content_types(SampledResources, NumberOfSampledResources) -->
  {
    aggregate_all
      set(ConflictingResource),
      (
        member(ConflictingResource, SampledResources),
        rdf_string(ConflictingResource, ckan:mimetype, MIME1, _),
        rdf_string(ConflictingResource, rfc2616:'Content-Type', ContentType, _),
        once(dcg_phrase('media-type'(_, MediaType), ContentType)),
        MediaType = media_type(Type, Subtype, _),
        atomic_list_concat([Type,Subtype], '/', MIME2),
        MIME1 == MIME2
      ),
      ConflictingResources
    )
  },
  html(
    p([
      \number_of_resources(ConflictingResources, NumberOfSampledResources),
      ' with conflicting ',
      tt('Content-Type'),
      ' response header.'
    ])
  ).


has_rfc_url(Resource):-
  once((
    rdf_string(Resource, ckan:url, URL, _),
    uri_components(URL, uri_components(Scheme, _, _, _, _)),
    uri_scheme(Scheme)
  )).


number_of_resources(NumberOfResources) -->
  {
    % The number of resources.
    aggregate_all(
      set(Resource),
      rdfs_individual_of(Resource, ckan:'Resource'),
      Resources
    ),
    length(Resources, NumberOfResources)
  },
  html(p(['There are ',\html_pl_term(NumberOfResources),' resources.'])).


number_of_resources_with_mime(NumberOfResources) -->
  {
    aggregate_all(
      set(ResourceWithMIME),
      rdf_string(ResourceWithMIME, ckan:mimetype, _, _),
      ResourcesWithMIME
    )
  },
  html(
    p([
      \number_of_resources(ResourcesWithMIME, NumberOfResources),
      ' with a value for the ',
      tt('ckan:mimetype'),
      ' property.'
    ])
  ).


number_of_resources_with_mime_or_format(NumberOfResources) -->
  {
    aggregate_all(
      set(X),
      ((
        rdf_string(X, ckan:mimetype, _, _)
      ;
        rdf_string(X, ckan:format, _, _)
      )),
      Xs
    )
  },
  html(
    p([
      \number_of_resources(Xs, NumberOfResources),
      ' with a value for the ',
      tt('ckan:mimetype'),
      ' or the ',
      tt('ckan:format'),
      ' property.'
    ])
  ).


number_of_resources_with_content_type(Resources, NumberOfResources) -->
  {
    aggregate_all(
      set(ResourceWithContentType),
      (
        member(ResourceWithContentType, Resources),
        rdf_string(ResourceWithContentType, rfc2616:'Content-Type', _, _)
      ),
      ResourcesWithContentType
    )
  },
  html(
    p([
      \number_of_resources(ResourcesWithContentType, NumberOfResources),
      ' with a value for the ',
      tt('Content-Type'),
      ' response header.'
    ])
  ).


number_of_resources_with_format(NumberOfResources) -->
  {
    aggregate_all(
      set(ResourceWithFormat),
      rdf_string(ResourceWithFormat, ckan:format, _, _),
      ResourcesWithFormat
    )
  },
  html(
    p([
      \number_of_resources(ResourcesWithFormat, NumberOfResources),
      ' with a value for the ',
      tt('ckan:format'),
      ' property.'
    ])
  ).


number_of_sampled_resources(NumberOfResources, SampledResources) -->
  {ckan_ap:take_lod_sample(datahub_io, SampledResources)},
  html(
    p([
      \number_of_resources(SampledResources, NumberOfResources),
      ' that could be mapped to a LOD format or MIME type.'
    ])
  ).


number_of_no_license_resources(SampledResources, NumberOfSampledResources) -->
  {
    aggregate_all(
      set(NoLicenseResource),
      (
        member(NoLicenseResource, SampledResources),
        rdf(Package, ckan:resources, NoLicenseResource),
        rdf(Package, ckan:license_id, License),
        rdfs_individual_of(License, ckan:'NoLicense')
      ),
      NoLicenseResources
    )
  },
  html(
    p([
      \number_of_resources(NoLicenseResources, NumberOfSampledResources),
      ' that have no license.'
    ])
  ).


number_of_closed_license_resources(SampledResources, NumberOfSampledResources) -->
  {
    aggregate_all(
      set(ClosedLicenseResource),
      (
        member(ClosedLicenseResource, SampledResources),
        rdf(Package, ckan:resources, ClosedLicenseResource),
        rdf(Package, ckan:license_id, License),
        rdfs_individual_of(License, ckan:'ClosedLicense')
      ),
      ClosedLicenseResources
    )
  },
  html(
    p([
      \number_of_resources(ClosedLicenseResources, NumberOfSampledResources),
      ' that have a closed license.'
    ])
  ).


number_of_open_license_resources(SampledResources, NumberOfSampledResources, OpenLicenseResources) -->
  {
    aggregate_all(
      set(OpenLicenseResource),
      (
        member(OpenLicenseResource, SampledResources),
        rdf(Package, ckan:resources, OpenLicenseResource),
        rdf(Package, ckan:license_id, License),
        rdfs_individual_of(License, ckan:'OpenLicense')
      ),
      OpenLicenseResources
    )
  },
  html(
    p([
      \number_of_resources(OpenLicenseResources, NumberOfSampledResources),
      ' that have an open license.'
    ])
  ).


number_of_resources(ResourcesX, NumberOfResources) -->
  {length(ResourcesX, NumberOfResourcesX)},
  html([
    'There are ',
    \html_pl_term(NumberOfResourcesX),
    ' resources (',
    \percentage(NumberOfResourcesX, NumberOfResources),
    ')'
  ]).


percentage(A, B) -->
  {Percentage is A / B * 100},
  html([
    \html_pl_term(Percentage),
    '%'
  ]).

