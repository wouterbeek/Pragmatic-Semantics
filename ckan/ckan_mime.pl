:- module(
  ckan_mime,
  [
    ckan_clean_mime/1, % +Graph:atom
    ckan_mime_table//0,
    ckan_mime_table_structured_open//0
  ]
).

/** <module> CKAN MIME

MIME cleaning for CKAN

# MIME types which we do not / cannot fix

Shapefile
api/sparql
application
application/download
application/esri
application/force-download
application/opensearchdescription+xml
application/rdf+json
application/vnd.ms-excel.12
application/x-file
application/x-unknown
application/xhtml+xml, text/plain, application/rdf+json,application/rdf+xml.text/rdf+n3
application/xls
application/xml-sitemap
kml
rd
tdyn/html
te
text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean
text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml,, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean
text/rdf
text/rdf+json
wsf/gml

# MIME types we can fix as either typos or deprecated MIME types

application/ html,text/html
application/application/csv,text/csv
application/bzip2,application/x-bzip2
application/csv,text/csv
application/json-ld,application/ld+json
application/octect-stream,application/octet-stream
application/owl+xml,application/rdf+xml
application/rar,application/x-rar-compressed
application/rdf xml,application/rdf+xml
application/rdf+n3,text/n3
application/rdf+turtle,text/turtle
application/rdf+xml; qs=0.9,application/rdf+xml
application/rdf\+xml,application/rdf+xml
application/sparql-results+json application/sparql-results+xml ,application/sparql-results+xml
application/turtle,text/turtle
application/vnd.ms-excel.sheet.binary.macroenabled.12,application/vnd.ms-excel.sheet.binary.macroEnabled.12
application/x-nquads,application/n-quads
application/x-ntriples,application/n-triples
application/x-pdf,application/pdf
application/x-turtle,text/turtle
application/x-zip,application/x-zip-compressed
csv,text/csv
example/rdf+xml,application/rdf+xml
html,text/html
rdf/xml,application/rdf+xml
xml/rdf,application/rdf+xml
text/comma-separated-values,text/csv
text/javascript,application/javascript
text/rdf+n3,text/n3
text/rss,application/rss+xml
text/sql,application/sql
text/tsv,text/tab-separated-values
text/x-csv,text/csv
text/x-sql,application/sql

# Either IANA-registered MIME types or de-facto standardized MIME types

application/atom+xml
application/gzip
application/json
application/marc
application/msword
application/octet-stream
application/pdf
application/rdf+xml
application/sparql-results+xml
application/vnd.google-earth.kml+xml
application/vnd.ms-excel
application/vnd.oasis.opendocument.spreadsheet
application/vnd.oasis.opendocument.text
application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
application/vnd.openxmlformats-officedocument.wordprocessingml.document
application/x-bibtex
application/x-bzip
application/x-bzip2
application/x-excel
application/x-gzip
application/x-javascript
application/x-netcdf
application/x-rar-compressed
application/x-tar
application/x-trig
application/x-zip-compressed
application/xhtml+xml
application/xml
application/zip
image/gif
image/jpeg
image/png
image/tiff
multipart/form-data
text/csv
text/html
text/n3
text/plain
text/tab-separated-values
text/turtle
text/vnd.graphviz
text/xml

@author Wouter Beek
@version 2014/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdfs)).

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(html(html_table)).
:- use_remote_module(http_parameters(rfc2616_media_type)).
:- use_remote_module(pl_web(html_pl_term)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(xml(xml_namespace)).

:- use_remote_module(prasem, ckan(ckan_db)).

:- meta_predicate(mime_table(//,+,?,?)).

:- xml_register_namespace(rfc2616, 'http://tools.ietf.org/html/rfc2616#').

http:location(ckan, root(ckan), []).
:- http_handler(ckan(mime), ckan_mime, []).

user:web_module('CKAN MIME', ckan_mime).



ckan_mime(_Request):-
  reply_html_page(
    app_style,
    title('CKAN MIME'),
    html([
      h1('CKAN MIME'),
      \ckan_mime_table
    ])
  ).

ckan_mime_table -->
  {
    aggregate_all(
      set(MIME),
      rdf_string(_, ckan:mimetype, MIME, _),
      MIMEs
    ),
    findall(
      NumberOfResources-MIME,
      (
        member(MIME, MIMEs),
        aggregate_all(
          set(Resource),
          rdf_string(Resource, ckan:mimetype, MIME, _),
          Resources
        ),
        length(Resources, NumberOfResources)
      ),
      Pairs1
    ),
    aggregate_all(
      set(ResourceWithoutMIME),
      (
        rdfs_individual_of(ResourceWithoutMIME, ckan:'Resource'),
        \+ rdf_string(ResourceWithoutMIME, ckan:mimetype, _, _)
      ),
      ResourcesWithoutMIME
    ),
    length(ResourcesWithoutMIME, NumberOfResourcesWithoutMIME),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    partition(mime_false, Pairs3, FalsePairs, Pairs4),
    partition(mime_true, Pairs4, TruePairs, ReplacementPairs),
    findall(
      [NumberOfResources,FalseMIME,TrueMIME],
      (
        member(NumberOfResources-FalseMIME, ReplacementPairs),
        mime(FalseMIME, TrueMIME)
      ),
      Rows
    )
  },
  html([
    p([
      'There are ',
      \html_pl_term(NumberOfResourcesWithoutMIME),
      ' resources with no MIME type.'
    ]),
    \mime_table(`Correct MIME types`, TruePairs),
    \html_table(
      [header_row(true),indexed(false)],
      html('MIME types we could fix'),
      [['Number of resources adhering','Incorrect MIME type','Replacing MIME type']|Rows]
    ),
    \mime_table(`MIME types we could not fix`, FalsePairs),
    \mime_content_type
  ]).


mime_content_type -->
  {
    findall(
      Resource-MIME1-MIME2,
      (
        rdfs_individual_of(Resource, ckan:'Resource'),
        rdf_string(Resource, ckan:mimetype, MIME1, _),
        rdf_string(Resource, rfc2616:'Content-Type', ContentType, _),
        dcg_phrase('media-type'(_, MediaType), ContentType),
        MediaType = media_type(Type, Subtype, _),
        atomic_list_concat([Type,Subtype], '/', MIME2)
      ),
      Tuples
    ),
    partition(same_mime, Tuples, SameTuples, DifferentTuples),
    length(SameTuples, L1),
    findall(
      [Resource,MIME1,MIME2],
      member(Resource-MIME1-MIME2, DifferentTuples),
      Rows
    )
  },
  html([
    p(['There are ',\html_pl_term(L1),' resources that have the same MIME type in their CKAN metadata and in their HTTP reply.']),
    \html_table(
      [header_row(true),indexed(false)],
      html('Resources with conflicting MIME types.'),
      [['Resource','CKAN MIME','HTTP MIME']|Rows]
    )
  ]).

same_mime(_-MIME-MIME).

mime_table(Caption, Pairs) -->
  {findall(
    [NumberOfResources,MIME],
    member(NumberOfResources-MIME, Pairs),
    Rows
  )},
  html(
    \html_table(
      [header_row(true),indexed(false)],
      Caption,
      [['Number of resources adhering','MIME type']|Rows]
    )
  ).


ckan_mime_table_structured_open -->
  {
    findall(
      NumberOfResources-[MIME,Structured,Open,Both],
      (
        mime(MIME, Structured, Open),
        boolean_and(Structured, Open, Both),
        findall(
          Resource,
          rdf_string(Resource, ckan:mimetype, MIME, _),
          Resources
        ),
        length(Resources, NumberOfResources)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [NumberOfResources|T],
      member(NumberOfResources-T, Pairs3),
      Rows
    ),
    findall(
      NoMIMEResource,
      (
        rdfs_individual_of(NoMIMEResource, ckan:'Resource'),
        \+ rdf_string(NoMIMEResource, ckan:mimetype, _, _)
      ),
      NoMIMEResources
    ),
    length(NoMIMEResources, NumberOfNoMIMEResources),
    findall(
      N1,
      member(N1-[_,true,_,_], Pairs3),
      N1s
    ),
    sum_list(N1s, N1),
    findall(
      N2,
      member(N2-[_,_,true,_], Pairs3),
      N2s
    ),
    sum_list(N2s, N2),
    findall(
      N3,
      member(N3-[_,_,_,true], Pairs3),
      N3s
    ),
    sum_list(N3s, N3)
  },
  html([
    \html_table(
      [header_row(true),indexed(false)],
      html('MIME types and their 2- and 3-star status.'),
      [
        ['NumberOfResources','MIME','Structured (2-star)','Open','Both (3-star)'],
        [NumberOfNoMIMEResources,'No MIME','false','false','false']
      |Rows]
    ),
    p(['There are ',\html_pl_term(N1),' resources with a structured MIME format (2-star).']),
    p(['There are ',\html_pl_term(N2),' resources with an open MIME format.']),
    p(['There are ',\html_pl_term(N3),' resources with a structured and open MIME format (3-stars).'])
  ]).

boolean_and(true, true, true ):- !.
boolean_and(_,    _,    false).

ckan_clean_mime(Graph):-
  forall(
    (
      rdf_string(Resource, ckan:mimetype, FalseMIME, Graph),
      mime(FalseMIME, TrueMIME),
      \+ member(TrueMIME, [false,true])
    ),
    (
      rdf_assert_string(Resource, ckan:mimetype, TrueMIME, Graph),
      rdf_retractall_string(Resource, ckan:mimetype, FalseMIME, Graph)
    )
  ).


mime_false(_-MIME):-
  mime(MIME).

mime_true(_-MIME):-
  mime(MIME, _, _).

% Typos we cannot convert.

mime('Shapefile').
mime('api/sparql').
mime('application').
mime('application/download').
mime('application/esri').
mime('application/force-download').
mime('application/opensearchdescription+xml').
mime('application/rdf+json').
mime('application/vnd.ms-excel.12').
mime('application/x-file').
mime('application/x-unknown').
mime('application/xhtml+xml, text/plain, application/rdf+json,application/rdf+xml.text/rdf+n3').
mime('application/xls').
mime('application/xml-sitemap').
mime('kml').
mime('rd').
mime('tdyn/html').
mime('te').
mime('text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean').
mime('text/html, application/XML, text/plain, application/sparql-results+json, application/sparql-results+xml, application/rdf+xml,, text/rdf+n3, application/x-turtle, application/x-trig, application/trix, text/boolean').
mime('text/rdf').
mime('text/rdf+json').
mime('wsf/gml').

% Typos we can convert.

mime('application/ html', 'text/html').
mime('application/application/csv', 'text/csv').
mime('application/bzip2', 'application/x-bzip2').
mime('application/csv', 'text/csv').
mime('application/json-ld', 'application/ld+json').
mime('application/octect-stream', 'application/octet-stream').
mime('application/owl+xml', 'application/rdf+xml').
mime('application/rar', 'application/x-rar-compressed').
mime('application/rdf xml', 'application/rdf+xml').
mime('application/rdf+n3', 'text/n3').
mime('application/rdf+turtle', 'text/turtle').
mime('application/rdf+xml; qs=0.9', 'application/rdf+xml').
mime('application/rdf\\+xml', 'application/rdf+xml').
mime('application/sparql-results+json application/sparql-results+xml ', 'application/sparql-results+xml').
mime('application/turtle', 'text/turtle').
mime('application/vnd.ms-excel.sheet.binary.macroenabled.12', 'application/vnd.ms-excel.sheet.binary.macroEnabled.12').
mime('application/x-nquads', 'application/n-quads').
mime('application/x-ntriples', 'application/n-triples').
mime('application/x-pdf', 'application/pdf').
mime('application/x-turtle', 'text/turtle').
mime('application/x-zip', 'application/x-zip-compressed').
mime('csv', 'text/csv').
mime('example/rdf+xml', 'application/rdf+xml').
mime('html', 'text/html').
mime('rdf/xml', 'application/rdf+xml').
mime('xml/rdf', 'application/rdf+xml').
mime('text/comma-separated-values', 'text/csv').
mime('text/javascript', 'application/javascript').
mime('text/rdf+n3', 'text/n3').
mime('text/rss', 'application/rss+xml').
mime('text/sql', 'application/sql').
mime('text/tsv', 'text/tab-separated-values').
mime('text/x-csv', 'text/csv').
mime('text/x-sql', 'application/sql').

%! mime(?MIME:atom, ?Structured:boolean, ?Open:boolean) is nondet.
% Real MIME content types.

mime('application/atom+xml',           true,  true ).
mime('application/gzip',               false, true ).
mime('application/json',               true,  true ).
mime('application/ld+json',            true,  true ).
mime('application/marc',               true,  true ).
mime('application/msword',             false, false).
mime('application/n-quads',            true,  true ).
mime('application/n-triples',          true,  true ).
mime('application/octet-stream',       false, false).
mime('application/pdf',                false, false).
mime('application/rdf+xml',            true,  true ).
mime('application/rss+xml',            true,  true ).
mime('application/sparql-results+xml', true,  true ).
mime('application/sql',                true,  true ).
mime('application/vnd.google-earth.kml+xml', true, true).
mime('application/vnd.ms-excel',       true,  false).
mime('application/vnd.ms-excel.sheet.binary.macroEnabled.12', true, false).
mime('application/vnd.oasis.opendocument.spreadsheet', false, true).
mime('application/vnd.oasis.opendocument.text', false, true).
mime('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', false, true).
mime('application/vnd.openxmlformats-officedocument.wordprocessingml.document', false, true).
mime('application/x-bibtex',           true,  true ).
mime('application/x-bzip',             false, true ).
mime('application/x-bzip2',            false, true ).
mime('application/x-excel',            true,  false).
mime('application/x-gzip',             false, true ).
mime('application/x-javascript',       true,  true ).
mime('application/x-netcdf',           true,  true ).
mime('application/x-rar-compressed',   false, false).
mime('application/x-tar',              false, true ).
mime('application/x-trig',             true,  true ).
mime('application/x-zip-compressed',   false, false).
mime('application/xhtml+xml',          true,  true ).
mime('application/xml',                true,  true ).
mime('application/zip',                false, false).
mime('image/gif',                      false, true ).
mime('image/jpeg',                     false, true ).
mime('image/png',                      false, true ).
mime('image/tiff',                     false, false).
mime('multipart/form-data',            false, false).
mime('text/csv',                       true,  true ).
mime('text/html',                      true,  true ).
mime('text/n3',                        true,  true ).
mime('text/plain',                     false, true ).
mime('text/tab-separated-values',      true,  true ).
mime('text/turtle',                    true,  true ).
mime('text/vnd.graphviz',              true,  true ).
mime('text/xml',                       true,  true ).

