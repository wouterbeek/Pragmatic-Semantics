:- module(
  ckan,
  [
    ckan/4 % +Options:list(nvpair)
           % +Action:atom
           % +Parameters:list(nvpair)
           % -Result:compound
  ]
).

/** <module> CKAN

@author Wouter Beek
@see http://docs.ckan.org/en/latest/api.html
@tbd The CKAN API uses `True` and `False` for boolean values.
@tbd The JSON `null` value is not replaced with a given default value.
@tbd Email addresses cannot all be parsed.
@tbd URLs cannot all be parsed.
@version 2013/11-2014/01, 2014/04
*/

:- use_module(library(http/http_header)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- use_remote_module(generics(uri_ext)).
:- use_remote_module(http(http_goal)).
:- use_remote_module(rdf_conv(json_to_rdf)).
:- use_remote_module(standards(json_ext)).

:- use_remote_module(prasem, ckan(ckan_db)).
:- use_remote_module(prasem, ckan(ckan_legend)). % Legend declarations.

:- meta_predicate(ckan_http(+,+,+,1)).



%! ckan(
%!   +Options:list(nvpair),
%!   +Action:atom,
%!   +Parameters:list(nvpair),
%!   -Result:compound
%! ) is det.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     An atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default: uninstantiated, using the server-side default.
%   * =|authority(+Authority:atom)|=
%     REQUIRED.
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.
%
% @arg Options A list of name-value pairs.
% @arg Action The atomic name of a CKAN action.
% @arg Parameters A list of name-value pairs.
% @arg Result A Prolog compound term.

ckan(O1, Action, Parameters, rdf):-
  option(graph(Graph), O1), !,
  ckan_http(O1, Action, Parameters, ckan_rdf(Graph)).
ckan(O1, Action, Parameters, Result):-
  ckan_http(O1, Action, Parameters, ckan_pl(Result)).


ckan_http(O1, Action, Parameters, Goal):-
  % URL
  option(scheme(Scheme), O1),
  option(authority(Authority), O1),
  option(api_version(Version), O1, _VAR),
  uri_path([api,Version,action,Action], Path),
  uri_components(URL, uri_components(Scheme, Authority, Path, _, _)),

  % API key
  (
    option(api_key(Key), O1)
  ->
    HttpO1 = [request_header('Authorization'=Key)]
  ;
    HttpO1 = []
  ),

  JSON_In = json(Parameters),
  append(
    [
      method(post),
      never_give_up(true),
      post(json(JSON_In)),
      request_header('Accept'='application/json')
    ],
    HttpO1,
    HttpO2
  ),
  http_goal(URL, HttpO2, Goal).


% To: Prolog
ckan_pl(Return, Stream):-
  ckan_stream_to_result(Stream, Result),
  json_to_prolog(ckan, Result, Return).


% To: RDF
ckan_rdf(Graph, Stream):-
  ckan_stream_to_result(Stream, Result),
  json_to_rdf(Graph, ckan_legend, ckan, Result, _).


ckan_stream_to_result(Stream, Result):-
  json_read(Stream, json(Reply)),
  memberchk(help=Help, Reply),
  (
    memberchk(error=Error, Reply)
  ->
    memberchk('__type'=Type, Error),
    memberchk(message=Message, Error),
    throw(error(Type, context(Help, Message)))
  ;
    memberchk(result=Result, Reply)
  ).

