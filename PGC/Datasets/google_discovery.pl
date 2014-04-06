:- module(google_discovery, []).

/** <module> Google Discovery

Support for discovering Google APIs.

@author Wouter Beek
@see https://developers.google.com/discovery/
@version 2013/11
*/

:- use_module(generics(uri_ext)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).
:- use_module(library(ssl)).
:- use_module(library(uri)).


test(O1, Out):-
  option(api_key(_API_Key), O1, 'AIzaSyCyMEBhEScxCFep7Z760Y5Eb5dNnTvrbT8'),
  option(scheme(Scheme), O1, https),
  option(authority(Authority), O1, 'www.googleapis.com'),
  uri_path([discovery,v1,apis], Path),
  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  http_get(URI, json(Out), []).

