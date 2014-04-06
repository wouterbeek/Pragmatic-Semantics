:- module(google_books, []).

/** <module> Google Books

Support for the Google Books API v1.

@author Wouter Beek
@see https://developers.google.com/books/
@version 2013/11
*/

:- use_module(generics(uri_ext)).
:- use_module(library(http/http_client)).
:- use_module(library(option)).
:- use_module(library(uri)).



test(O1, Out):-
  option(api_key(API_Key), O1, 'AIzaSyCyMEBhEScxCFep7Z760Y5Eb5dNnTvrbT8'),
  option(scheme(Scheme), O1, https),
  option(authority(Authority), O1, 'googleapis.com'),
  uri_path([books,v1,volumes], Path),
  uri_query_components(
    Search,
    [key=API_Key,q='flowers+inauthor:keyes']
  ),
  uri_components(
    URL,
    uri_components(Scheme, Authority, Path, Search, _Fragment)
  ),
  http_get(URL, Out, []),

  google_authenticate.

google_authenticate:-
  google_authenticate([scope('https://www.googleapis.com/auth/books/feeds/')], _).

%! google_authenticate(+Options:list(nvpair), -Out) is semidet.
% This endpoint is the target of the initial request for an access token.
% It handles active session lookup, authenticating the user,
% and user consent.
% The result of requests of this endpoint include access tokens,
% refresh tokens, and authorization codes.
%
% ~~~
% https://accounts.google.com/o/oauth2/auth
% ~~~

google_authenticate(O1, Out):-
  % ## =client_id=
  %
  % Indicates the client that is making the request.
  % The value passed in this parameter must exactly match
  % the value shown in the Google Cloud Console.
  option(
    client_id(ClientId),
    O1,
    '205320353378-a663mdu4v9drpc7koiunkt52u2777udt.apps.googleusercontent.com'
  ),

  % ## =scope=
  %
  % Indicates the Google API access your application is requesting.
  % The values passed in this parameter inform the consent page shown
  % to the user.
  % There is an inverse relationship between the number of permissions
  % requested and the likelihood of obtaining user consent.
  option(scope(Scope), O1),

  % @tbd
  option(redirectURI(RedirectURI), O1, 'http://localhost:5000'),

  option(scheme(Scheme), O1, https),
  option(authority(Authority),O1, 'accounts.google.com'),
  uri_path([o,oauth2,auth], Path),
  uri_query_components(
    Search,
    [
      client_id=ClientId,
      redirect_uri=RedirectURI,
      response_type=code,
      scope=Scope
    ]
  ),
  uri_components(
    URL,
    uri_components(Scheme, Authority, Path, Search, _Fragment)
  ),
  http_get(URL, Out, []).

