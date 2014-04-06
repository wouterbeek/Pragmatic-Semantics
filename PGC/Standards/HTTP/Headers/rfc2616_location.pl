:- module(
  rfc2616_location,
  [
    'Location'//2 % -PraseTree:compound
                  % ?Location:compound
  ]
).

/** <module> RFC 2616 location

DCG for the `Location` response header in RFC 2616.

# Datatypes

## Location

~~~{.pl}
'Location'(
  URI:compound
)
~~~

## URI

~~~{.pl}
uri(
  Scheme:atom,
  Authority:or(atom,compound]),
  Path:list(list(atom)),
  Query:atom
)
~~~

## Authority

~~~{.pl}
authority(
  User:atom,
  Host:or([list(atom),list(integer)]),
  Port:integer
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_content)).
:- use_module(uri(rfc2396_dcg)).



%! 'Location'(-ParseTree:compound, ?Location:compound)//
% # Syntax
%
% The field value consists of a single absolute URI.
%
% ~~~{.abnf}
% Location = "Location" ":" absoluteURI
% ~~~
%
% # Semantics
%
% The `Location` response-header field is used to redirect the recipient to
%  a location other than the `Request-URI` for completion of the request
%  or identification of a new resource.
%
% ## Differences with `Content-Location`
%
% The `Content-Location` header field differs from `Location`
%  in that the `Content-Location` identifies the original location
%  of the entity enclosed in the request.
% It is therefore possible for a response to contain header fields
%  for both `Location` and `Content-Location`.
%
% ## Create
%
% For `201` (Created) responses, the `Location` is that of the new resource
%  which was created by the request.
%
% ## Redirect
%
% For `3xx` responses, the location SHOULD indicate the server's preferred URI
%  for automatic redirection to the resource.
%
% # Example
%
% ~~~{.http}
% Location: http://www.w3.org/pub/WWW/People.html
% ~~~
%
% @tbd RFC 2616 section 13.10 for cache requirements of some methods.

'Location'('Location'(Location), 'Location'(Location)) -->
  "Location:",
  % @tbd
  %absoluteURI(T1, Location).
  atom(Location).

