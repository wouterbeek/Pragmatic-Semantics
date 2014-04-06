:- module(
  rfc2616_request_uri,
  [
    'Request-URI'//2 % -ParseTree:compound
                     % ?URI:compound
  ]
).

/** <module> RFC 2616 request URI

DCG for RFC 2616 request URIs.

# Resource identification

The exact resource identified by an Internet request is determined by
 examining both the `Request-URI` and the `Host` header field.

An origin server that does not allow resources to differ by the
 requested host MAY ignore the `Host` header field value when
 determining the resource identified by an HTTP/1.1 request.
But see section 19.6.1.1 for other requirements on `Host` support in HTTP/1.1.

An origin server that does differentiate resources based on the host requested
 (sometimes referred to as virtual hosts or vanity host names)
 MUST use the following rules for determining the requested resource
 on an HTTP/1.1 request:
   1. If `Request-URI` is an `absoluteURI`,
       the host is part of the `Request-URI`.
      Any `Host` header field value in the request MUST be ignored.
   2. If the `Request-URI` is not an `absoluteURI`,
      and the request includes a `Host` header field,
      the host is determined by the `Host` header field value.
   3. If the host as determined by rule 1 or 2 is not a valid host
       on the server, the response MUST be a `400` (Bad Request)
       error message.

## Backward compatibility

Recipients of an HTTP/1.0 request that lacks a `Host` header field
 MAY attempt to use heuristics (e.g., examination of the URI path for
 something unique to a particular host) in order to determine
 what exact resource is being requested.

--

@author Wouter Beek
@see RFC 26160
@version 2013/12
*/

:- use_module(
  uri(rfc2396_dcg),
  [
    abs_path//2,
    absoluteURI//2,
    authority//2
  ]
).



%! 'Request-URI'(-ParseTree:compound, ?URI:compound)//
% The Request-URI is a Uniform Resource Identifier
%  and identifies the resource upon which to apply the request.
%
% # Syntax
%
% The `Request-URI` is transmitted in the format specified in RFC 2396.
%
% ~~~{.abnf}
% Request-URI = "*" | absoluteURI | abs_path | authority
% ~~~
%
% The four options for Request-URI are dependent on the nature of the request.
%
% # Semantics
%
% ## Asterisk
%
% The asterisk means that the request does not apply to a particular resource,
%  but to the server itself, and is only allowed when the method used
%  does not necessarily apply to a resource.
%
% One example would be
% ~~~{.http}
% OPTIONS * HTTP/1.1
% ~~~
%
% ## Absolute URI
%
% This form is REQUIRED when the request is being made to a proxy.
% The proxy is requested to forward the request or service it
%  from a valid cache, and return the response.
% Note that the proxy MAY forward the request on to another proxy
%  or directly to the server specified by the `absoluteURI`.
% In order to avoid request loops, a proxy MUST be able to recognize
%  all of its server names, including any aliases, local variations,
%  and the numeric IP address.
%
% Example:
% ~~~{.http}
% GET http://www.w3.org/pub/WWW/TheProject.html HTTP/1.1
% ~~~
%
% To allow for transition to the use of `absoluteURI` in all requests
%  in future versions of HTTP, all HTTP/1.1 servers MUST accept
%  the `absoluteURI` form in requests, even though HTTP/1.1 clients will only
%  generate them in requests to proxies.
%
% ## Absolute path
%
% The most common form of `Request-URI` is that used to identify a resource
%  on an origin server or gateway.
% In this case the absolute path of the URI MUST be transmitted
%  as the `Request-URI`, and the network location of the URI (`authority`)
%  MUST be transmitted in a `Host` header field.
%
% For example, a client wishing to retrieve the resource above directly
%  from the origin server would create a TCP connection to port `80`
%  of the host `www.w3.org` and send the lines:
% ~~~{.http}
% GET /pub/WWW/TheProject.html HTTP/1.1
% Host: www.w3.org
% ~~~
% followed by the remainder of the `Request`.
% Note that the absolute path cannot be empty;
%  if none is present in the original URI, it MUST be given as "/"
%  (the server root).
%
% ## Authority
%
% The authority form is only used by the `CONNECT` method.
%
% --
%
% # Pragmatics
%
% ## Decoding
%
% If the `Request-URI` is encoded using the `% HEX HEX` encoding,
%  the origin server MUST decode the `Request-URI` in order to properly
%  interpret the request.
%
% ## Invalid values
%
% Servers SHOULD respond to invalid `Request-URI`s with
%  an appropriate status code.
%
% ## Backwards compatibility
%
% Note: The "no rewrite" rule prevents the proxy from changing the meaning
%  of the request when the origin server is improperly using
%  a non-reserved URI character for a reserved purpose.
% Implementors should be aware that some pre-HTTP/1.1 proxies
%  have been known torewrite the `Request-URI`.
%
% --
%
% @arg ParseTree
% @arg URI A compound term uri/4 containing the following arguments:
%   1. `Scheme:atom`
%   2. `Authority:compound`
%   3. `Path:list(list(atom))`
%   4. `Query:atom`

'Request-URI'('Request-URI'('*'), uri(_Scheme,_Authority,_Path,_Query)) -->
  "*".
'Request-URI'('Request-URI'(T1), uri(Scheme,Authority,Path,Query)) -->
  absoluteURI(T1, uri(Scheme,Authority,Path,Query)).
'Request-URI'('Request-URI'(T1), uri(_Scheme,_Authority,Path,_Query)) -->
  abs_path(T1, Path).
'Request-URI'('Request-URI'(T1), uri(_Scheme,Authority,_Path,_Query)) -->
  authority(T1, Authority).

