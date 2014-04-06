:- module(
  rfc2616_response_header,
  [
    'response-header'//2 % -ParseTree:compound
                         % ?ResponseHeader:compound
  ]
).

/** <module> RFC 2616 response header

DCG for response headers in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http_headers(rfc2616_accept_ranges)).
:- use_module(http_headers(rfc2616_age)).
:- use_module(http_headers(rfc2616_etag)).
:- use_module(http_headers(rfc2616_location)).
:- use_module(http_headers(rfc2616_proxy_authenticate)).



%! 'response-header'(-ParseTree:compound, ?Header)//
% The response-header fields allow the server to pass additional information
%  about the response which cannot be placed in the `Status-Line`.
%
% # Syntax
%
% ~~~{.abnf}
% response-header = Accept-Ranges        ; Section 14.5
%                 | Age                  ; Section 14.6
%                 | ETag                 ; Section 14.19
%                 | Location             ; Section 14.30
%                 | Proxy-Authenticate   ; Section 14.33
%                 | Retry-After          ; Section 14.37
%                 | Server               ; Section 14.38
%                 | Vary                 ; Section 14.44
%                 | WWW-Authenticate     ; Section 14.47
% ~~~
%
% # Semantics
%
% These header fields give information about the server
%  and about further access to the resource identified by the `Request-URI`.
%
% ## Unrecognized
%
% Unrecognized header fields are treated as entity-header fields.
%
% # Pragmatics
%
% Response-header field names can be extended reliably only
%  in combination with a change in the protocol version.
% However, new or experimental header fields MAY be given the semantics of
%  response-header fields if all parties in the communication
%  recognize them to be response-header fields.

'response-header'('response-header'(T1), AcceptRanges) -->
  'Accept-Ranges'(T1, AcceptRanges).
'response-header'('response-header'(T1), Age) -->
  'Age'(T1, Age).
'response-header'('response-header'(T1), ETag) -->
  'ETag'(T1, ETag).
'response-header'('response-header'(T1), Location) -->
  'Location'(T1, Location).
/*
'response-header'('response-header'(T1), Challenges) -->
  'Proxy-Authenticate'(T1, Challenges).
'response-header' -->
  'Retry-After'.
'response-header' -->
  'Server'.
'response-header' -->
  'Vary'.
'response-header' -->
  'WWW-Authenticate'.
*/

