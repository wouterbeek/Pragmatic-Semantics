:- module(
  rfc2616_retry_after,
  [
    'Retry-After'//2
  ]
).

/** <module> RFC 2616 retry after

DCG for the `Retry-After` response header in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/



%! 'Retry-After'(-ParseTree:compound)//
% # Syntax
%
% The value of this field can be either an `HTTP-date` or
%  an integer number of seconds (in decimal) after the time of the response.
%
% ~~~{.abnf}
% Retry-After = "Retry-After" ":" ( HTTP-date | delta-seconds )
% ~~~
%
% # Semantics
%
% ## Service unavailable
%
% The `Retry-After` response-header field can be used with
%  a `503` (Service Unavailable) response to indicate how long
%  the service is expected to be unavailable to the requesting client.
%
% ## Redirection
%
% This field MAY also be used with any `3xx` (Redirection) response
%  to indicate the minimum time the user-agent is asked [to] wait
%  before issuing the redirected request.
%
% # Examples
%
% ~~~{.http}
% [1]   Retry-After: Fri, 31 Dec 1999 23:59:59 GMT
% [2]   Retry-After: 120
% ~~~
% In [2] the delay is 2 minutes.

'Retry-After'('Retry-After'(T1), ) -->
  "Retry-After:",
  'HTTP-date'(T1).
'Retry-After'('Retry-After'(T1), ) -->
  "Retry-After:",
  'delta-seconds'(T1, Seconds).

