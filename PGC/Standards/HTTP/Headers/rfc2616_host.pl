:- module
  rfc2616_host,
  [
    'Host'//2 % -ParseTree:compound
              % ?Host:compound
  ]
).

/** <module> RFC 2616 host

DCG for `Host` request header of RFC 2616.

The `Host` request-header field MUST accompany all HTTP/1.1 requests.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

