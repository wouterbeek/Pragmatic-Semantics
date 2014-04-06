:- module(
  rfc2616_version,
  [
    'HTTP-Version'//2 % -Tree:compound
                      % ?Version:compound
  ]
).

/** <module> HTTP version

DCG rule for the HTTP version field.

# RFC 2616

## Syntax

HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
 of the protocol.
The protocol versioning policy is intended to allow the sender to indicate
 the format of a message and its capacity for understanding
 further HTTP communication,
 rather than the features obtained via that communication.
No change is made to the version number for the addition of
 message components which do not affect communication behavior or
 which only add to extensible field values.

The version of an HTTP message is indicated by an HTTP-Version field
 in the first line of the message.

~~~{.abnf}
HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
~~~

Note that the major and minor numbers MUST be treated as separate
 integers and that each MAY be incremented higher than a single digit.
Thus, HTTP/2.4 is a lower version than HTTP/2.13, which in turn is
 lower than HTTP/12.3.

Leading zeros MUST be ignored by recipients and MUST NOT be sent.

## Semantics

### Minor

The `<minor>` number is incremented when the changes made to
 the protocol add features which do not change the general message parsing
 algorithm, but which may add to the message semantics and imply
 additional capabilities of the sender.

### Major

The `<major>` number is incremented when the format of a message
 within the protocol is changed.

@see RFC 2145

## Pragmatics

An application that sends a request or response message that includes
 HTTP-Version of `HTTP/1.1` MUST be at least conditionally compliant
 with this specification.
Applications that are at least conditionally compliant with
 this specification SHOULD use an HTTP-Version of `HTTP/1.1`
 in their messages,
 and MUST do so for any message that is not compatible with HTTP/1.0.

The HTTP version of an application is the highest HTTP version for
 which the application is at least conditionally compliant.

Proxy and gateway applications need to be careful when forwarding
 messages in protocol versions different from that of the application.
Since the protocol version indicates the protocol capability of the sender,
 a proxy/gateway MUST NOT send a message with a version indicator which is
 greater than its actual version.
If a higher version request is received,
 the proxy/gateway MUST either downgrade the request version,
 or respond with an error, or switch to tunnel behavior.

@see RFC 2145

### Backwards compatibility

Due to interoperability problems with HTTP/1.0 proxies
 discovered since the publication of RFC 2068,
 caching proxies MUST, gateways MAY, and tunnels MUST NOT upgrade
 the request to the highest version they support.
The proxy/gateway's response to that request MUST be in
 the same major version as the request.

Note: Converting between versions of HTTP may involve modification of
 header fields required or forbidden by the versions involved.

--

@author Wouter Beek
@version 2013/07, 2013/12
*/

:- use_module(dcg(dcg_cardinal)).



%! 'HTTP-Version'(-ParseTree:compound, ?Version:compound)//
% HTTP uses a `<major>.<minor>` numbering scheme to indicate versions
% of the protocol.
% The version of an HTTP message is indicated by an HTTP-Version field
% in the first line of the message.
%
% ~~~{.abnf}
% HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
% ~~~
%
% @arg ParseTree
% @arg Version A compound term of the form
%        `version(Major:nonneg,Minor:nonneg)`

'HTTP-Version'(
  'HTTP-Version'(major(Major),minor(Minor)),
  version(Major,Minor)
) -->
  "HTTP/",
  decimal_number(Major),
  ".",
  decimal_number(Minor).

