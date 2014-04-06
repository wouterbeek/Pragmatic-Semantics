:- module(
  rfc2616_method,
  [
    'Method'//2 % -ParseTree:compound
                % ?Method:compound
  ]
).

/** <module> RFC 2616 method

DCG for HTTP methods.

Although the set of methods can be expanded,
 additional methods cannot be assumed to share the same semantics
 for separately extended clients and servers.

# Safe methods

Implementors should be aware that the software represents the user
 in their interactions over the Internet,
 and should be careful to allow the user to be aware of
 any actions they might take which may have
 an unexpected significance to themselves or others.

In particular, the convention has been established that
 the `GET` and `HEAD` methods SHOULD NOT have the significance of
 taking an action other than retrieval.
These methods ought to be considered "safe".
This allows user agents to represent other methods,
 such as `POST`, `PUT` and `DELETE`, in a special way,
 so that the user is made aware of the fact that
 a possibly unsafe action is being requested.

Naturally, it is not possible to ensure that the server does not generate
 side-effects as a result of performing a `GET` request;
 in fact, some dynamic resources consider that a feature.
The important distinction here is that the user did not request
 the side-effects, so therefore cannot be held accountable for them.

# Idempotent Methods

Methods can also have the property of "idempotence" in that
 (aside from error or expiration issues)
 the side-effects of N > 0 identical requests is the same as for
 a single request.
The methods `GET`, `HEAD`, `PUT` and `DELETE` share this property.
Also, the methods `OPTIONS` and `TRACE` SHOULD NOT have side effects,
 and so are inherently idempotent.

However, it is possible that a sequence of several requests is non-idempotent,
 even if all of the methods executed in that sequence are idempotent.
(A sequence is idempotent if a single execution of the entire sequence
 always yields a result that is not changed by a reexecution of all,
 or part, of that sequence.)
For example, a sequence is non-idempotent if its result
 depends on a value that is later modified in the same sequence.

A sequence that never has side effects is idempotent, by definition
 (provided that no concurrent operations are being executed on
 the same set of resources).

--

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http(rfc2616_generic)).



%~ 'extension-method'(-ParseTree:compound, ?Method:atom)// .
% ~~~{.abnf}
% extension-method = token
% ~~~

'extension-method'('extension-method'(Method), Method) -->
  token(Method).



%! 'Method'(
%!   ?Method:oneof([connect,delete,get,head,options,post,put,send,trace])
%! )// .
%! 'Method'(-ParseTree:compound, ?Method:compound)// .
% The HTTP method.
%
% # Syntax
%
% The method is case-sensitive.
%
% ~~~{.abnf}
% Method = "OPTIONS"   ; Section 9.2
%        | "GET"       ; Section 9.3
%        | "HEAD"      ; Section 9.4
%        | "POST"      ; Section 9.5
%        | "PUT"       ; Section 9.6
%        | "DELETE"    ; Section 9.7
%        | "TRACE"     ; Section 9.8
%        | "CONNECT"   ; Section 9.9
%        | extension-method
% ~~~
%
% # Semantics
%
% The `Method` token indicates the method to be performed on the resource
%  identified by the `Request-URI`.
%
% # Pragmatics
%
% The list of methods allowed by a resource can be specified in
%  an `Allow` header field.
%
% The return code of the response always notifies the client
%  whether a method is currently allowed on a resource,
%  since the set of allowed methods can change dynamically.
%
% ## Method not allowed (`405`, `501`)
%
% An origin server SHOULD return the status code `405` (Method Not Allowed)
%  if the method is known by the origin server but not allowed
%  for the requested resource, and `501` (Not Implemented)
%  if the method is unrecognized or not implemented by the origin server.
%
% ## Required methods
%
% The methods `GET` and `HEAD` MUST be supported
%  by all general-purpose servers.
%
% ## Optional methods
%
% All other methods are OPTIONAL;
%  however, if the above methods are implemented,
%  they MUST be implemented with the same semantics
%  as those specified in section 9.

'Method'(options) -->
  options.
'Method'(get) -->
  "GET".
'Method'(head) -->
  "HEAD".
'Method'(post) -->
  "POST".
'Method'(put) -->
  "PUT".
'Method'(delete) -->
  "DELETE".
'Method'(trace) -->
  "TRACE".
'Method'(connect) -->
  "CONNECT".

'Method'(method(M), method(M)) -->
  'Method'(M).
'Method'(method(T1), method(M)) -->
  'extension-method'(T1, M).

%! options// .
% # Syntax
%
% ## Entity body
%
% If the `OPTIONS` request includes an entity-body
%  (as indicated by the presence of `Content-Length` or `Transfer-Encoding`),
%  then the media type MUST be indicated by a `Content-Type` field.
% Although this specification does not define any use for such a body,
%  future extensions to HTTP might use the `OPTIONS` body
%  to make more detailed queries on the server.
% A server that does not support such an extension MAY discard
%  the request body.
%
% If no response body is included,
%  the response MUST include a `Content-Length` field
%  with a field-value of `0`.
%
% # Semantics
%
% The `OPTIONS` method represents a request for information about
%  the communication options available on the request/response chain
%  identified by the `Request-URI`.
%
% ## `Request-URI`
%
% If the `Request-URI` is an asterisk,
%  the `OPTIONS` request is intended to apply to the server in general
%  rather than to a specific resource.
% Since a server's communication options typically depend on the resource,
%  the `*` request is only useful as a "ping" or "no-op" type of method;
%  it does nothing beyond allowing the client to test the capabilities of
%  the server.
% For example, this can be used to test a proxy for HTTP/1.1 compliance
%  (or lack thereof).
%
% If the `Request-URI` is not an asterisk,
%  the `OPTIONS` request applies only to the options that are available
%  when communicating with that resource.
%
% ## Entity body
%
% The response body, if any, SHOULD also include information about
%  the communication options.
% The format for such a body is not defined by this specification,
%  but might be defined by future extensions to HTTP.
%
% ## Forwarding
%
% The `Max-Forwards` request-header field MAY be used to target
%  a specific proxy in the request chain.
% When a proxy receives an `OPTIONS` request on an `absoluteURI`
%  for which request forwarding is permitted,
%  the proxy MUST check for a `Max-Forwards` field.
% If the `Max-Forwards` field-value is zero,
%  the proxy MUST NOT forward the message;
%  instead, the proxy SHOULD respond with its own communication options.
% If the `Max-Forwards` field-value is an integer greater than zero,
%  the proxy MUST decrement the field-value when it forwards the request.
% If no `Max-Forwards` field is present in the request,
%  then the forwarded request MUST NOT include a `Max-Forwards` field.
%
% # Pragmatics
%
% This method allows the client to determine the options and/or requirements
%  associated with a resource, or the capabilities of a server,
% without implying a resource action or initiating a resource retrieval.
%
% ## Caching
%
% Responses to this method are not cacheable.
%
% ## Entity body
%
% Content negotiation MAY be used to select the appropriate response format.

options -->
  "OPTIONS".

