:- module(
  rfc2616_generic_message,
  [
    'generic-message'//3, % -ParseTree:compound
                          % ?MessageHeaders:list
                          % ?MessageBody:list(octet)
    'message-body'//2 % -ParseTree:compound
                      % ?MessageBody:list(octet)
  ]
).

/** <module> RFC 2616 generic message

DCG for generic messages (comprising request and response messages)
 in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(http(rfc2616_basic)).
:- use_remote_module(http(rfc2616_entity)).
:- use_remote_module(http(rfc2616_request)).
:- use_remote_module(http(rfc2616_response)).
:- use_remote_module(http(rfc2616_status_line)).
:- use_remote_module(http_headers(rfc2616_message_header)).



%! 'generic-message'(
%!   -ParseTree:compound,
%!   ?MessageHeaders:list(pair(atom,atom)),
%!   ?Body:list(octec)
%! )//
% `Request` and `Response` messages use the generic message format
%  of RFC 822 for transferring entities (the payload of the message).
% Both types of message consist of
%  - a start-line,
%  - zero or more header fields (also known as "headers"),
%  - an empty line (i.e., a line with nothing preceding the `CRLF`)
%    indicating the end of the header fields, and
%  - possibly a message-body.

% ~~~{.abnf}
% generic-message = start-line
%                   *(message-header CRLF)
%                   CRLF
%                   [ message-body ]
% ~~~

'generic-message'(T0, MessageHeaders, MessageBody) -->
  'start-line'(T1),
  dcg_multi2('_generic-message', T2s, MessageHeaders),
  'CRLF',
  (
    'message-body'(T3, MessageBody)
  ;
    "",
    {MessageBody = []}
  ),
  {parse_tree(generic_message, [T1,headers(T2s),T3], T0)}.
'_generic-message'(T1, Header) -->
  'message-header'(T1, Header),
  'CRLF'.



%! 'message-body'(-ParseTree:compound, ?MessageBody:list(octet))//
% The message-body (if any) of an HTTP message is used to carry
%  the entity-body associated with the request or response.
%
% The message-body differs from the entity-body only when
%  a transfer-coding has been applied,
%  as indicated by the `Transfer-Encoding` header field.
%
% # Syntax
%
% ~~~{.abnf}
% message-body = entity-body | <entity-body encoded as per Transfer-Encoding>
% ~~~
%
% # Pragmatics
%
% `Transfer-Encoding` MUST be used to indicate any transfer-codings applied by
%  an application to ensure safe and proper transfer of the message.
% `Transfer-Encoding` is a property of the message, not of the entity,
%  and thus MAY be added or removed by any application
%  along the request/response chain.
% However, section 3.6 places restrictions on when
%  certain transfer-codings may be used.
%
% The rules for when a message-body is allowed in a message differ
%  for requests and responses.
%
% ## Request
%
% The presence of a message-body in a request is signaled by
%  the inclusion of a `Content-Length` or `Transfer-Encoding` header field
%  in the request's message-headers.
% A message-body MUST NOT be included in a request if
%  the specification of the request method does not allow
%  sending an entity-body in requests.
% A server SHOULD read and forward a message-body on any request;
%  if the request method does not include defined semantics
%  for an entity-body, then the message-body SHOULD be ignored
%  when handling the request.
%
% ## Response
%
% For response messages, whether or not a message-body is included
%  with a message is dependent on both the request method
%  and the response status code.
% All responses to the HEAD request method MUST NOT include a message-body,
%  even though the presence of entity-header fields might lead one to believe
%  they do.
% All `1xx` (informational), `204` (no content), and `304` (not modified)
%  responses MUST NOT include a message-body.
% All other responses do include a message-body,
%  although it MAY be of zero length.
%
% ## Message length
%
% The transfer-length of a message is the length of the message-body as
%  it appears in the message; that is, after any transfer-codings
%  have been applied.
% When a message-body is included with a message,
%  the transfer-length of that body is determined by one of the following
%  (in order of precedence):
%    1. Any response message which "MUST NOT" include a message-body (such
%       as the `1xx`, `204`, and `304` responses and any response to a `HEAD`
%       request) is always terminated by the first empty line after the
%       header fields, regardless of the entity-header fields present in
%       the message.
%    2. If a `Transfer-Encoding` header field (section 14.41) is present and
%       has any value other than "identity", then the transfer-length is
%       defined by use of the "chunked" transfer-coding (section 3.6),
%       unless the message is terminated by closing the connection.
%    3. If a `Content-Length` header field (section 14.13) is present, its
%       decimal value in OCTETs represents both the entity-length and the
%       transfer-length. The `Content-Length` header field MUST NOT be sent
%       if these two lengths are different (i.e., if a `Transfer-Encoding`
%       header field is present). If a message is received with both a
%       `Transfer-Encoding` header field and a `Content-Length` header field,
%       the latter MUST be ignored.
%    4. If the message uses the media type `multipart/byteranges`, and the
%       transfer-length is not otherwise specified, then this self-
%       elimiting media type defines the transfer-length. This media type
%       MUST NOT be used unless the sender knows that the recipient can parse
%       it; the presence in a request of a `Range` header with multiple byte-
%       range specifiers from a 1.1 client implies that the client can parse
%       `multipart/byteranges` responses.
%       A range header might be forwarded by a 1.0 proxy that does not
%       understand `multipart/byteranges`; in this case the server MUST
%       delimit the message using methods defined in items 1,3 or 5 of
%       this section.
%    5. By the server closing the connection. (Closing the connection
%       cannot be used to indicate the end of a request body, since that
%       would leave no possibility for the server to send back a response.)
%
% For compatibility with HTTP/1.0 applications, HTTP/1.1 requests
%  containing a message-body MUST include
%  a valid `Content-Length` header field
%  unless the server is known to be HTTP/1.1 compliant.
% If a request contains a message-body and a `Content-Length` is not given,
%  the server SHOULD respond with `400` (bad request)
%  if it cannot determine the length of the message,
%  or with `411` (length required) if it wishes to insist on
%  receiving a valid `Content-Length`.
%
% All HTTP/1.1 applications that receive entities MUST accept the
%  "chunked" transfer-coding (section 3.6), thus allowing this mechanism
%  to be used for messages when the message length cannot be determined
%  in advance.
%
% Messages MUST NOT include both a `Content-Length` header field and
%  a non-identity transfer-coding.
% If the message does include a non-identity transfer-coding,
%  the `Content-Length` MUST be ignored.
%
% When a `Content-Length` is given in a message where a message-body is
%  allowed, its field value MUST exactly match the number of `OCTET`s
%  in the message-body.
% HTTP/1.1 user agents MUST notify the user when an invalid length
% is received and detected.
%
% @tbd How to implement the encoded case?

'message-body'(T1, Body) -->
  'entity-body'(T1, Body).



%! 'start-line'(-ParseTree:compound)//
% In the interest of robustness, servers SHOULD ignore any empty line(s)
%  received where a `Request-Line` is expected.
% In other words, if the server is reading the protocol stream
%  at the beginning of a message and receives a `CRLF` first,
%  it should ignore the `CRLF`.
%
% Certain buggy HTTP/1.0 client implementations generate extra `CRLF`'s
%  after a `POST` request.
% To restate what is explicitly forbidden by the BNF,
%  an HTTP/1.1 client MUST NOT preface or follow a request
%  with an extra `CRLF`.

'start-line'('start-line'(T1)) -->
  rfc2616_request:'Request-Line'(T1, _Method, _RequestURI, _Version).
'start-line'('start-line'(T1)) -->
  'Status-Line'(T1, _Version, _Status).

