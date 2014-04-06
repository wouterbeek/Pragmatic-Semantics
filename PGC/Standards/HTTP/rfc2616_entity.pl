:- module(
  rfc2616_entity,
  [
    'entity-body'//2 % -ParseTree:compound
                     % ?EntityBody:list(octet)
  ]
).

/** <module> RFC 2616 entity

Request and Response messages MAY transfer an entity if not otherwise
 restricted by the request method or response status code.

An entity consists of entity-header fields and an entity-body,
 although some responses will only include the entity-headers.

Both sender and recipient refer to either the client or the server,
 depending on who sends and who receives the entity.

# Length

The entity-length of a message is the length of the message-body
 before any transfer-codings have been applied.

--

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_content)).
:- use_module(http(rfc2616_basic)).



%! 'entity-body'(-ParseTree:compound, ?EntityBody:list(octet))// .
% ~~~{.abnf}
% entity-body = *OCTET
% ~~~
%
% # Pragmatics
%
% When an `entity-body` is included with a message,
%  the data type of that body is determined via the header fields
%  `Content-Type` and `Content-Encoding`.
% These define a two-layer, ordered encoding model:
% ~~~{.txt}
% entity-body := Content-Encoding(Content-Type(data))
% ~~~
%
% ## Content type & encoding
%
% `Content-Type` specifies the media type of the underlying data.
% `Content-Encoding` may be used to indicate any additional content codings
%  applied to the data, usually for the purpose of data compression,
%  that are a property of the requested resource.
%
% ## Default encoding
%
% There is no default encoding.
%
% ## Absent media-type
%
% Any HTTP/1.1 message containing an entity-body SHOULD include
%  a `Content-Type` header field defining the media type of that body.
% If and only if the media type is not given by a `Content-Type` field,
%  the recipient MAY attempt to guess the media type via inspection of
%  its content and/or the name extension(s) of the URI used to identify
%  the resource.
%
% ## Default media-type
%
% If the media type remains unknown, the recipient SHOULD treat it as type
%  `application/octet-stream`.
%
% @tbd Feature between `Content-Type` header and `entity-body`.

'entity-body'('entity-body'(EntityBody), EntityBody) -->
  codes(EntityBody).

