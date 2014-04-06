:- module(
  rfc2616_entity_header,
  [
    'entity-header'//2 % -ParseTree:compound
                       % ?EntityHeader
  ]
).

/** <module> RFC 2616 entity header

DCG for entity header in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(http_headers(rfc2616_allow)).
:- use_remote_module(http_headers(rfc2616_content_length)).
:- use_remote_module(http_headers(rfc2616_content_type)).
:- use_remote_module(http_headers(rfc2616_message_header)).



%! 'entity-header'(-ParseTree:compound, ?EntityHeader)//
% # Syntax
%
% ~~~{.abnf}
% entity-header = Allow              ; Section 14.7
%               | Content-Encoding   ; Section 14.11
%               | Content-Language   ; Section 14.12
%               | Content-Length     ; Section 14.13
%               | Content-Location   ; Section 14.14
%               | Content-MD5        ; Section 14.15
%               | Content-Range      ; Section 14.16
%               | Content-Type       ; Section 14.17
%               | Expires            ; Section 14.21
%               | Last-Modified      ; Section 14.29
%               | extension-header
% ~~~
%
% # Semantics
%
% Entity-header fields define metainformation about the `entity-body` or,
%  if no body is present, about the resource identified by the request.
%
% # Pragmatics
%
% Some of this metainformation is OPTIONAL;
%  some might be REQUIRED by portions of this specification.

'entity-header'('entity-header'(T1), Allow) -->
  'Allow'(T1, Allow).
/*
'entity-header'('entity-header'(T1), ContentEncoding) -->
  'Content-Encoding'(T1, ContentEncoding).
'entity-header'('entity-header'(T1), ContentLanguage) -->
  'Content-Language'(T1, ContentLanguage).
*/
'entity-header'('entity-header'(T1), ContentLength) -->
  'Content-Length'(T1, ContentLength).
/*
'entity-header'('entity-header'(T1), ContentLocation) -->
  'Content-Location'(T1, ContentLocation).
'entity-header'('entity-header'(T1), ContentMD5) -->
  'Content-MD5'(T1, ContentMD5).
'entity-header'('entity-header'(T1), ContentRange) -->
  'Content-Range'(T1, ContentRange).
*/
'entity-header'('entity-header'(T1), ContentType) -->
  'Content-Type'(T1, ContentType).
/*
'entity-header'('entity-header'(T1), Expires) -->
  'Expires'(T1, Expires).
'entity-header'('entity-header'(T1), LastModified) -->
  'Last-Modified'(T1, LastModified).
*/
'entity-header'('entity-header'(T1), ExtensionHeader) -->
  'extension-header'(T1, ExtensionHeader).



%! 'extension-header'(-ParseTree:compound, ?ExtensionHeader)//
% # Syntax
%
% ~~~{.abnf}
% extension-header = message-header
% ~~~
%
% # Pragmatics
%
% The `extension-header` mechanism allows additional `entity-header` fields
%  to be defined without changing the protocol,
%  but these fields cannot be assumed to be recognizable by the recipient.
% Unrecognized header fields SHOULD be ignored by the recipient
%  and MUST be forwarded by transparent proxies.

'extension-header'('extension-header'(T1), ExtensionHeader) -->
  'message-header'(T1, ExtensionHeader).

