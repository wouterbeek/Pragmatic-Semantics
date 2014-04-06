:- module(
  rfc2616_transfer_coding,
  [
    'transfer-coding'//3 % -ParseTree:compound
                         % ?TransferCoding:oneof([chunked,compress,deflate,gzip,identity])
                         % ?Parameters:list(kvpair(atom,atom))
  ]
).

/** <module> RFC 2616 transfer coding

Transfer coding values for RFC 2616.

Transfer coding is primarily used for safety.
Transfer coding is a property of the message.

All HTTP/1.1 applications MUST be able to receive and decode the
 "chunked" transfer-coding, and MUST ignore chunk-extension extensions
 they do not understand.

@author Wouter Beek
@see RFC 2616
@tbd Import `entity-header`//2.
@version 2013/12
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(parse_tree)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic)).
:- use_module(http_headers(rfc2616_entity_header)).
:- use_module(math(radix)).



%! chunk(
%!   -ParseTree:compound,
%!   ?ChunkExtensions:list(pair(atom,atom)),
%!   ?ChunkSize:nonneg,
%!   ?ChunkData:list(octet)
%! )//
% ~~~{.abnf}
% chunk = chunk-size [ chunk-extension ] CRLF chunk-data CRLF
% ~~~
%
% @tbd Notice that HTTP is a context-dependent language!
%      The chunk size has to be shared between `chunk-size` and `chunk-data`.

chunk(T0, ChunkExtensions, ChunkSize, ChunkData) -->
  'chunk-size'(T1, ChunkSize),
  (
    'chunk-extension'(T2, ChunkExtensions)
  ;
    "",
    ChunkExtensions = []
  ),
  'CRLF',
  'chunk-data'(T3, ChunkSize, ChunkData),
  'CRLF',
  {parse_tree(chunk, [T1,T2,T3], T0)}.



%! 'chunk-data'(
%!   -ParseTree:compound,
%!   ?ChunkSize:nonneg,
%!   ?ChunkData:list(octet)
%! )//
%
% ~~~{.abnf}
% chunk-data = chunk-size(OCTET)
% ~~~
%
% @tbd Notice that this is not an ABNF rule!

'chunk-data'('chunk-data'(ChunkSize,ChunkData), ChunkSize, ChunkData) -->
  dcg_multi('OCTET', ChunkSize-ChunkSize, ChunkData).



%! 'chunk-ext-name'(-ParseTree:compound, ?ChunkExtensionName:atom)//
% The name of a chunk extension.
%
% ~~~{.abnf}
% chunk-ext-name = token
% ~~~

'chunk-ext-name'('chunk-ext-name'(ChunkExtensionName), ChunkExtensionName) -->
  token(ChunkExtensionName).



%! 'chunk-ext-val'(-ParseTree:compound, ?ChunkExtensionValue:atom)//
% The value of a chunk extension.
%
% ~~~{.abnf}
% chunk-ext-val = token | quoted-string
% ~~~

'chunk-ext-val'('chunk-ext-val'(ChunkExtensionValue), ChunkExtensionValue) -->
  token(ChunkExtensionValue).
'chunk-ext-val'('chunk-ext-val'(ChunkExtensionValue), ChunkExtensionValue) -->
  'quoted-string'(ChunkExtensionValue).



%! 'chunk-extension'(
%!   -ParseTree:compound,
%!   ?ChunkExtensions:list(pair(atom,atom))
%! )//
% A chunk extension name-value pair.
%
% ~~~{.abnf}
% chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% ~~~

'chunk-extension'(T0, ChunkExtensions) -->
  dcg_multi('_chunk-extension', _-_, Ts, ChunkExtensions).
'_chunk-extension'(T0, Name-Value) -->
  ";",
  'chunk-ext-name'(T1, Name),
  (
    "=",
    'chunk-ext-val'(T2, Value)
  ;
    ""
  ),
  {parse_tree('chunk-extension', [T1,T2|Ts], T0)}.



%! 'chunk-size'(-ParseTree:compound, -ChunkSize:positive_integer)//
% The `chunk-size` field is a string of hexadecimal digits
%  indicating the size of the chunk.
%
% ~~~{.abnf}
% chunk-size = 1*HEX
% ~~~

'chunk-size'('chunk-size'(H), ChunkSize) -->
  dcg_multi('HEX', 1-_, Hs),
  {atomic_list_concat(Hs, H)},
  {digits_to_decimal(Hs, 16, ChunkSize)}.



%! 'Chunked-Body'(
%!   -ParseTree:compound,
%!   ?Chunks:list(compound),
%!   ?LastChunkExtensions:list(pair(atom,atom)),
%!   ?EntityHeaders:list
%! )//
% The chunked encoding modifies the body of a message in order to transfer it
%  as a series of chunks, each with its own size indicator,
%  followed by an OPTIONAL trailer containing entity-header fields.
%
% The chunked encoding is ended by any chunk whose size is zero,
%  followed by the trailer, which is terminated by an empty line.
%
% ~~~{.abnf}
% Chunked-Body = *chunk
%                last-chunk
%                trailer
%                CRLF
% ~~~
%
% This allows dynamically produced content to be transferred along with the
%  information necessary for the recipient to verify that it has received
%  the full message.
%
% @tbd Include the example process for decoding a `Chunked-Body`
%      in appendix 19.4.6.

'Chunked-Body'(
  'Chunked-Body'(T1,T2,T3),
  Chunks,
  LastChunkExtensions,
  EntityHeaders
) -->
  '_chunks'(T1, Chunks),
  'last-chunk'(T2, LastChunkExtensions),
  trailer(T3, EntityHeaders),
  'CRLF'.



%! 'last-chunk'(-ParseTree:compound, ?ChunkExtensions:list(pair(atom,atom)))//
% ~~~{.abnf}
% last-chunk     = 1*("0") [ chunk-extension ] CRLF
% ~~~

'last-chunk'(T0, ChunkExtension) -->
  dcg_multi1(zero, 1-_),
  (
    'chunk-extension'(T1, ChunkExtension)
  ;
    ""
  ),
  {parse_tree('last-chunk', [T1], T0)},
  'CRLF'.



%! trailer(-ParseTree:compound, ?EntityHeaders:list)//
% The trailer allows the sender to include additional HTTP header fields
%  at the end of the message.
% The `Trailer` header field can be used to indicate which header fields
%  are included in a trailer.
%
% ~~~{.abnf}
% trailer = *(entity-header CRLF)
% ~~~
%
% # Pragmatics
%
% A server using chunked transfer-coding in a response MUST NOT use
%  the trailer for any header fields unless at least one of the following
%  is true:
%   - the request included a `TE` header field that indicates `"trailers"`
%      is acceptable in the transfer-coding of the response,
%      as described in section 14.39
%   - the server is the origin server for the response,
%      the trailer fields consist entirely of optional metadata,
%      and the recipient could use the message
%      (in a manner acceptable to the origin server)
%      without receiving this metadata.
%     In other words, the origin server is willing to accept
%      the possibility that the trailer fields might be silently discarded
%      along the path to the client.
%
% This requirement prevents an interoperability failure when the message
%  is being received by an HTTP/1.1 (or later) proxy and forwarded to
%  an HTTP/1.0 recipient.
% It avoids a situation where compliance with the protocol would have
%  necessitated a possibly infinite buffer on the proxy.
%
% @tbd Document the type of the `EntityHeaders` parameter.

trailer(T0, EntityHeaders) -->
  dcg_multi2('_entity-header_and_CRLF', _-_, Ts, EntityHeaders),
  {parse_tree(trailer, Ts, T0)}.
'_entity-header_and_CRLF'(T0, EntityHeader) -->
  'entity-header'(T0, EntityHeader),
  'CRLF'.



%! 'transfer-coding'(
%!   -ParseTree:compound,
%!   ?TransferCoding:oneof([chunked,compress,deflate,gzip,identity]),
%!   ?TransferCoding:atom
%! )//
% Transfer-coding values are used to indicate an encoding transformation
%  that has been, can be, or may need to be applied to an entity-body
%  in order to ensure "safe transport" through the network.
% This differs from a content coding in that the transfer-coding is a
%  property of the message, not of the original entity.
%
% # Syntax
%
% ~~~{.abnf}
% transfer-coding = "chunked" | transfer-extension
% ~~~
%
% All transfer-coding values are case-insensitive.
%
% # Pragmatics
%
% HTTP/1.1 uses transfer-coding values in the `TE` header field
%  and in the `Transfer-Encoding` header field.
%
% ## Message length
%
% Whenever a transfer-coding is applied to a message-body,
%  the set of transfer-codings MUST include "chunked",
%  unless the message is terminated by closing the connection.
% When the "chunked" transfer-coding is used,
%  it MUST be the last transfer-coding applied to the message-body.
% The "chunked" transfer-coding MUST NOT be applied more than once
%  to a message-body.
% These rules allow the recipient to determine the transfer-length
%  of the message.
%
% ## Comparison to MIME `Content-Transfer-Encoding`
%
% Transfer-codings are analogous to the `Content-Transfer-Encoding` values
%  of MIME, which were designed to enable safe transport of binary data
%  over a 7-bit transport service.
% However, safe transport has a different focus for
%  an 8bit-clean transfer protocol.
% In HTTP, the only unsafe characteristic of message-bodies
%  is the difficulty in determining the exact body length,
%  or the desire to encrypt data over a shared transport.
%
% ## IANA
%
% The Internet Assigned Numbers Authority (IANA) acts as a registry for
%  transfer-coding value tokens.
% Initially, the registry contains the following tokens:
%   * `chunked`
%   * `compress`
%   * `deflate`
%   * `gzip`
%   * `identity`
%
% New transfer-coding value tokens SHOULD be registered in the same way
%  as new content-coding value tokens.
%
% ## Unimplemented transfer-codings
%
% A server which receives an entity-body with a transfer-coding
%  it does not understand SHOULD return 501 (Unimplemented),
%  and close the connection.
%
% ## Backward compatibility
%
% A server MUST NOT send transfer-codings to an HTTP/1.0 client.
%
% --
%
% @see RFC 2616

'transfer-coding'('transfer-coding'(chunked), chunked, []) -->
  "chunked".
'transfer-coding'('transfer-coding'(T0), TransferCoding, Params) -->
  'transfer-extension'(T0, TransferCoding, Params).



%! 'transfer-extension'(
%!   -ParseTree:compound,
%!   ?Token:atom,
%!   ?Parameters:list(pair(atom,atom))
%! )//
% ~~~{.abnf}
% transfer-extension = token *( ";" parameter )
% ~~~

'transfer-extension'(T0, Token, Parameters) -->
  token(Token),
  dcg_multi2('_;_and_parameter', _-_, Ts, Parameters),
  {parse_tree('transfer-extension', [Token|Ts], T0)}.
'_;_and_parameter'(T0, Parameter) -->
  ";",
  parameter(T0, Parameter).

