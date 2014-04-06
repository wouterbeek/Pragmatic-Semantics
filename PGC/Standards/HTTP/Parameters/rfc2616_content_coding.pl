:- module(
  rfc2616_content_coding,
  [
    'content-coding'//2 % -ParseTree:compound
                        % ?ContentCoding:oneof([compress,deflate,gzip,identity])
  ]
).

/** <module> RFC 2616 content coding

Content coding values for RFC 2616.

Content coding is primarily used for compression.
Content coding is a property of the original entity.

@author Wouter Beek
@version 2013/12
*/

:- use_module(http(rfc2616_generic)).



%! 'content-coding'(
%!   -ParseTree:compound,
%!   ?ContentCoding:oneof([compress,deflate,gzip,identity])
%! )//
%
% HTTP/1.1 uses content-coding values in the `Accept-Encoding`
%  and `Content-Encoding` header fields.
%
% # Syntax
%
% All content-coding values are case-insensitive.
%
% ~~~{.abnf}
% content-coding = token
% ~~~
%
% # Semantics
%
% Content coding values indicate an encoding transformation that has been
%  or can be applied to an entity.
%
% # Pragmatics
%
% Content codings are primarily used to allow a document to be compressed
%  or otherwise usefully transformed without losing the identity of
%  its underlying media type and without loss of information.
% Frequently, the entity is stored in coded form, transmitted directly,
%  and only decoded by the recipient.
%
% ## Decoding
%
% Although the value describes the content-coding,
%  what is more important is that it indicates what decoding mechanism
%  will be required to remove the encoding.
%
% ## Current values
%
% The Internet Assigned Numbers Authority (IANA) acts as a registry for
%  content-coding value tokens.
% Initially, the registry contains the following tokens:
%   * `gzip`
%     An encoding format produced by the file compression program "gzip"
%      (GNU zip) as described in RFC 1952.
%     This format is a Lempel-Ziv coding (LZ77) with a 32 bit CRC.
%   * `compress`
%     The encoding format produced by the common UNIX file compression
%      program "compress".
%     This format is an adaptive Lempel-Ziv-Welch coding (LZW).
%     Use of program names for the identification of encoding formats
%      is not desirable and is discouraged for future encodings.
%     Their use here is representative of historical practice,
%      not good design.
%     For compatibility with previous implementations of HTTP,
%      applications SHOULD consider `x-gzip` and `x-compress` to be equivalent
%      to `gzip` and `compress` respectively.
%   * `deflate`
%     The `zlib` format defined in RFC 1950 in combination with
%     the `deflate` compression mechanism described in RFC 1951.
%   * `identity`
%     The default encoding; the use of no transformation whatsoever.
%     This content-coding is used only in the `Accept-Encoding` header,
%      and SHOULD NOT be used in the `Content-Encoding` header.
%
% @see RFC 1950
% @see RFC 1951
% @see RFC 1952
%
% ## Future values
%
% New content-coding value tokens SHOULD be registered;
%  to allow interoperability between clients and servers,
%  specifications of the content coding algorithms needed to implement
%  a new value SHOULD be publicly available and adequate for
%  independent implementation, and conform to the purpose of content coding
%  defined in this section.
%
% --
%
% @see RFC 2616
% @tbd Content-coding values are case-insensitive.
% @tbd Value `identity` cannot occur in the `Content-Encoding` header.
% @tbd Check whether IANA has registered additional content encodings.

'content-coding'('content-coding'(T0), ContentCoding) -->
  % GENERATING
  % Only canonical values are allowed.
  (
    nonvar(ContentCoding)
  ->
    'content-coding_value'(ContentCoding, _)
  ;
    true
  ),
  
  token(ContentToken),
  
  % PARSING
  % There could be non-canonical content-coding values.
  % Translate these to their canonical value.
  (
    {var(ContentCoding)}
  ->
    'content-coding_value'(CanonicalValue, ContentTokens),
    memberchk(ContentToken, ContentTokens).
  ;
    ConnectionToken = ContentToken
  ).

'content-coding_value'(compress, [compress,'x-compress']).
'content-coding_value'(deflate, [defalte]).
'content-coding_value'(gzip, [gzip,'x-gzip']).
'content-coding_value'(identity, [identity]).

