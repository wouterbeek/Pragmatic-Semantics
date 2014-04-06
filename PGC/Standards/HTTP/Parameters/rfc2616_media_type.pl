:- module(
  rfc2616_media_type,
  [
    'media-type'//2 % -ParseTree:compound
                    % ?MediaType:compound
  ]
).

/** <module> RFC 2616 media types

@author Wouter Beek
@see RFC 2616
@version 2013/12, 2014/02
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(http(rfc2616_generic)).



%! 'media-type'(-ParseTree:compound, ?MediaType:compound)// .
% HTTP uses Internet Media Types in the `Content-Type` and `Accept` header
%  fields in order to provide open and extensible data typing and
%  type negotiation.
%
% # Syntax
%
% ~~~{.abnf}
% media-type = type "/" subtype *( ";" parameter )
% ~~~
%
% Parameters MAY follow the type/subtype in the form of attribute/value pairs.
%
% ## Linear white space
%
% Parameter values might or might not be case-sensitive,
%  depending on the semantics of the parameter name.
% `LWS` MUST NOT be used between an attribute and its value.
%
% # Semantics
%
% The presence or absence of a parameter might be significant to
%  the processing of a media-type, depending on its definition
%  within the media type registry.
%
% ## Multipart types
%
% The MIME header fields within each body-part of a multipart message-body
%  do not have any significance to HTTP beyond that defined by their
%  MIME semantics.
%
% # Pragmatics
%
% ## Backwards compatibility
%
% Note that some older HTTP applications do not recognize media type
%  parameters.
% When sending data to older HTTP applications, implementations SHOULD only
%  use media type parameters when they are required by that type/subtype
%  definition.
%
% ## IANA
%
% Media-type values are registered with the IANA.
% The media type registration process is outlined in RFC 1590.
% Use of non-registered media types is discouraged.
%
% ## Canonicalization
%
% Internet media types are registered with a canonical form.
% An entity-body transferred via HTTP messages MUST be represented in
%  the appropriate canonical form prior to its transmission except for
%  "text" types, as defined in the next paragraph.
%
% ## Alternative line breaks
%
% When in canonical form, media subtypes of the "text" type use `CRLF`
%  as the text line break.
% HTTP relaxes this requirement and allows the transport of text media
%  with plain `CR` or `LF` alone representing a line break when it is done
%  consistently for an entire entity-body.
% HTTP applications MUST accept `CRLF`, bare `CR`, and bare `LF`
%  as being representative of a line break in text media received via HTTP.
% In addition, if the text is represented in a character set that does not
%  use octets `13` and `10` for `CR` and `LF` respectively,
%  as is the case for some multi-byte character sets,
%  HTTP allows the use of whatever octet sequences are defined
%  by that character set to represent the equivalent of `CR` and `LF`
%  for line breaks.
% This flexibility regarding line breaks applies only to text media
%  in the entity-body; a bare `CR` or `LF` MUST NOT be substituted for
%  `CRLF` within any of the HTTP control structures
%  (such as header fields and multipart boundaries).
%
% ## Content coding
%
% If an entity-body is encoded with a content-coding, the underlying
%  data MUST be in a form defined above prior to being encoded.
%
% ## Character set
%
% The "charset" parameter is used with some media types to define
%  the character set of the data.
% When no explicit charset parameter is provided by the sender,
%  media subtypes of the "text" type are defined to have
%  a default charset value of `ISO-8859-1` when received via HTTP.
% Data in character sets other than `ISO-8859-1` or its subsets
%  MUST be labeled with an appropriate charset value.
%
% ## Multipart types
%
% MIME provides for a number of "multipart" types
%  -- encapsulations of one or more entities within a single message-body.
% All multipart types share a common syntax (section 5.1.1 of RFC 2046),
%  and MUST include a boundary parameter as part of the media type value.
% The message body is itself a protocol element and MUST therefore use
%  only `CRLF` to represent line breaks between body-parts.
% Unlike in RFC 2046, the epilogue of any multipart message MUST be empty;
% HTTP applications MUST NOT transmit the epilogue
%  (even if the original multipart contains an epilogue).
% These restrictions exist in order to preserve the self-delimiting nature of
%  a multipart message-body, wherein the "end" of the message-body
%  is indicated by the ending multipart boundary.
%
% In general, HTTP treats a multipart message-body no differently than
%  any other media type: strictly as payload.
% The one exception is the `multipart/byteranges` type
%  when it appears in a 206 (`Partial Content`) response,
%  which will be interpreted by some HTTP caching mechanisms
%  (sections 13.5.4 and 14.16).
% In all other cases, an HTTP user agent SHOULD follow the same or similar
%  behavior as a MIME user agent would upon receipt of a multipart type.
%
% In general, an HTTP user agent SHOULD follow the same or similar behavior
%  as a MIME user agent would upon receipt of a multipart type.
% If an application receives an unrecognized multipart subtype,
%  the application MUST treat it as being equivalent to `multipart/mixed`.
%
% Note: The `multipart/form-data` type has been specifically defined for
%  carrying form data suitable for processing via the `POST` request method
%  (RFC 1867).
%
% @arg ParseTree
% @arg MediaType A compound term of the form
%        `media_type(Type:atom,Subtyp:atom,Params:list(pair(atom,atom)))`
%
% @see RFC 1590
% @see RFC 1867
% @see RFC 2046

'media-type'(T0, media_type(Type,Subtype,Params)) -->
  type(T1, Type),
  forward_slash,
  subtype(T2, Subtype),
  parameters(Ts, Params),
  {parse_tree('media-type', [T1,T2|Ts], T0)}.

parameters([T1|Ts], [H|T]) -->
  semi_colon, blanks,
  parameter(T1, H),
  parameters(Ts, T).
parameters([], []) --> [].

%! subtype(-ParseTree:compound, Subtype:atom)// .
%! type(-ParseTree:compound, Type:atom)// .
%
% # Syntax
%
% The type, subtype, and parameter attribute names are case-insensitive.
%
% ~~~{.abnf}
% type    = token
% subtype = token
% ~~~
%
% `LWS` MUST NOT be used between the type and subtype.

type(type(Type), Type) -->
  token(Type).
subtype(subtype(Subtype), Subtype) -->
  token(Subtype).

