:- module(
  rfc2616_accept,
  [
    'Accept'//2 % -ParseTree:compound
                % ?Accepts:list(compound)
  ]
).

/** <module> RFC 2616 accept

DCG for the `Accept` request header in RFC 2616.

# Terms

## `Accept`

~~~{.pl}
accept(
  MediaRange:compound,
  QualityValue:between(0.0,1.0),
  AcceptExtensions:list(pair(atom,atom))
)
~~~

## `MediaRange`

~~~{.pl}
media_range(
  Type:atom,
  Subtype:atom,
  Parameters:list(pair(atom,atom))
)
~~~

--

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(flp(rfc2616_abnf)).
:- use_remote_module(http(rfc2616_generic)).
:- use_remote_module(http_parameters(rfc2616_media_type)).
:- use_remote_module(http_parameters(rfc2616_quality_value)).



%! 'Accept'(-ParseTree:compound, ?Accepts:compound)// .
% # Syntax
%
% ~~~{.abnf}
% Accept = "Accept" ":"
%          #( media-range [ accept-params ] )
% ~~~
%
% # Semantics
%
% The `Accept` request-header field can be used to specify
%  certain media types which are acceptable for the response.
%
% `Accept` headers can be used to indicate that the request
%  is specifically limited to a small set of desired types,
%  as in the case of a request for an in-line image.
%
% ## Default value
%
% If no `Accept` header field is present,
%  then it is assumed that the client accepts all media types.
%
% ## Examples
%
% [1] SHOULD be interpreted as "I prefer `audio/basic`,
%  but send me any audio type if it is the best available after
%  an 80% mark-down in quality."
%
% ~~~{.http}
% [1]   Accept: audio/*; q=0.2, audio/basic
% ~~~
%
% [2] would be interpreted as `text/html` and `text/x-c`
%  are the preferred media types, but if they do not exist,
%  then send the `text/x-dvi` entity, and if that does not exist,
%  send the `text/plain` entity."
%
% ~~~{.http}
% [2]   Accept: text/plain; q=0.5, text/html,
%               text/x-dvi; q=0.8, text/x-c
% ~~~
%
% ## Precendence
%
% The media type quality factor associated with a given type is determined by
%  finding the media range with the highest precedence
%  which matches that type.
%
% ~~~{.http}
% Accept: text/*;q=0.3, text/html;q=0.7, text/html;level=1,
%         text/html;level=2;q=0.4, */*;q=0.5
% ~~~

% E.g. [1] would cause the following values to be associated:
% ~~~{.txt}
%   | `text/html;level=1  | 1   |
%   | `text/html`         | 0.7 |
%   | `text/plain`        | 0.3 |
%   | `image/jpeg`        | 0.5 |
%   | `text/html;level=2` | 0.4 |
%   | `text/html;level=3` | 0.7 |
% ~~~
%
% # Pragmatics
%
% ## Not acceptable
%
% If an `Accept` header field is present,
%  and if the server cannot send a response which is acceptable
%  according to the combined `Accept` field value,
%  then the server SHOULD send a `406` (not acceptable) response.
%
% ## Default configurable
%
% A user agent might be provided with a default set of quality values
%  for certain media ranges.
% However, unless the user agent is a closed system which cannot interact
%  with other rendering agents, this default set ought to be
%  configurable by the user.

'Accept'('Accept'(Ts), Accepts) -->
  "Accept:",
  abnf_list2('_Accept', _-_, Ts, Accepts).
'_Accept'(T0, accept(MediaRange,QualityValue, AcceptExtensions)) -->
  'media-range'(T1, MediaRange),
  (
    'accept-params'(T2, QualityValue, AcceptExtensions)
  ;
    "",
    {QualityValue = 1.0},
    {AcceptExtensions = []}
  ),
  {parse_tree('Accept', [T1,T2], T0)}.



%! 'accept-extension'(
%!   -ParseTree:compound,
%!   ?AcceptExtension:pair(atom,atom)
%! )// .
% ~~~{.abnf}
% accept-extension = ";" token [ "=" ( token | quoted-string ) ]
% ~~~

'accept-extension'(T0, AcceptExtension) -->
  ";",
  token(Name),
  (
    "=",
    (
      token(Value)
    ;
      'quoted-string'(Value)
    ),
    {T0 = 'accept-extension'(Name,Value)},
    {AcceptExtension = Name-Value}
  ;
    "",
    {T0 = 'accept-extension'(Name)},
    {AcceptExtension = Name-Name}
  ).



%! 'accept-params'(
%!   -ParseTree:compound,
%!   ?QualityValue:between(0.0,1.0),
%!   ?AcceptExtensions:list(pair(atom,atom))
%! )// .
% # Syntax
%
% Each `media-range` MAY be followed by one or more `accept-params`,
%  beginning with the `q` parameter for indicating a relative quality factor.
%
% ~~~{.abnf}
% accept-params = ";" "q" "=" qvalue *( accept-extension )
% ~~~
%
% ## Separator
%
% The first `q` parameter (if any) separates the `media-range` parameter(s)
%  from the `accept-params`.
%
% ## Note on `q`
%
% Use of the `q` parameter name to separate media type parameters
%  from `Accept` extension parameters is due to historical practice.
% Although this prevents any media type parameter named `q` from being used
%  with a media range, such an event is believed to be unlikely
%  given the lack of any `q` parameters in the IANA media type registry
%  and the rare usage of any media type parameters in `Accept`.
% Future media types are discouraged from registering any parameter named `q`.
%
% # Semantics
%
% Quality factors allow the user or user agent to indicate
%  the relative degree of preference for that `media-range`,
%  using the `qvalue` scale from `0` to `1`.
% The default value is `q=1`.

'accept-params'(T0, QualityValue, AcceptExtensions) -->
  ";q=",
  qvalue(T1, QualityValue),
  dcg_multi2('accept-extension', _-_, T2s, AcceptExtensions),
  {parse_tree('accept-params', [T1|T2s], T0)}.



%! 'media-range'(-ParseTree:compound, ?MediaRange:compound)// .
% # Syntax
%
% ~~~{.abnf}
% media-range = ( "*/*"
%               | ( type "/" "*" )
%               | ( type "/" subtype )
%               ) *( ";" parameter )
% ~~~
%
% ## Range grouping
%
% The asterisk character is used to group media types into ranges,
%  with `*/*` indicating all media types and `type/*` indicating all subtypes
%  of that type.
%
% ## Parameters
%
% The `media-range` MAY include media type parameters
%  that are applicable to that range.
%
% # Semantics
%
% ## Precedence
%
% Media ranges can be overridden by more specific media ranges
%  or specific media types.
% If more than one media range applies to a given type,
%  the most specific reference has precedence.
% E.g. [1]  has the following precedence:
%   1. `text/html;level=1`
%   2. `text/html`
%   3. `text/*`
%   4. `*/*`
%
% ~~~{.http}
% [1]   Accept: text/*, text/html, text/html;level=1, */*
% ~~~

'media-range'(T0, media_range('*','*',Parameters)) -->
  "*/*",
  dcg_multi2('_;_and_parameter', _-_, Ts, Parameters),
  {parse_tree('media-range', Ts, T0)}.
'media-range'(T0, media_range(Type,'*',Parameters)) -->
  rfc2616_media_type:type(T1, Type),
  "/*",
  dcg_multi2('_;_and_parameter', _-_, Ts, Parameters),
  {parse_tree('media-range', [T1|Ts], T0)}.
'media-range'(T0, media_range(Type,Subtype,Parameters)) -->
  rfc2616_media_type:type(T1, Type),
  "/",
  rfc2616_media_type:subtype(T2, Subtype),
  dcg_multi2('_;_and_parameter', _-_, Ts, Parameters),
  {parse_tree('media-range', [T1,T2|Ts], T0)}.
'_;_and_parameter'(T1, Parameter) -->
  ";",
  parameter(T1, Parameter).

