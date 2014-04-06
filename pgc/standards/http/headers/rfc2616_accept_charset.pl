:- module(
  rfc2616_accept_charset,
  [
    'Accept-Charset'//2 % -ParseTree:compound
                        % ?Statements:list(pair(atom,between(0.0,1.0)))
  ]
).

/** <module> RFC 2616 accept charset

DCG for `Accept-Charset` request header of RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(flp(rfc2616_abnf)).
:- use_remote_module(http(rfc2616_generic)).
:- use_remote_module(http_parameters(rfc2616_quality_value)).



%! 'Accept-Charset'(
%!   -ParseTree:compound,
%!   ?Statements:list(pair(atom,between(0.0,1.0)))
%! )// .
% # Syntax
%
% ~~~{.abnf}
% Accept-Charset = "Accept-Charset" ":"
%         1#( ( charset | "*" )[ ";" "q" "=" qvalue ] )
% ~~~
%
% # Semantics
%
% The `Accept-Charset` request-header field can be used to indicate
%  what character sets are acceptable for the response.
%
% ## Header absence
%
% If no `Accept-Charset` header is present,
%  the default is that any character set is acceptable.
%
% ## Quality value
%
% Each charset MAY be given an associated quality value
%  which represents the user's preference for that charset.
%
% ### Default value
%
% The default value is `q=1`.
%
% ### Ascent values
%
% If no `*` is present in an `Accept-Charset` field,
%  then all character sets not explicitly mentioned get
%  a quality value of `0`, except for ISO-8859-1,
%  which gets a quality value of `1` if not explicitly mentioned.
%
% ### Asterisk value
%
% The special value `*`, if present in the `Accept-Charset` field,
%  matches every character set (including ISO-8859-1) which is not mentioned
%  elsewhere in the `Accept-Charset` field.
%
% # Pragmatics
%
% This field allows clients capable of understanding more comprehensive
%  or special-purpose character sets to signal that capability to a server
%  which is capable of representing documents in those character sets.
%
% ## Not acceptable
%
% If an `Accept-Charset` header is present,
%  and if the server cannot send a response which is acceptable
%  according to the `Accept-Charset` header,
%  then the server SHOULD send an error response
%  with the `406` (not acceptable) status code,
%  though the sending of an unacceptable response is also allowed.
%
% # Example
%
% ~~~{.http}
% Accept-Charset: iso-8859-5, unicode-1-1;q=0.8
% ~~~

'Accept-Charset'(T0, Statements) -->
  "Accept-Charset:",
  abnf_list2('_Accept-Charset', 1-_, Ts, Statements),
  {parse_tree('Accept-Charset', Ts, T0)}.
'_Accept-Charset'(T0, Charset-QualityValue) -->
  (
    charset(T1, Charset)
  ;
    "*",
    {T1 = '*'},
    {Charset = '*'}
  ),
  (
    ";q=",
    qvalue(T2, QualityValue)
  ;
    "",
    {QualityValue = 1.0}
  ),
  {parse_tree('Accept-Charset_stmt', [T1,T2], T0)}.

