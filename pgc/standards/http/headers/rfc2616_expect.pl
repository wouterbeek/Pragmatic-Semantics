:- module(
  rfc2616_expect,
  [
    'Expect'//2 % -ParseTree:compound
                % ?Expect:compound
  ]
).

/** <module> RFC 2616 expect

DCG for `Expect` request header in RFC 2616.

~~~{.pl}
'Expect'(
  Expectations:list(pair(compound,list(compound)))
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/



%! 'Expect'(-ParseTree:compound, ?Expect:compound)// .
% # Syntax
%
% ~~~{.abnf}
% Expect = "Expect" ":" 1#expectation
% ~~~
%
% ## Case-sensitivity
%
% Comparison of expectation values is case-insensitive for unquoted tokens
%  (including the `100-continue` token),
%  and is case-sensitive for quoted-string expectation-extensions.
%
% # Semantics
%
% The `Expect` request-header field is used to indicate
%  that particular server behaviors are required by the client.
%
% # Pragmatics
%
% ## Expectation failed
%
% A server that does not understand or is unable to comply
%  with any of the expectation values in the `Expect` field of a request
%  MUST respond with appropriate error status.
% The server MUST respond with a `417` (Expectation Failed) status
%  if any of the expectations cannot be met or,
%  if there are other problems with the request,
%  some other `4xx` status.
%
% ## Extensions
%
% This header field is defined with extensible syntax to allow
%  for future extensions.
% If a server receives a request containing an `Expect` field
%  that includes an expectation-extension that it does not support,
%  it MUST respond with a `417` (Expectation Failed) status.
%
% ## Forwarding
%
% The `Expect` mechanism is hop-by-hop:
%  that is, an HTTP/1.1 proxy MUST return a `417` (Expectation Failed) status
%  if it receives a request with an expectation that it cannot meet.
% However, the `Expect` request-header itself is end-to-end;
%  it MUST be forwarded if the request is forwarded.
%
% ## Backward compatibility
%
% Many older HTTP/1.0 and HTTP/1.1 applications do not understand
%  the `Expect` header.

'Expect'('Expect'(Ts), 'Expect'(Expectations)) -->
  "Expect:",
  abnf_list(expectation, 1-_, Ts, Expectations).

%! expectation(
%!  -ParseTree:compound,
%!  ?Expectation:pair(compound,list(compound))
%! )//

expectation(
  expectation('100-continue'),
  '100-continue'('100-continue')-[]
) -->
  "100-continue".
expectation(expectation(T1), ExpectationExtension-ExpectationParameters) -->
  'expectation-extension'(T1, ExpectationExtension, ExpectationParameters).

'expectation-extension'(T0, ExpectationExtension, ExpectationParameters) -->
  token(Name),
  (
    "=",
    (
      token(Value)
    ;
      'quoted-string'(Value)
    ),
    dcg_multi('expect-params', _-_, Ts, ExpectationParameters),
    {ExpectationExtension =.. [Name,Value]}
  ;
    "",
    {ExpectationExtension =.. [Name,Name]}
  ),
  {parse_tree('expectation-extension', [ExpectationExtension|Ts], T0)}.

'expect-params'(
  'expect-params'(ExpectationParameter),
  ExpectationParameter
) -->
  ";",
  token(Name),
  (
    "=",
    (
      token(Value)
    ;
      'quoted-string'(Value)
    ),
    {ExpectationParameter =.. [Name,Value]}
  ;
    "",
    {ExpectationParameter = Name}
  ).

