:- module(
  rfc2616_proxy_authenticate,
  [
    'Proxy-Authenticate'//2 % -ParseTree:compound
                            % ?ProxyAuthenticate:compound
  ]
).

/** <module> RFC 2616 proxy authenticate

DCG for the `Proxy-Authenticate` response header in RFC 2616.

# Datatypes

## ProxyAuthenticate

~~~{.pl}
'Proxy-Authenticate'(
  Challenges:list
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(flp(rfc2616_abnf)).



%! 'Proxy-Authenticate'(-ParseTree:compound, ?ProxyAuthenticate:compound)//
% # Syntax
%
% ~~~{.abnf}
% Proxy-Authenticate = "Proxy-Authenticate" ":" 1#challenge
% ~~~
%
% # Semantics
%
% The field value consists of a challenge that indicates
%  the authentication scheme and parameters applicable to
%  the proxy for this `Request-URI`.
%
% # Pragmatics
%
% The `Proxy-Authenticate` response-header field MUST be included
%  as part of a `407` (Proxy Authentication Required) response.
%
% The HTTP access authentication process is described in RFC 2617.
% Unlike `WWW-Authenticate`, the `Proxy-Authenticate` header field applies
%  only to the current connection and SHOULD NOT be passed on to
%  downstream clients.
% However, an intermediate proxy might need to obtain its own credentials
%  by requesting them from the downstream client,
%  which in some circumstances will appear as if the proxy is forwarding
%  the `Proxy-Authenticate` header field.
%
% @tbd Implement RFC 2617.

'Proxy-Authenticate'(T0, 'Proxy-Authenticate'(Challenges)) -->
  "Proxy-Authenticate:",
  abnf_list2(challenge, 1-_, Ts, Challenges),
  {parse_tree('Proxy-Authenticate', Ts, T0)}.

% @tbd
challenge(_, _) --> `undefined`.

