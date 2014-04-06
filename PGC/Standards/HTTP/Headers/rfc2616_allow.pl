:- module(
  rfc2616_allow,
  [
    'Allow'//2 % -ParseTree:compound
               % ?Methods:list(oneof([connect,get,delete,head,options,post,put,trace]))
  ]
).

/** <module> RFC 2616 Allow header

Support for the `Allow` header in HTTP.

@author Wouter Beek
@version 2014/01
*/

:- use_module(http(rfc2616_method)).



%! 'Allow'(
%!   -ParseTree:compound,
%!   ?Methods:list(oneof([connect,get,delete,head,options,post,put,trace]))
%! )// .
% # Syntax
%
% ~~~{.abnf}
% Allow = "Allow" ":" #Method
% ~~~
%
% # Semantics
%
% The `Allow` entity-header field lists the set of methods supported by
%  the resource identified by the `Request-URI`.
%
% # Pragmatics
%
% The purpose of this field is strictly to inform the recipient of
%  valid methods associated with the resource.
%
% An `Allow` header field MUST be present in a `405` (Method Not Allowed)
%  response.
%
% This field cannot prevent a client from trying other methods.
% However, the indications given by the `Allow` header field value
%  SHOULD be followed.
% The actual set of allowed methods is defined by the origin server
%  at the time of each request.
%
% The `Allow` header field MAY be provided with a `PUT` request
%  to recommend the methods to be supported by the new or modified resource.
%  The server is not required to support these methods and SHOULD
%  include an `Allow` header in the response giving
%  the actual supported methods.
%
% A proxy MUST NOT modify the `Allow` header field even if it does not
%  understand all the methods specified, since the user agent might
%  have other means of communicating with the origin server
%
% # Example
%
% ~~~{.http}
% Allow: GET, HEAD, PUT
% ~~~

'Allow'('Allow'(Ts), 'Allow'(Methods)) -->
  "Allow:",
  methods1(Ts, Methods).

methods1([], []) --> [].
methods1([T1|Ts], [H|T]) -->
  'Method'(T1, method(H)),
  methods2(Ts, T).

methods2([], []) --> [].
methods2([T1|Ts], [H|T]) -->
  ",",
  'Method'(T1, method(H)),
  methods2(Ts, T).
