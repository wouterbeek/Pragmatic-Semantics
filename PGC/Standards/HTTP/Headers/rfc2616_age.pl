:- module(
  rfc2616_age,
  [
    'Age'//2 % -ParseTree:compound
             % ?Age:compound
  ]
).

/** <module> RFC 2616 Age

DCG for the `Age` header of RFC 2616.

# Datatypes

## Age

~~~{.pl}
'Age'(
  Seconds:nonneg
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(http_parameters(rfc2616_date)).



%! 'Age'(-ParseTree:compound, ?Age:compound)//
% # Syntax
%
% ~~~{.abnf}
% Age = "Age" ":" age-value
% ~~~
%
% # Semantics
%
% The `Age` response-header field conveys the sender's estimate of
%  the amount of time since the response (or its revalidation) was generated
%  at the origin server.
% A cached response is "fresh" if its age does not exceed
%  its freshness lifetime.
%
% @see `Age` values are calculated as specified in section 13.2.3.

'Age'('Age'(T1), 'Age'(Seconds)) -->
  "Age:",
  'age-value'(T1, Seconds).



%! 'age-value'(-ParseTree:compound, ?Age:nonneg)//
% # Syntax
%
% Age values are non-negative decimal integers, representing time in seconds.
%
% ~~~{.abnf}
% age-value = delta-seconds
% ~~~
%
% # Pragmatics
%
% An HTTP/1.1 server that includes a cache MUST include an `Age` header field
%  in every response generated from its own cache.
%
% ## Large integers
%
% If a cache receives a value larger than the largest positive integer
%  it can represent, or if any of its age calculations overflows,
%  it MUST transmit an `Age` header with a value of `2147483648 (2^31)`.
%
% ## Minimum precision
%
% Caches SHOULD use an arithmetic type of at least `31` bits of range.

'age-value'('age-value'(T1), Seconds) -->
  'delta-seconds'(T1, Seconds).

