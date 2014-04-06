:- module(
  rfc2616_accept_ranges,
  [
    'Accept-Ranges'//2 % -ParseTree:compound
                       % ?AcceptRanges:compound
  ]
).

/** <module> RFC 2616 accept ranges

DCG for accept ranges header in RFC 2616.

# Datatypes

## AcceptRanges

~~~{.pl}
'Accept-Ranges'(
  RangeUnits:list(atom)
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(parse_tree)).
:- use_remote_module(flp(rfc2616_abnf)).
:- use_remote_module(http_parameters(rfc2616_range_unit)).



%! 'Accept-Ranges'(-ParseTree:compound, ?AcceptRanges:compound)//
% The `Accept-Ranges` response-header field allows the server to indicate
%  its acceptance of range requests for a resource.
%
% ~~~{.abnf}
% Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
% acceptable-ranges = 1#range-unit | "none"
% ~~~
%
% Origin servers that accept byte-range requests MAY send
% ~~~{.abnf}
% Accept-Ranges: bytes
% ~~~
% but are not required to do so.
%
% Clients MAY generate byte-range requests without having received this header
%  for the resource involved.
%
% @see Range units are defined in [rfc2616_range_unit].
%
% Servers that do not accept any kind of range request for a resource MAY send
% ~~~{.abnf}
% Accept-Ranges: none
% ~~~
% to advise the client not to attempt a range request.

'Accept-Ranges'('Accept-Ranges'(T1), 'Accept-Ranges'(RangeUnits)) -->
  "Accept-Ranges:",
  'acceptable-ranges'(T1, RangeUnits).

'acceptable-ranges'(T0, RangeUnits) -->
  abnf_list2('range-unit', 1-_, Ts, RangeUnits),
  {parse_tree('acceptable-ranges', Ts, T0)}.
'acceptable-ranges'('acceptable-ranges'(none), []) -->
  "none".

