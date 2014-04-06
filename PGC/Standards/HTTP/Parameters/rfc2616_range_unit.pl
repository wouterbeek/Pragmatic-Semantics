:- module(
  rfc2616_range_unit,
  [
    'range-unit'//2 % -ParseTree:compound
                    % ?RangeUnit:atom
  ]
).

/** <module> RFC 2616 range units

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http(rfc2616_generic)).



% ! 'bytes-unit'(-ParseTree:compound, RangeUnit:oneof([bytes]))//
% ~~~
% bytes-unit = "bytes"
% ~~~

'bytes-unit'('bytes-unit'(bytes), bytes) -->
  "bytes".



%! 'other-range-unit'(-ParseTree:compound, ?RangeUnit:atom)//
% ~~~{.abnf}
% other-range-unit = token
% ~~~

'other-range-unit'('other-range-unit'(RangeUnit), RangeUnit) -->
  token(RangeUnit).



%! 'range-unit'(-ParseTree:compound, ?RangeUnit:atom)//
% An entity can be broken down into subranges
%  according to various structural units.
%
% The only range unit defined by HTTP/1.1 is `bytes`.
%
% # Syntax
%
% ~~~{.abnf}
% range-unit = bytes-unit | other-range-unit
% ~~~{.abnf}
%
% # Semantics
%
% HTTP/1.1 allows a client to request that only part (a range of)
%  the response entity be included within the response.
%
% # Pragmatics
%
% HTTP/1.1 implementations MAY ignore ranges specified using other units.
%
% HTTP/1.1 has been designed to allow implementations of applications
%  that do not depend on knowledge of ranges.
%
% HTTP/1.1 uses range units in the `Range` and `Content-Range` header fields.

'range-unit'('range-unit'(T1), RangeUnit) -->
  'bytes-unit'(T1, RangeUnit).
'range-unit'('range-unit'(T1), RangeUnit) -->
  'other-range-unit'(T1, RangeUnit).

