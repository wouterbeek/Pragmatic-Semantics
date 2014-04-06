:- module(
  rfc2616_quality_value,
  [
    qvalue//2 % -ParseTree:compound
              % ?QualityValue:between(0.0,1.0)
  ]
).

/** <module> RFC 2616 quality value

DCG for RFC 2616 quality values.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(http(rfc2616_basic)).
:- use_remote_module(math(radix)).



%! qvalue(-ParseTree:compound, ?QualityValue:between(0.0,1.0))//
% HTTP quality values, used in `Accept` and `Accept-Language`.
%
% # Syntax
%
% ~~~{.abnf}
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ~~~
%
% ## Precision
%
% HTTP/1.1 applications MUST NOT generate more than three digits after
%  the decimal point.
% User configuration of these values SHOULD also be limited in this fashion.
%
% # Semantics
%
% HTTP content negotiation uses short floating point numbers to indicate
%  the relative importance ("weight") of various negotiable parameters.
%
% ## Normalization
%
% A weight is normalized to a real number in the range 0 through 1,
%  where 0 is the minimum and 1 the maximum value.
%
% ## `0.0`
%
% If a parameter has a quality value of `0`,
%  then content with this parameter is `not acceptable' for the client.
%
% # Nomenclature
%
% *|Quality values|* is a misnomer, since these values merely represent
%  relative degradation in desired quality.

qvalue(qvalue(D2), D2) -->
  "0",
  (
    ".",
    dcg_multi2('DIGIT', 0-3, _, Ds),
    {digits_to_decimal(Ds, D1)},
    {D2 is D1 / 10}
  ;
    ""
  ).
qvalue(qvalue(1.0), 1.0) -->
  "1",
  (
    ".",
    dcg_multi(zero, 0-3)
  ;
    ""
  ).

