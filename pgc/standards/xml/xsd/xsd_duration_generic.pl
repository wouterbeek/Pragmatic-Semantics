:- module(
  xsd_duration_generic,
  [
    duYearMonthFrag//1 % -Month:nonneg
  ]
).

/** <module> XSD duration generic

Generic predicates and grammar rules that are used by the duration datatypes.

@author Wouter Beek
@version 2013/07-2013/08, 2014/03
*/

:- use_remote_module(xsd(xsd_dateTime_generic)).
:- use_remote_module(xsd(xsd_number_generic)).



%! duMonthFrag(-Month:nonneg)//
% ~~~{.ebnf}
% duMonthFrag ::= unsignedNoDecimalPtNumeral 'M'
% ~~~

duMonthFrag(M) -->
  unsignedNoDecimalPtNumeral(M),
  `M`.


%! duYearFrag(-Year:nonneg)//
% ~~~{.ebnf}
% duYearFrag ::= unsignedNoDecimalPtNumeral 'Y'
% ~~~

duYearFrag(Y) -->
  unsignedNoDecimalPtNumeral(Y),
  `Y`.


%! duYearMonthFrag(-Month:nonneg)//
% ~~~{.ebnf}
% duYearMonthFrag ::= (duYearFrag duMonthFrag?) | duMonthFrag
% ~~~

duYearMonthFrag(M2) -->
  (
    duYearFrag(Y),
    (duMonthFrag(M1) ; {M1 = 0})
  ;
    {Y = 0},
    duMonthFrag(M1)
  ),
  {M2 is 12 * Y + M1}.

