:- module(
  xsd,
  [
    xsd_canonical_map/3, % +DatatypeIri:iri
                         % +Value
                         % -LexicalForm:atom
    xsd_compare/4, % +DatatypeIri:iri
                   % -Order:oneof([incomparable,<,=,>])
                   % +Value1
                   % +Value2
    xsd_datatype/1, % ?DatatypeIri:iri
    xsd_datatype/2, % ?DatatypeName:atom
                    % ?DatatypeIri:iri
    xsd_datatype/3, % ?DatatypeName:atom
                    % ?DatatypeIri:iri
                    % ?PrologName:atom
    xsd_lexical_map/3 % +DatatypeIri:iri
                      % +LexicalForm:atom
                      % ?Value
  ]
).

/** <module> XML Schema 2: Datatypes

Support for XML Scheme 2 datatypes,
conforming to recommendation version 1.1.

@author Wouter Beek
@compat XML Schema 2: Datatypes (Second Edition)
@see Canonical map for double does not work (loops on log value =:= 0.
@see http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/
@see Turn the infinite datatype requirements into a unit test.
@tbd Implement =base64Binary=.
@tbd Implement =anyURI=.
@tbd Implement =QNAME=.
@tbd Implement =NOTATION=.
@tbd Implement the non-primitive built-in atomic and list datatypes.
@tbd Read section 4: DatatypeIri components.
@tbd Read section 5: Conformance.
@tbd Read section E.3.3 on adding durations to dateTime.
@tbd Read section G on REs.
@tbd Read section H on implementation-defined datatypes.
@version 2013/08-2013/10, 2014/01, 2014/03-2014/04
*/

:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(generics(codes_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % RDF-meta assertions.
:- use_remote_module(rdf(rdf_read)).
:- use_remote_module(xml(xml_namespace)).
:- use_remote_module(xsd(xsd_boolean)).
:- use_remote_module(xsd(xsd_date)).
:- use_remote_module(xsd(xsd_dateTime)).
:- use_remote_module(xsd(xsd_decimal)).
:- use_remote_module(xsd(xsd_duration)).
:- use_remote_module(xsd(xsd_float)).
:- use_remote_module(xsd(xsd_gDay)).
:- use_remote_module(xsd(xsd_gMonth)).
:- use_remote_module(xsd(xsd_gMonthDay)).
:- use_remote_module(xsd(xsd_gYear)).
:- use_remote_module(xsd(xsd_gYearMonth)).
:- use_remote_module(xsd(xsd_hexBinary)).
:- use_remote_module(xsd(xsd_integer)).
:- use_remote_module(xsd(xsd_string)).
:- use_remote_module(xsd(xsd_time)).

:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- rdf_meta(xsd_canonical_map(r,+,-)).
:- rdf_meta(xsd_canonical_map_(r,+,-)).
:- rdf_meta(xsd_datatype(r)).
:- rdf_meta(xsd_datatype(?,r)).
:- rdf_meta(xsd_datatype(?,r,?)).
:- rdf_meta(xsd_lexical_map(r,+,?)).



%! xsd_canonical_map(+DatatypeIri:iri, +Value, -LexicalForm:atom) is det.

xsd_canonical_map(DatatypeIri, Value, LexicalForm):-
  xsd_canonical_map_(DatatypeIri, Value, Codes),
  atom_codes(LexicalForm, Codes).

xsd_canonical_map_(xsd:boolean, Boolean, LexicalForm):- !,
  phrase(xsd_boolean_canonical_map(Boolean), LexicalForm).
xsd_canonical_map_(xsd:date, Date, LexicalForm):- !,
  phrase(xsd_date_canonical_map(Date), LexicalForm).
xsd_canonical_map_(xsd:dateTime, DateTime, LexicalForm):- !,
  phrase(xsd_dateTime_canonical_map(DateTime), LexicalForm).
xsd_canonical_map_(xsd:decimal, Decimal, LexicalForm):- !,
  phrase(xsd_decimal_canonical_map(Decimal), LexicalForm).
xsd_canonical_map_(xsd:double, Double, LexicalForm):- !,
  phrase(xsd_double_canonical_map(Double), LexicalForm).
xsd_canonical_map_(xsd:duration, Duration, LexicalForm):- !,
  phrase(xsd_duration_canonical_map(Duration), LexicalForm).
xsd_canonical_map_(xsd:float, Float, LexicalForm):- !,
  phrase(xsd_float_canonical_map(Float), LexicalForm).
xsd_canonical_map_(xsd:gDay, GregorianDay, LexicalForm):- !,
  phrase(xsd_gDay_canonical_map(GregorianDay), LexicalForm).
xsd_canonical_map_(xsd:gMonth, GregorianMonth, LexicalForm):- !,
  phrase(xsd_gMonth_canonical_map(GregorianMonth), LexicalForm).
xsd_canonical_map_(xsd:gMonthDay, GregorianMonthDay, LexicalForm):- !,
  phrase(xsd_gMonthDay_canonical_map(GregorianMonthDay), LexicalForm).
xsd_canonical_map_(xsd:gYear, GregorianYear, LexicalForm):- !,
  phrase(xsd_gYear_canonical_map(GregorianYear), LexicalForm).
xsd_canonical_map_(xsd:gYearMonth, GregorianYearMonth, LexicalForm):- !,
  phrase(xsd_gYearMonth_canonical_map(GregorianYearMonth), LexicalForm).
xsd_canonical_map_(xsd:hexBinary, HexBinary, LexicalForm):- !,
  phrase(xsd_hexBinary_canonical_map(HexBinary), LexicalForm).
xsd_canonical_map_(xsd:integer, Integer, LexicalForm):- !,
  phrase(xsd_integer_canonical_map(Integer), LexicalForm).
xsd_canonical_map_(xsd:string, String, LexicalForm):- !,
  phrase(xsd_string_canonical_map(String), LexicalForm).
xsd_canonical_map_(xsd:time, Time, LexicalForm):- !,
  phrase(xsd_time_canonical_map(Time), LexicalForm).


%! xsd_datatype(+DatatypeIri:iri) is semidet.
%! xsd_datatype(-DatatypeIri:iri) is nondet.

xsd_datatype(DatatypeIri):-
  xsd_datatype(_, DatatypeIri).

%! xsd_datatype(+DatatypeName:atom, +DatatypeIri:iri) is semidet.
%! xsd_datatype(+DatatypeName:atom, -DatatypeIri:iri) is det.
%! xsd_datatype(-DatatypeName:atom, +DatatypeIri:iri) is det.
%! xsd_datatype(-DatatypeName:atom, -DatatypeIri:iri) is nondet.

xsd_datatype(DatatypeName, DatatypeIri):-
  xsd_datatype(DatatypeName, DatatypeIri, _).

%! xsd_datatype(?DatatypeName:atom, ?DatatypeIri:iri, ?PrologName:atom) .

xsd_datatype(boolean,    xsd:boolean   , boolean).
xsd_datatype(date,       xsd:date      , date   ).
xsd_datatype(dateTime,   xsd:dateTime  , date   ).
xsd_datatype(decimal,    xsd:decimal   , float  ).
xsd_datatype(double,     xsd:double    , float  ).
xsd_datatype(duration,   xsd:duration  , date   ).
xsd_datatype(float,      xsd:float     , float  ).
xsd_datatype(gDay,       xsd:gDay      , date   ).
xsd_datatype(gMonth,     xsd:gMonth    , date   ).
xsd_datatype(gMonthDay,  xsd:gMonthDay , date   ).
xsd_datatype(gYear,      xsd:gYear     , date   ).
xsd_datatype(gYearMonth, xsd:gYearMonth, date   ).
xsd_datatype(hexBinary,  xsd:hexBinary , unknown).
xsd_datatype(integer,    xsd:integer   , integer).
xsd_datatype(string,     xsd:string    , atom   ).
xsd_datatype(time,       xsd:time      , date   ).


%! xsd_lexical_map(+DatatypeIri:iri, +LexicalForm:atom, +Value) is semidet.
%! xsd_lexical_map(+DatatypeIri:iri, +LexicalForm:atom, -Value) is det.

xsd_lexical_map(xsd:boolean, LexicalForm, Boolean):- !,
  dcg_phrase(xsd_boolean_lexical_map(Boolean), LexicalForm).
xsd_lexical_map(xsd:date, LexicalForm, Date):- !,
  dcg_phrase(xsd_date_lexical_map(Date), LexicalForm).
xsd_lexical_map(xsd:dateTime, LexicalForm, DateTime):- !,
  dcg_phrase(xsd_dateTime_lexical_map(DateTime), LexicalForm).
xsd_lexical_map(xsd:decimal, LexicalForm, Decimal):- !,
  dcg_phrase(xsd_decimal_lexical_map(Decimal), LexicalForm).
xsd_lexical_map(xsd:double, LexicalForm, Double):- !,
  dcg_phrase(xsd_double_lexical_map(Double), LexicalForm).
xsd_lexical_map(xsd:duration, LexicalForm, Duration):- !,
  dcg_phrase(xsd_duration_lexical_map(Duration), LexicalForm).
xsd_lexical_map(xsd:float, LexicalForm, Float):- !,
  dcg_phrase(xsd_float_lexical_map_friendly(Float), LexicalForm).
xsd_lexical_map(xsd:gDay, LexicalForm, GregorianDay):- !,
  dcg_phrase(xsd_gDay_canonical_map(GregorianDay), LexicalForm).
xsd_lexical_map(xsd:gMonth, LexicalForm, GregorianMonth):- !,
  dcg_phrase(xsd_gMonth_lexical_map(GregorianMonth), LexicalForm).
xsd_lexical_map(xsd:gMonthDay, LexicalForm, GregorianMonthDay):- !,
  dcg_phrase(xsd_gMonthDay_lexical_map(GregorianMonthDay), LexicalForm).
xsd_lexical_map(xsd:gYear, LexicalForm, GregorianYear):- !,
  dcg_phrase(xsd_gYear_lexical_map(GregorianYear), LexicalForm).
xsd_lexical_map(xsd:gYearMonth, LexicalForm, GregorianYearMonth):- !,
  dcg_phrase(xsd_gYearMonth_lexical_map(GregorianYearMonth), LexicalForm).
xsd_lexical_map(xsd:hexBinary, LexicalForm, HexBinary):- !,
  dcg_phrase(xsd_hexBinary_lexical_map(HexBinary), LexicalForm).
xsd_lexical_map(xsd:integer, LexicalForm, Integer):- !,
  dcg_phrase(xsd_integer_lexical_map(Integer), LexicalForm).
xsd_lexical_map(xsd:string, LexicalForm, String):- !,
  dcg_phrase(xsd_string_lexical_map(String), LexicalForm).
xsd_lexical_map(xsd:time, LexicalForm, Time):- !,
  dcg_phrase(xsd_time_lexical_map(Time), LexicalForm).


%! xsd_compare(+DatatypeIri:iri, ?Order:oneof([<,=,>]), +Value1, +Value2) is semidet.
% Fails only if the given values are incomparable.

% Date-time comparisons.
xsd_compare(DatatypeIri, Order, DateTime1, DateTime2):-
  rdf_memberchk(
    DatatypeIri,
    [
      xsd:date,
      xsd:dateTime,
      xsd:gDay,
      xsd:gMonth,
      xsd:gMonthDay,
      xsd:gYear,
      xsd:gYearMonth
    ]
  ), !,
  xsd_dateTime_compare(Order, DateTime1, DateTime2).
% Duration comparisons.
xsd_compare(DatatypeIri, Order, Duration1, Duration2):-
  rdf_memberchk(DatatypeIri, [xsd:duration,xsd:xsd_yearMonthDuration]), !,
  xsd_duration_compare(Order, Duration1, Duration2).
% Numeric comparators.
xsd_compare(DatatypeIri, Order, Value1, Value2):-
  rdf_memberchk(DatatypeIri, [xsd:decimal,xsd:double,xsd:float,xsd:integer]),
  compare(Order, Value1, Value2).

