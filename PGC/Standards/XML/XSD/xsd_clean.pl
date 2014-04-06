:- module(
  xsd_clean,
  [
    pl_to_xsd_value/3, % +DatatypeIri:or([atom,iri])
                       % +PlValue
                       % -XsdValue
    xsd_canonize_graph/1, % +Graph:atom
    xsd_convert_value/4, % +FromDatatype:iri
                         % +FromLexicalForm:atom
                         % +ToDatatype:iri
                         % -ToLexicalForm:atom
    xsd_convert_value/4, % +FromDatatype:iri
                         % +FromLexicalForm:list(code)
                         % +ToDatatype:iri
                         % -ToLexicalForm:list(code)
    xsd_lexical_canonical_map/3, % +DatatypeIri:iri
                                 % +LexicalForm:atom
                                 % -CanonicalLexicalForm:atom
    xsd_value/3 % ?DatatypeIri:or([atom,iri])
                % ?Value
                % ?XsdValue
  ]
).

/** <module> XSD clean

Predicates for cleaning XML Scheme 1.1 datatypes.

@author Wouter Beek
@version 2013/08-2013/10, 2014/01, 2014/03-2014/04
*/

:- use_module(generics(boolean_ext)).
:- use_module(generics(codes_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(pl_to_xsd_value(r,+,-)).
:- rdf_meta(xsd_convert_value(r,+,r,-)).
:- rdf_meta(xsd_value(+,r,-)).



%! pl_to_xsd_value(
%!   +DatatypeIri:iri,
%!   +PlValue1:compound,
%!   -XsdValue:compound
%! ) is det.
% A layer on top of xsd_value/3 which supports more input values.
% Ideally, it would be possible to transform lots of different kinds
% of Prolog values to their XSD equivalent.

pl_to_xsd_value(Datatype, PlValue1, XsdValue):-
  rdf_equal(xsd:boolean, Datatype), !,
  to_boolean(PlValue1, PlValue2),
  xsd_value(Datatype, PlValue2, XsdValue).
pl_to_xsd_value(Datatype, PlValue, XsdValue):-
  xsd_value(Datatype, PlValue, XsdValue).


%! xsd_canonize_graph(+Graph:atom) is det.
% Make sure all typed literals in the graph with the given name
% have a lexical value that is a canonical value for its datatype.
%
% This check every RDF triple in the given graph
% that contains a typed literal.
%
% @tbd Use a predicate from module [rdf/rdf_literal].
% @tbd Is is not more efficient to canonize per object term?
%      Triples that share the same non-canonical typed literal
%      will be updated at once.

xsd_canonize_graph(Graph):-
  forall(
    rdf(Subject, Predicate, literal(type(DatatypeIri,LexicalForm)), Graph),
    xsd_canonize_triple(Subject, Predicate, DatatypeIri, LexicalForm, Graph)
  ).


%! xsd_canonize_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +DatatypeIri:iri,
%!   +LexicalForm:atom,
%!   +Graph:atom
%! ) is det.
% Converts from lexical to value,
% and then from value to canonical lexical.

xsd_canonize_triple(Subject, Predicate, DatatypeIri, LexicalForm, Graph):-
  xsd_datatype(DatatypeIri),
  xsd_lexical_canonical_map(DatatypeIri, LexicalForm, CanonicalLexicalForm),

  % Only changes need to be written.
  LexicalForm \== CanonicalLexicalForm,

  % Update the object term.
  rdf_update(
    Subject,
    Predicate,
    literal(type(DatatypeIri,LexicalForm)),
    Graph,
    object(literal(type(DatatypeIri,CanonicalLexicalForm)))
  ).


%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalForm:atom,
%!   +ToDatatype:uri,
%!   -ToLexicalForm:atom
%! ) is semidet.
%! xsd_convert_value(
%!   +FromDatatype:uri,
%!   +FromLexicalForm:list(code),
%!   +ToDatatype:uri,
%!   -ToLexicalForm:list(code)
%! ) is semidet.

xsd_convert_value(FromDatatype, FromLexical1, ToDatatype, ToLexical):-
  atomic_codes(FromLexical1, FromLexical2),
  xsd_convert_value_codes(FromDatatype, ToDatatype, FromLexical2, ToLexical).

% We can simply copy the lexical form, since it can be mapped to the new value space.
%%%%xsd_convert_value_codes(_, ToDatatype, Lexical, Lexical):-
  %%%%xsd_lexical_map(ToDatatype, Lexical, _), !.
xsd_convert_value_codes(FromXsdDatatype, ToXsdDatatype, FromLexical, ToLexical):-
  xsd_lexical_map(FromXsdDatatype, FromLexical, FromValue),

  % Convert between values in Prolog.
  xsd_datatype(_, FromXsdDatatype, FromPlDatatype),
  xsd_datatype(_, ToXsdDatatype, ToPlDatatype),
  prolog_convert_value(FromPlDatatype, FromValue, ToPlDatatype, ToValue),

  xsd_canonical_map(ToXsdDatatype, ToValue, ToLexical).


%! xsd_lexical_canonical_map(+DatatypeIri:iri, +LexicalForm:atom, -CanonicalLexicalForm:atom) is det.
% Reads a datatype lexical expression and converts it into its canonical form.

xsd_lexical_canonical_map(DatatypeIri, LexicalForm, CanonicalLexicalForm):-
  xsd_convert_value(DatatypeIri, LexicalForm, DatatypeIri, CanonicalLexicalForm).


%! xsd_value(+DatatypeIri:or([atom,iri]), +Value1, -Value2) is det.
%! xsd_value(-DatatypeIri:iri,            +Value1, -Value2) is nondet.

% Try out different datatypes.
xsd_value(DatatypeIri, LexicalForm, CanonicalLexicalForm):-
  var(DatatypeIri), !,
  % Choicepoint.
  xsd_datatype(DatatypeIri),
  xsd_value(DatatypeIri, LexicalForm, CanonicalLexicalForm).
% Allow datatypes to be denoted by a shortcut name.
xsd_value(Name, PrologValue, CanonicalLexicalForm):-
  xsd_datatype(Name, DatatypeIri), !,
  xsd_value(DatatypeIri, PrologValue, CanonicalLexicalForm).
% The value is not yet in XSD format, but could be converted
% by using the supported mappings.
xsd_value(DatatypeIri, XsdValue, CanonicalLexicalForm):-
  xsd_datatype(DatatypeIri),
  xsd_canonical_map(DatatypeIri, XsdValue, CanonicalLexicalForm), !.
% The value is already in XSD format: recognize that this is the case
% and convert it back and forth to ensure we have the canonical lexical.
xsd_value(DatatypeIri, LexicalForm, CanonicalLexicalForm):-
  xsd_datatype(DatatypeIri),
  xsd_lexical_map(DatatypeIri, LexicalForm, XsdValue),
  xsd_value(DatatypeIri, XsdValue, CanonicalLexicalForm).

