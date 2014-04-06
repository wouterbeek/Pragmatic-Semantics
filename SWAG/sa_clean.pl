:- module(
  sa_clean,
  [
    sa_clean/1 % +Graph:atom
  ]
).

/** <module> Sackner Archives cleaning

Automated cleaning of the Sackner Archives dataset.

@author Wouter Beek
@version 2014/03-2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(lod(wb)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal_build)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_web(rdf_store_table)).
:- use_module(rdf_web(rdf_term_html)).
:- use_module(rdfs(rdfs_read)).
:- use_module(swag(swag_db)).
:- use_module(xsd(xsd)).
:- use_module(xsd(xsd_clean)).

:- rdf_meta(sa_clean(r,r,r,+)).
:- rdf_meta(sa_clean_preview(r,r,r,+)).



sa_clean(G):-
  forall(
    (
      % @tbd Use rdfs_domain/4 when its done.
      rdf(P, rdfs:range, Range, G),
      xsd_datatype(Range),
      \+ rdf_equal(Range, xsd:string)
    ),
    sa_clean_preview(P, xsd:string, Range, G)
  ).


sa_clean(P, FromDatatype, ToDatatype, G):-
  forall(
    rdf_literal(S, P, FromLexicalForm, FromDatatype, G),
    (
      xsd_convert_value(FromDatatype, FromLexicalForm, ToDatatype, ToLexicalForm),
      rdf_assert_literal(S, P, ToLexicalForm, ToDatatype, G),
      rdf_retractall_literal(S, P, FromLexicalForm, FromDatatype, G)
    )
  ).


sa_clean_preview(P, FromDatatype, ToDatatype, G):-
  findall(
    [S,P,FromLiteral,ToLiteral,G],
    (
      rdf_literal(S, P, FromLexicalForm, FromDatatype, G),
      xsd_convert_value(
        FromDatatype,
        FromLexicalForm,
        ToDatatype,
        ToLexicalForm
      ),
      rdf_literal(FromLiteral, FromLexicalForm, FromDatatype),
      rdf_literal(ToLiteral, ToLexicalForm, ToDatatype)
    ),
    Rows
  ),
  rdf_store_rows(
    html([
      'Literal term transformations for RDF property ',
      \rdf_term_html(rdf_tabular, P),
      '.'
    ]),
    ['Subject','Predicate','Old literal','New literal','Graph'],
    Rows
  ).



/*
% Not all years are compliant with `xsd:gYear`.
sa_assert_value(Entry, Predicate, year, Value1, Graph):-
  once(dcg_phrase(year(Value2), Value1)), !,
  sa_assert_value(Entry, Predicate, gYear, Value2, Graph).
sa_assert_value(Entry, Predicate, dimensions, Value, Graph):- !,
  once(dcg_phrase(dimensions(Height, Width, Depth), Value)),
  rdf_bnode(BNode),
  rdf_assert(Entry, Predicate, BNode, Graph),
  rdf_assert_datatype(BNode, swag:height, Height, xsd:decimal, Graph),
  rdf_assert_datatype(BNode, swag:width, Width, xsd:decimal, Graph),
  (
    var(Depth), !
  ;
    rdf_assert_datatype(BNode, swag:depth, Depth, xsd:decimal, Graph)
  ).
sa_assert_value(Entry, Predicate, DatatypeName, Value1, Graph):-
  xsd_datatype(DatatypeName, Datatype),
  xsd_value(DatatypeName, Value1, Value2),
  rdf_assert_datatype(Entry, Predicate, Value2, Datatype, Graph), !.
sa_assert_value(Entry, Predicate, Datatype, Value, Graph):-
  gtrace, %DEB
  format(user_output, '<~w,~w,~w^^~w>', [Entry,Predicate,Value,Datatype]),
  sa_assert_value(Entry, Predicate, Datatype, Value, Graph).

year(Year) -->
  (`c.`, blanks ; []),
  xsd_gYear_lexical_map(dateTime(Year, _, _, _, _, _, _)).

dimensions(Height, Width, Depth) -->
  xsd_decimal_lexical_map(Height),
  dimensions_separator,
  xsd_decimal_lexical_map(Width),
  (
    dimensions_separator,
    xsd_decimal_lexical_map(Depth)
  ;
    []
  ).

dimensions_separator -->
  blanks,
  `x`,
  blanks.


year(Year) -->
  (`c.`, blanks ; []),
  gYearLexicalRep(dateTime(Year, _, _, _, _, _, _)).
*/

