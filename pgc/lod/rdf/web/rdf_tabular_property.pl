:- module(
  rdf_tabular_property,
  [
    rdf_tabular_property//2, % +Graph:atom
                             % +Property:iri
    rdf_tabular_properties//1 % +Graph:atom
  ]
).

/** <module> RDF tabular predicate term

Generates HTML tables that descrive RDF predicate terms.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(generics(list_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(pl_web(html_pl_term)).
:- use_remote_module(rdf(rdf_container)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_term(rdf_term)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(rdf_web(rdf_tabular)).
:- use_remote_module(rdf_web(rdf_term_html)).



%! rdf_tabular_property(+Graph:atom, +Property:iri)// is det.

rdf_tabular_property(G, P) -->
  % The extension of the interpretation of the property consists of pairs.
  % We enumerate the classes of individuals that occur in these pairs.
  % We distinguish between individuals that occur in
  % the first argument position
  % (classes denoting the domain of the property)
  % and the second argument position
  % (classes denoting the range of the property).
  rdf_tabular_property_domain(G, P),
  rdf_tabular_property_range(G, P),

  % For literal ranges we also display the values that occur.
  rdf_tabular_predicate_literals(G, P),

  % Subject-object pairs.
  rdf_tabular_triples(_, P, _, G),

  % Triples that describe the property, if any.
  rdf_tabular_triples(P, _, _, G).


rdf_tabular_property_domain(G, P) -->
  {
    aggregate_all(
      set([Domain]),
      (
        rdf(S, P, _, G),
        rdfs_individual_of(S, Domain)
      ),
      Rows
    )
  },
  rdf_html_table(
    [graph(G),header_row(true)],
    html([
      'Overview of the domain of property ',
      \rdf_term_in_graph_html(rdf_tabular, P, G),
      '.'
    ]),
    [['Class']|Rows]
  ).


rdf_tabular_property_range(G, P) -->
  {
    aggregate_all(
      set([Range]),
      (
        rdf(_, P, O, G),
        rdfs_individual_of(O, Range)
      ),
      Rows
    )
  },
  rdf_html_table(
    [graph(G),header_row(true)],
    html([
      'Overview of the range of property ',
      \rdf_term_in_graph_html(rdf_tabular, P, G),
      '.'
    ]),
    [['Class']|Rows]
  ).


rdf_tabular_predicate_literals(G, P) -->
  {
    aggregate_all(
      set([LiteralValue]),
      ((
        rdf(_, P, literal(type(_,LiteralValue)), G)
      ;
        rdf(_, P, literal(lang(_,LiteralValue)), G)
      ;
        rdf(_, P, literal(LiteralValue), G),
        \+ compound(LiteralValue)
      )),
      Rows1
    ),
    length(Rows1, L),
    list_truncate(Rows1, 50, Rows2)
  },
  html([
    p([
      \rdf_term_html(rdf_tabular, P, G),
      ' has ',
      \html_pl_term(L),
      ' unique values.'
    ]),
    \rdf_html_table(
      [graph(G),header_row(true)],
      html([
        'Values that occur for property ',
        \rdf_term_in_graph_html(rdf_tabular, P, G),
        '.'
      ]),
      [['Literal value']|Rows2]
    )
  ]).


rdf_tabular_properties(G) -->
  {
    aggregate_all(
      set(Predicate),
      (
        rdf_predicate(Predicate, G),
        % Exclude RDF container membership properties.
        \+ rdf_container_membership_property(Predicate, _)
      ),
      Predicates
    ),
    findall(
      NumberOfOccurrences-Predicate,
      (
        member(Predicate, Predicates),
        aggregate_all(
          count,
          rdf(_, Predicate, _, G),
          NumberOfOccurrences
        )
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [Predicate,NumberOfOccurrences],
      member(NumberOfOccurrences-Predicate, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    [graph(G),header_row(true)],
    html([
      'Overview of properties in RDF graph ',
      \rdf_graph_html(rdf_tabular, G),
      '.'
    ]),
    [['Predicate','Occurrences']|Rows]
  ).

