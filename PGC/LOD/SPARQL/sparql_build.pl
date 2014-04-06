:- module(
  sparql_build,
  [
    sparql_count//5, % +Regime:oneof([owl])
                     % +DefaultGraph:iri
                     % +Prefixes:list(atom)
                     % +Variable:atom
                     % +BGPs:or([compound,list(compound)])
    sparql_formulate//10 % ?Regime:oneof([owl])
                         % ?DefaultGraph:iri
                         % +Prefixes:list(atom)
                         % +Mode:oneof([select])
                         % +Distinct:boolean
                         % +Variables:list(atom)
                         % +BGPs:or([compound,list(compound)])
                         % ?Limit:or([nonneg,oneof([inf])])
                         % ?Offset:nonneg
                         % ?Order:pair(oneof([asc]),list(atom))
  ]
).

/** <module> SPARQL build

DCGs for constructing SPARQL queries.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/02
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_cardinal)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(xml(xml_namespace)).



bgp([]) --> [].
bgp([filter(Filter)|T]) -->
  "FILTER ",
  filter(Filter),
  bgp(T).
bgp([optional(Optional)|T]) -->
  "  OPTIONAL {\n",
  bgp(Optional),
  "  }\n",
  bgp(T).
bgp([rdf(S,P,O)|T]) -->
  "  ",
  term(S),
  " ",
  term(P),
  " ",
  term(O),
  " .\n",
  bgp(T).

default_graph(VAR) -->
  {var(VAR)}, !,
  [].
default_graph(DefaultGraph) -->
  "# Default graph (located at ",
  atom(DefaultGraph),
  ")\n".

define(inference(Regime)) -->
  "define input:inference ",
  define_inference_regime(Regime),
  "\n".

define_inference_regime(owl) -->
  quoted(atom('http://www.w3.org/2002/07/owl#')).

distinct(true) -->
  " DISTINCT".
distinct(false) --> [].


%! filter(+Filter:compound)// is det.
% The following filters are supported:
%   * =|regex(+Term, +ReMatch:compound, +ReFlags:list(oneof([case_sensitive])))|=
%     Succeeds when `Term` matches `ReMatch` under the given `ReFlags`.
%     The following RE matches are supported:
%       * =|at_start(+String:atom)|=
%     The following RE flags are supported:
%       * `case_insensitive`
%   * =|strends(+Term, +Match:compound)|=
%     Succeeds if `Term` is instantiated by a string that ends in `Match`.
%     The following matching terms are supported:
%       * =|string(+String:atom)|=

filter(regex(Arg1,Arg2)) -->
  filter(regex(Arg1,Arg2,[])).
filter(regex(Arg1,Arg2,Flags)) -->
  "REGEX(",
    term(Arg1),
    ",",
    term(Arg2),
    regex_flags(Flags),
  ")".
filter(strends(Arg1,Arg2)) -->
  "STRENDS(",
    term(Arg1),
    ",",
    term(Arg2),
  ")".


inference_regime(VAR) -->
  {var(VAR)}, !,
  [].
inference_regime(Regime) -->
  define(inference(Regime)).


%! iri(+Iri:iri)// is det.
% An IRI term.

iri(Iri) -->
  bracketed(angular, atom(Iri)).


limit(VAR) -->
  {var(VAR)}, !,
  [].
limit(inf) --> !, [].
limit(Limit) -->
  "LIMIT ",
  integer(Limit),
  "\n".

mode(select) -->
  "SELECT".

offset(VAR) -->
  {var(VAR)}, !,
  [].
offset(Offset) -->
  "OFFSET ",
  integer(Offset),
  "\n".

order(VAR) -->
  {var(VAR)}, !,
  [].
order(Criterion-Variables) -->
  "ORDER BY ",
  order_criterion(Criterion),
  bracketed(variables(Variables)),
  "\n".

order_criterion(ascending) -->
  "ASC".

prefix(Prefix) -->
  {xml_current_namespace(Prefix, Iri)},
  "PREFIX ",
  atom(Prefix),
  ": ",
  iri(Iri),
  "\n".

prefixes([]) --> [].
prefixes([H|T]) -->
  prefix(H),
  prefixes(T).

regex_flags([]) --> [].
regex_flags(Flags) -->
  ",",
  quoted(regex_flags1(Flags)).

regex_flags1([]) --> [].
regex_flags1([case_insensitive|T]) -->
  "i",
  regex_flags1(T).


%! sparql_count(
%!   +Regime:oneof([owl]),
%!   +DefaultGraph:iri,
%!   +Prefixes:list(atom),
%!   +Variable:atom,
%!   +BGPs:or([compound,list(compound)])
%! )// is det.

sparql_count(Regime, DefaultGraph, Prefixes, Variable, BGPs) -->
  inference_regime(Regime),
  default_graph(DefaultGraph),
  prefixes(Prefixes),
  `SELECT COUNT`,
  bracketed(variable(Variable)),
  "\n",
  where(BGPs).


%! sparql_formulate(
%!   +Regime:oneof([owl]),
%!   +DefaultGraph:iri,
%!   +Prefixes:list(atom),
%!   +Mode:oneof([select]),
%!   +Distinct:boolean,
%!   +Variables:list(atom),
%!   +BGP:or([compound,list(compound)]),
%!   +Limit:or([nonneg,oneof([inf])]),
%!   +Offset:nonneg,
%!   +Order:pair(oneof([asc]),list(atom))
%! )// is det.
%
% # Example
%
% ~~~{.pl}
% 'SPARQL(_, [rdfs], select, true, [class], Where, inf, asc-class)
% ~~~
% With the corresponding SPARQL query:
% ~~~{.sparql}
% PREFIX rdf: <...>
% SELECT DISTINCT ?class
% WHERE {
%   dbpedia:Monkey rdf:type ?x .
%   ?x rdfs:subClassOf* ?class .
% }
% ORDER BY ASC(?class)
% ~~~
%
% @arg Regime The inference regime. Currently only OWL is supported.
% @arg DefaultGraph An IRI denoting the default graph to query from.
% @arg Prefixes A list of registered atomic XML prefixes.
% @arg Mode The mode of the SPARQL query.
%      Currently only `select` is supported.
% @arg Distinct Whether the returned results should be distinct or not.
% @arg Variables A list of atomic variable names.
% @arg BGP A list denoting a basic graph pattern or a compound term
%      of the form =|union(BGPs)|= where `BGPs` is a list
%      of basic graph patterns.
% @arg Limit Either a positive integer indicating the maximum number of
%      retrieved results, or `inf`.
% @arg Offset
% @arg Order A pair consisting of the ordering criterion and the variables
%      relative to which ordering takes place.
%      Currently the only supported ordering criterion is `asc` for
%      ascending lexicographically.
%
% @tbd Update examples in predicate documentation.

sparql_formulate(
  Regime,
  DefaultGraph,
  Prefixes,
  Mode,
  Distinct,
  Variables,
  BGPs,
  Limit,
  Offset,
  Order
) -->
  inference_regime(Regime),
  default_graph(DefaultGraph),
  prefixes(Prefixes),
  mode(Mode),
  distinct(Distinct),
  " ",
  variables(Variables),
  "\n",
  where(BGPs),
  limit(Limit),
  offset(Offset),
  order(Order).


%! term(+Term)// is det.
% The following terms are supported:
%   * `a`
%     Abbreviation for `rdf:type`.
%   * `at_start(String)`
%     String pattern matching the start of a string.
%   * `iri(IRI)`
%     Unprefixed IRI.
%   * `var(Variable)`
%     SPARQL variable.
%   * `Prefix:Postfix`
%     Prefixed IRI.

term(a) --> !,
  "a".
term(at_start(String)) -->
  double_quote,
  "^",
  atom(String),
  double_quote.
term(closure(Term,Closure)) -->
  term(Term),
  term_closure(Closure).
term(iri(Iri)) --> !,
  iri(Iri).
term(string(String)) --> !,
  quoted(atom(String)).
term(var(Variable)) --> !,
  variable(Variable).
term(Prefix:Postfix) --> !,
  atom(Prefix),
  ":",
  atom(Postfix).

term_closure([reflexive,transitive]) -->
  "*".
term_closure([transitive]) -->
  "+".


union([]) --> [].
union([H]) -->
  bgp(H).
union([H|T]) -->
  bgp(H),
  `} UNION {`,
  union(T).


variable(Variable) -->
  "?",
  atom(Variable).

variables('*') -->
  "*".
variables([H|T]) -->
  variable(H),
  variables1(T).

variables1([]) --> [].
variables1([H|T]) -->
  " ",
  variable(H),
  variables1(T).


where(Content) -->
  `WHERE `,
  bracketed(curly, where_inner(Content)).

where_inner(union(L)) --> !,
  union(L).
where_inner(BGP) -->
  bgp(BGP).

