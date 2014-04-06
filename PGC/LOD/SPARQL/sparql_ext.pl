:- module(
  sparql_ext,
  [
    sparql_enqueue/5, % +Remote:atom
                        % +Query:atom
                        % +Attempts:or([oneof([inf]),positive_integer])
                        % -VarNames:list
                        % -Results:list
    sparql_query/4, % +Remote:atom
                      % +Query:atom
                      % -VarNames:list
                      % -Results:list
    sparql_query_sameas/3 % +Remote:atom
                            % +Resource:iri
                            % -IdenticalResources:ordset
  ]
).

/** <module> SPARQL extensions

Predicates for executing SPARQL queries.

# SPARQL 1.1 Query Language

`true` and `false` are `xsd:boolean`.
Numbers with `e` are `xsd:double`.
Numbers with `.` are `xsd:decimal`.
Integers are `xsd:integer`.

`a` abbreviates `rdf:type`.

Blank nodes are denotated by `_:LABEL`,
 where `LABEL` need not reflect the label that is used in the triple store.

Predicate-object lists `;`

Object-lists `,`

`()` is `rdf:nil`.
`a` is `rdf:type`.

Alternatives `UNION`.

~~~{.sparql}
FILTER NOT EXISTS { pattern }
FILTER EXISTS { pattern }
{ pattern } MINUS { pattern }
~~~

~~~{.sparql}
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX ns: <http://example.org/ns#>
SELECT ?title ?price
WHERE {
  ?x ns:price ?price .
  FILTER (?price < 30.5)
  ?x dc:title ?title .
}
~~~

Count the number of employees in each department:

~~~{.sparql}
SELECT DISTINCT ?dept (COUNT(?emp) AS ?count)
WHERE {
  ?dept a f:dept .
  ?emp f:Dept ?dept .
} GROUP BY ?dept
~~~

# Sample query

~~~{.sparql}
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT *
WHERE {
  ?s rdf:type rdfs:Class .
}
LIMIT 10
~~~

# Warnings

When the results from a SPARQL endpoint are in XML/RDF without
 proper end tags, the following warnings will be given by
 the XML parser:

~~~{.txt}
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "uri"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "binding"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "result"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "results"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "sparql"
~~~

@author Wouter Beek
@see SPARQL 1.1 Recommendation 2013/03
     http://www.w3.org/TR/2013/REC-sparql11-overview-20130321/
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(codes_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(row_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_closure)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/sparql_client)).
:- use_module(math(math_ext)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

% OWL
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').

:- if(predicate_property(user:debug_mode, visible)).
  :-
    once(http_server_property(Port, _)),
    sparql_register_remote(localhost, localhost, Port, '/sparql/').
:- endif.



%! sparql_enqueue(
%!   +Remote:atom,
%!   +Query:atom,
%!   +Retries:or([oneof([inf]),positive_integer]),
%!   -VarNames:list,
%!   -Results:list
%! ) is det.
% @error =|existence_error(url,URL)|= with context
%        =|context(_, status(509, 'Bandwidth Limit Exceeded'))|=

sparql_enqueue(_, _, 0, [], []):- !.
sparql_enqueue(Remote, Query, Attempts1, VarNames, Results):-
  catch(
    sparql_query_no_catch(Remote, Query, VarNames, Results),
    E,
    (
      http_exception(E),
      count_down(Attempts1, Attempts2),
      sparql_enqueue(Remote, Query, Attempts2, VarNames, Results)
    )
  ).


%! sparql_query(
%!   +Remote:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Results:list
%! ) is det.
% Simply performs a SPARQL query (no additional options, closures).

sparql_query(Remote, Query1, VarNames, Results):-
  catch(
    sparql_query_no_catch(Remote, Query1, VarNames, Results),
    E,
    (http_exception(E), Results = [])
  ).

sparql_query_no_catch(Remote, Query1, VarNames, Results):-
  atomic_codes(Query2, Query1),
  debug(sparql_ext, '~w', [Query2]),
  once(sparql_current_remote(Remote, Host, Port, Path)),
  O1 = [host(Host),timeout(1),path(Path),variable_names(VarNames)],
  (
    Port == default
  ->
    O2 = O1
  ;
    merge_options([port(Port)], O1, O2)
  ),
  findall(
    Result,
    sparql_query(Query2, Result, O2),
    Results
  ).


%! sparql_query_sameas(
%!   +Remote:atom,
%!   +Resource:uri,
%!   -IdenticalResources:ordset
%! ) is det.
% @arg Remote The atomic name of a registered SPARQL remote.
% @arg Resource The URI of a resource.
% @arg IdenticalResources An ordered set of identical resources.

sparql_query_sameas(Remote, Resource, Resources2):-
  phrase(
    sparql_formulate(
      owl,
      _,
      [owl],
      select,
      true,
      [x],
      [rdf(iri(Resource), owl:sameAs, var(x))],
      inf,
      _,
      _
    ),
    Query
  ),
  sparql_query(Remote, Query, _, Rows),
  rows_to_resources(Rows, Resources1),
  ord_add_element(Resources1, Resource, Resources2).

