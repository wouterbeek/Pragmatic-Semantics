:- module(dbpedia, []).

/** <module> DBpedia

Dedicated DBpedia queries using SPARQL.

### Examples

Query for a resource:
~~~{.sparql}
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX umbel: <http://umbel.org/umbel/rc/>
SELECT DISTINCT ?s
WHERE {
  ?s rdf:type umbel:Writer .
  ?s rdfs:label ?label .
  FILTER regex(?label, "Queneau", "i")
}
LIMIT 1
OFFSET 0
~~~

Retrieve all known facts about a query result:
~~~{.sparql}
PREFIX dbpedia: <http://dbpedia.org/resource/>
SELECT ?p ?o
WHERE
{
  dbpedia.org:Raymond_Queneau ?p ?o .
}
~~~

@author Wouter Beek
@version 2013/03-2013/05, 2013/08, 2013/12-2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(ttl, turtle)).



% XML and SPARQL namespace prefixes that often occur in DBpedia.

:- xml_register_namespace(dbo, 'http://dbpedia.org/ontology/').

:- xml_register_namespace(dt, 'http://dbpedia.org/datatype/').

:- xml_register_namespace(dbp, 'http://dbpedia.org/property/').

:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- sparql_register_remote(dbpedia, 'dbpedia.org', default, '/sparql').

:- xml_register_namespace(fb, 'http://rdf.freebase.com/ns/').

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

:- xml_register_namespace('powder-s', 'http://www.w3.org/2007/05/powder-s#').

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- xml_register_namespace(umbel, 'http://umbel.org/umbel/rc/').

:- xml_register_namespace(yago, 'http://yago-knowledge.org/resource/').

