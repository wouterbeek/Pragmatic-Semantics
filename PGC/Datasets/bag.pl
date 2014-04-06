:- module(bag,
  [
    test/0
  ]
).

/** <module> BAG

The Dutch base registration for buildings.

http://lod.geodan.nl/BAG/sparql?query=SELECT+DISTINCT+*+WHERE+{%0D%0A++%3Fs+%3Fp+%3Fo%0D%0A}%0D%0ALIMIT+1

info@geodan.nl

@author Wouter Beek
@version 2013/04, 2013/12-2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(bag, 'http://lod.geodan.nl/BAG/').
:- sparql_register_remote(bag, 'lod.geodan.nl', default, '/BAG/sparql').

:- xml_register_namespace(bags, 'http://lod.geodan.nl/BAG-schema/').



test:-
  phrase(
    sparql_formulate(
      _,
      _,
      [bags],
      select,
      true,
      [],
      [rdf(var(s), vocab:pand_status, var(o))],
      10,
      _,
      _
    ),
    Query
  ),
  sparql_query(bag, Query, _VarNames, Resources),
  dcg_with_output_to(current_user, list(pl_term, Resources)).

