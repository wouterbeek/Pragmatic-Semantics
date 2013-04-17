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
@version 2013/04
*/

:- use_module(generic(print), [list/2 as print_list]).
:- use_module(rdf(rdf_namespace)).
:- use_module(sparql(sparql_ext)).

:- rdf_register_namespace(bag).
:- rdf_register_namespace(bags).

:- register_sparql_prefix(bag).
:- register_sparql_prefix(bags).

:- register_sparql_remote(bag, 'lod.geodan.nl', '/BAG/sparql').

:- debug(bag).



test:-
  formulate_sparql(
    [bags],
    'SELECT DISTINCT *',
    ['?s vocab:pand_status ?o'],
    10,
    Query
  ),
  enqueue_sparql(bag, Query, _VarNames, Resources),
  print_list(user, Resources).

