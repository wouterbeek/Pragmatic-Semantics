:- module(
  dbpedia_categories,
  [
    dbpedia_load_categories/0
  ]
).

/** <module> DBpedia categories

DBpedia publishes the following three kind of files about categories:
  * *|Articles Categories|*
    Links from concepts to categories using the SKOS vocabulary.
  * *|Categories (Labels)|*
    Labels for Categories.
  * *|Categories (Skos)|*
    Information which concept is a category and how categories are
    related using the SKOS Vocabulary.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(rdf_file(rdf_serial)).



%! dbpedia_load_categories is det.
% Loads the SKOS hierarchy of categories.

dbpedia_load_categories:-
  dbpedia_categories_url(Url),
  rdf_download_extract_load(Url, [graph(G)]),
  write(G).

dbpedia_categories_url(
  'http://downloads.dbpedia.org/3.9/en/skos_categories_en.ttl.bz2'
).

