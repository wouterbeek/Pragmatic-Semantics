:- module(prasem, []).

/** <module> PraSem

Description of the NWO-funded project
Pragmatic Semantic for the Web of Data.

@author Stefan Schlobach
@author Wouter Beek
@version 2012, 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(html(html_image)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(server(app_ui)).
:- use_module(server(article)).
:- use_module(server(web_modules)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(prasem, 'http://xmlns.com/foaf/0.1/').

:- web_module_add('What is prasem?', prasem, prasem).

% /img
:- db_add_novel(http:location(img, root(img), [])).
:- db_add_novel(user:file_search_path(img, bk(img))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

:- http_handler(root(prasem), prasem, []).

:- initialization(init_prasem).



assert_foaf_stefan_schlobach(S, G):-
  rdf_global_id(prasem:'StefanSchlobach', S), 
  % foaf:firstName
  rdf_assert_literal(S, foaf:firstName, 'Stefan', G),
  % foaf:lastName
  rdf_assert_literal(S, foaf:lastName, 'Schlobach', G),
  % foaf:page
  assert_foaf_stefan_schlobach_www(S_WWW, G),
  rdf_assert(S, foaf:page, S_WWW, G),
  % foaf:mbox
  rdf_assert(S, foaf:mbox, 'mailto:k.s.schlobach@vu.nl', G),
  % rdf:type
  rdf_assert_individual(S, foaf:'Person', G),
  % rdfs:label
  rdfs_assert_label(S, nl, 'Stefan Schlobach', G).

assert_foaf_stefan_schlobach_www(S_WWW, G):-
  S_WWW = 'http://www.few.vu.nl/~schlobac/',
  % dc:title
  rdf_assert_literal(S_WWW, 'Stefan Schlobach\'s VU website.', en, G),
  % rdf:type
  rdf_assert_individual(S_WWW, foaf:'Document', G).

init_prasem:-
  G = prasem,
  assert_foaf_stefan_schlobach(_S, G).

prasem(_Request):-
  reply_html_page(app_style, [], \prasem_body).

prasem_body -->
  html(
    body([
      \general_information(
        \(prasem:title),
        prasem:'StefanSchlobach'
      ),
      \section('WHAT IS THE PROBLEM?', \(prasem:problem))
    ])
  ).

title -->
  html('Pragmatic Semantics for the Web of Data').

problem -->
  html(\paragraph(\(prasem:problem_p1))).

problem_p1 -->
  html(
    'The Web of Data (WOD) connects data in a similar way as the WWW connects documents. Atomic data-units called resources are connected via typed links with arbitrary resources anywhere on the Web, and together these RDF triples form a gigantic graph of linked data. The meaning of the types can be fixed using standardised schema and ontology languages such as RDFS and OWL. The semantics of these languages are based on logical paradigms that were designed for small and hand-made knowledge bases, and come with a classical model-theory assigning truth to formulae, and entailment based on this truth. In a highly complex, dynamic, context-dependent, opinionated, contradictory and multi-dimensional semantic network as the WOD, these Semantics are insufficient, as they are one-dimensional, often prone to logical fallacies, and usually intractable.'
  ).

