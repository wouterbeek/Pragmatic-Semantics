:- module(prasem, []).

/** <module> PraSem

Description of the NWO-funded project
Pragmatic Semantic for the Web of Data.

@author Stefan Schlobach
@author Wouter Beek
@version 2013/11, 2014/03
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_remote_module(html(html_article)).
:- use_remote_module(html(html_image)).
:- use_remote_module(rdf(rdf_build)).
:- use_remote_module(rdf_term(rdf_language_tagged_string)).
:- use_remote_module(rdf_term(rdf_literal)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(server(app_ui)). % Uses the default application style.
:- use_remote_module(server(web_modules)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(prasem, 'http://www.wouterbeek.com/prasem#').

% /css
http:location(css, root(css), []).
user:file_search_path(css, server(css)).
:- http_handler(css(.), serve_files_in_directory(css), [prefix,priority(10)]).
:- html_resource(css('image.css'), []).

% /img
user:file_search_path(img, prasem(img)).
:- http_handler(root(prasem), prasem, []).
user:web_module('What is prasem?', prasem).

:- initialization(init_prasem).



assert_foaf_stefan_schlobach(S, G):-
  rdf_global_id(prasem:'StefanSchlobach', S),
  % foaf:firstName
  rdf_assert_string(S, foaf:firstName, 'Stefan', G),
  % foaf:lastName
  rdf_assert_string(S, foaf:lastName, 'Schlobach', G),
  % foaf:page
  assert_foaf_stefan_schlobach_www(S_WWW, G),
  rdf_assert(S, foaf:page, S_WWW, G),
  % foaf:mbox
  rdf_assert(S, foaf:mbox, 'mailto:k.s.schlobach@vu.nl', G),
  % rdf:type
  rdf_assert_individual(S, foaf:'Person', G),
  % rdfs:label
  rdfs_assert_label(S, 'Stefan Schlobach', nl, G).

assert_foaf_stefan_schlobach_www(S_WWW, G):-
  S_WWW = 'http://www.few.vu.nl/~schlobac/',
  % dc:title
  rdf_assert_language_tagged_string(S_WWW, dc:title,
      'Stefan Schlobach\'s VU website.', en, G),
  % rdf:type
  rdf_assert_individual(S_WWW, foaf:'Document', G).

init_prasem:-
  G = prasem,
  assert_foaf_stefan_schlobach(_S, G).

prasem(_Request):-
  reply_html_page(app_style, \prasem_head, \prasem_body).

prasem_body -->
  html(
    body([
      div(style='float: right;',
          \html_image_thumbnail_box([], '', _, _, 'prasem.jpg')
      ),
      \general_information(
        html('Pragmatic Semantics for the Web of Data'),
        prasem:'StefanSchlobach'
      ),
      \html_receive(toc),
      \section('What is the problem?', \(prasem:paragraphs(10))),
      \section('Scientific Topics and Research Questions',
          \(prasem:paragraphs(20))
      ),
      \section('Pragmatic Semantics', \(prasem:paragraphs(30))),
      \section('Calculi for Pragmatic Semantics', \(prasem:paragraphs(40))),
      \section('Research Goal and Questions', \(prasem:paragraph_50)),
      \toc(TOC),
      \html_post(toc, TOC)
    ])
  ).

prasem_head -->
  html([
    title('What is PraSem?'),
    \html_requires(css('image.css'))
  ]).

paragraph_50 -->
  html([
    p('The idea of Pragmatic Semantics is to allow users to integrate different levels of knowledge and abstraction directly and explicitly into the semantics of their ontology. This has implications that are not yet well understood, both from theoretical and an application perspective. The main goal of the research is thus threefold:'),
    ol([
      li(['To investigate the theoretical properties of pragmatic semantics in general, and a number of application-specific instantiations, including formal (such as monotonicity, completeness, decidability etc) and computational properties. ', b('(Properties of the Semantics)')]),
      li(['To assess whether the proposed formalisms are effective solutions to the semantic interoperability problem on the WOD for a selected number of practical scenarios. ', b('(Quality of the Semantics)')]),
      li(['To investigate whether we can build efficient (approximate) calculi for a number of instantiations of pragmatic semantics. ', b('(Practical applicability of the Semantics)')])
    ]),
    p('Scientific question: To understand, define and evaluate pragmatic semantics as a proposed semantic formalism for the Web of Data?')
  ]).

paragraphs(S) -->
  {
    integer(S), !,
    findall(
      P-C,
      p_(S, P, C),
      P1
    ),
    keysort(P1, P2),
    pairs_values(P2, L)
  },
  paragraphs(L).
paragraphs([]) --> !.
paragraphs([H|T]) -->
  paragraph(H),
  paragraphs(T).

p_(10, 10, 'The Web of Data (WOD) connects data in a similar way as the WWW connects documents. Atomic data-units called resources are connected via typed links with arbitrary resources anywhere on the Web, and together these RDF triples form a gigantic graph of linked data. The meaning of the types can be fixed using standardised schema and ontology languages such as RDFS and OWL. The semantics of these languages are based on logical paradigms that were designed for small and hand-made knowledge bases, and come with a classical model-theory assigning truth to formulae, and entailment based on this truth. In a highly complex, dynamic, context-dependent, opinionated, contradictory and multi-dimensional semantic network as the WOD, these Semantics are insufficient, as they are one-dimensional, often prone to logical fallacies, and usually intractable.').
p_(10, 20, 'Two simple scenarios will illustrate some of the high-level problems. Scenario 1 is about heterogeneous publishing of data. Many libraries describe their books with controlled vocabularies. Linking collections and those vocabularies to the Linked-Open Data cloud, a collection of hundreds of interconnected data-graphs on the WOD, has huge benefits for libraries as search becomes more powerful, and metadata of documents is automatically enriched. Suppose a library in China annotates a book about Amsterdam with a concept ch:SmallTown. The Dutch National Library, on the other hand, annotates the same book with subject nl:BigCity. What happens now when the two libraries add their vocabularies and data to the WOD? What should be the desired answer to a query for big cities? Linking the libraries’ vocabularies to the Linked-Open Data cloud will lead to conflicts and hamper access to the document in question rather than support it.').
p_(10, 30, 'Scenario 2 is about opinionated interpretation of data, and is taken from Scientometrics, the Science of measuring and predicting Science. Scientometric researcher often use the Web as a proxy for studying science itself. Scientists leave online traces while doing research and a lot of this data is structured and part of the Web of Data. In some way, the WOD becomes a magnifying glass to measure activity in Science. The problem, however, is that multiple views are omnipresent: research blogs are biased, there are networks of publications of different impact levels, social networks that overshadow reliable analysis, which all comes on top of the usual technical problems of instance- unification, homonymy and synonymy. Modelling this highly complex Science Web with standard ontology languages is impossible as long as standard semantics are enforced.').
p_(10, 40, 'On the Web in general, and the Web of Data in particular, almost every bit of information is context-dependent, biased towards a particular viewpoint, opinionated, dated, uncertain or vague. The WOD is a market-place of ideas, not a database, and has to be dealt with accordingly. As making the representational languages more complex is not an option, we have to adapt the formal semantics of existing formalisms to the new requirements.').
p_(10, 50, 'The goal of PraSem is to introduce, and critically analyse, a new semantic paradigm that is appropriate for the complex, and inherently inconsistent, noisy and multi-dimensional nature of the Web of Data.').
p_(20, 10, 'PraSem will introduce and assess the potential of novel semantics that can help overcome the weakness of traditional semantics when dealing with a messy, multi-dimensional, contextualised and complex knowledge structure such as the Web of Data.').
p_(20, 20, 'Consider a prototypical example: a Dutch dataset describes European cities, among them Amsterdam, which is a capital and does not require a visa for travel. In good practice the resources are linked to existing sources, e.g. DBpedia, by an owl:sameAs predicate. Similar data is published in China (using the namespace ch:), but now for European cities a visa is required. Both pieces of information are locally correct and the linking follows the correct principles. Still, considering the two classes ch:VisumNeeded and nl:VisumFreeCity to be disjoint, classical semantics collapse, and even useful derivable information, such as the fact that Amsterdam is a city with an airport, as it is a capital city, is lost. To address this issue, we need to deal with truth at different contexts.').
p_(20, 30, 'Recent approaches to extend current methods, e.g., with quantitative information about vagueness and uncertainty, or towards multi-dimensionality are highly useful for specific applications. But they necessarily fall short of representing the full rich of the Web of Data. They also fail on a second, critical, requirement for our new semantics: as current ontology languages are already perceived as being too complex for practical use, the burden cannot be put on the modeller. We need to adapt the semantics, not the languages.').
p_(20, 40, 'More concretely, we consider ontologies to be sets of RDF(S)/OWL triples. Without loss of generality an interpretation consists of a domain and interpretation function, assiging individuals to objects, concepts and classes to subsets of this domain, and properties and roles to binary relations, and which extended over the operators of the underlying representation language. Models are interpretations satisfying all the axioms. Axioms or triples are then classically entailed by the ontology if they are satisfied in all its models. Unfortunately, even in our simple example things go wrong as there cannot be a model where the instance dbp:Amsterdam is both visa-free and a city where a visa is required. By definition, everything is entailed: the semantics becomes useless.').
p_(30, 10, 'Pragmatic semantics integrate different world-views instead of defining meaning with respect to a single one. The idea is to make as much information in the data explicit, and turn it into first-class semantics citizens. First, it allows integrating classic model-theoretic notions of truth with explicit knowledge about the structure of the knowledge base. But also semantic meta-data, such as popularity, scarcity, abnormality, etc., and even background knowledge from other sources, can be integrated.').
p_(30, 20, 'Most of this additional knowledge induces some kind of ordering on the formulas, which we will call truth orderings. A simple example of such a truth ordering is the one induced by the size of the minimal subontology classically entailing a formula. Other examples are orderings derived as the ratio of sub-models (models for parts of an ontology) in which a formula is satisfied versus the total number of sub-models, or the ratio between sub-ontologies of O in which a formula holds holds versus the number of all sub-ontologies are interesting candidates. Those are orderings based on subset of the ontology, a well-known class often used when dealing with inconsistent ontologies [44].').
p_(30, 30, 'Another relevant class are orderings induced by the graph properties of the ontology, in case that the underlying data-model is graph-based. A shortest path ordering can be determined as the inverse of the longest shortest distance between all nodes in the ontology (diameter of the induced sub-graphs). Such a notion is a proxy for confidence of derivation. Other graph-based measures, e.g. based on random-walk distance or edge-weights, induce orderings that are clustering-aware, with sub-ontologies entailing a formula have more cohesion than others. Finally, taking node properties such as PageRank into account, orderings can be used as proxies for popularity.').
p_(30, 40, 'While those two classes of orderings make structural properties of the ontology explicit and use them to implicitly contextualise meaning, others are based on external information outside the ontology itself. Examples for this are the Google count and similarity [30] based on frequency of labels of resources on the WWW.').
p_(30, 50, 'The different orderings cover different aspects of the “true” semantics of the Web of Data. To combine those aspects pragmatic entailment is defined through multi-objective optimisation. A pragmatic closure C for an ontology O and orderings f1 to fn is then a set of formulas that is Pareto-optimal [45] w.r.t. the optimisation problem max[f1 (C),…,fn (C)].').
p_(30, 60, 'Interoperability is then achieved by enriching an ontology with meta-information about semantic orderings, as well as agreement on the weighting of orderings. As there are possibly several pragmatic closures (different solutions on the Pareto-front) also agreement on the weighting of features is required. We will refer to the entailment induced by a given set of orderings as an instantiation of the family of pragmatic semantics.').
p_(40, 10, 'Another way of looking at it is that the Web of Data is a Complex System [28], with interlinked information at different scales of abstraction. A well-argued claim in the Complex Systems literature suggests that it is impossible to construct logical systems that capture the full meaning of a true Complex System [35]. Results from studying the Web of Data as a Complex System show that considering different scales and levels of interactions make it impossible to engineer a web-scale reasoner (whatever the semantics considered), as traditional, decomposition-based approaches, are doomed with bandwidth limitations between the coordinating components (i.e. the datasets). Traditional semantics deal with this problem by an intrinsic reduction of the complexity: only one world-view, one perspective is considered at the time, the Web of Data is seen as a database. With pragmatic semantics, this advantage gets lost, and the computational price has to be paid, which applies that classical top-down reasoning becomes impossible.').
p_(40, 20, 'It is often claimed that such systems have to evolve according to biological evolution rules [36], and web-scale semantics and reasoning should emerge from controlled interactions between autonomous components. In [37] we introduced such a calculus based on swarm intelligence where instead of indexing all triples and joining the results, swarms of lightweight agents (so-called boids) autonomously traverse the graph, each representing a reasoning rule, which might be (partially) instantiated. Whenever the conditions of a rule match the node a boid is on, it locally adds the new derived triple. This provides an index-free alternative for reasoning over large distributed dynamic networks of RDF(S) graphs. It calculates the pragmatic closure under the condition of maximising popularity of nodes (as random walks of boids simulate PageRank calculation) and minimizing the length of sub-ontologies, two particular truth orderings. Not all of the conceivable calculi for pragmatic semantics have to be inspired by Computational Intelligence approaches, but PraSem will focus on this family of algorithms.').

