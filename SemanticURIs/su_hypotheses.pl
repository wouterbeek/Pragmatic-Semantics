:- module(su_hypotheses, []).

/** <module> SemanticURIs hypotheses

Hypothesis validation for the Semantic URIs project.

@author Wouter Beek
@version 2014/02, 2014/04
*/

:- rdf_meta(number_of(r,r)).
:- rdf_meta(number_of(r,r,?)).
:- rdf_meta(same_number_of(r,r,r)).

:- use_module(dcg(dcg_content)). % Meta-DCG.
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(math(math_ext)).
:- use_module(pl_web(html_pl_term)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).
:- use_module(su(su_db)).
:- use_module(xml(xml_namespace)).

http:location(su, root(su), []).
:- http_handler(su(hypotheses), su_hypotheses, []).

user:web_module('SemanticURIs Hypotheses', su_hypotheses).



su_hypotheses(_Request):-
  flag(hypothesis, _, 0),
  reply_html_page(
    app_style,
    title('Semantic URIs - Hypothesis validation'),
    \su_hypotheses
  ).

su_hypotheses -->
  {
    % Retrieve the graph to which the resources belong.
    once((
      rdfs_individual_of(Resource, ckan:'Resource'),
      rdf([graph_mode(no_index)], Resource, _, _, Graph)
    )),

    % The number of triples is the same.
    Label1 = 'All resources have the same number of triples.',
    aggregate_all(
      set(Resource1),
      (
        number_of(void:triples, Resource1),
        number_of(su:triples, Resource1)
      ),
      Resources1
    ),
    partition(
      same_number_of(void:triples, su:triples),
      Resources1,
      In1,
      Out1
    ),

    % The number of predicate terms / properties is the same.
    Label2 = 'All resources have the same number of predicate terms and/or properties.',
    aggregate_all(
      set(Resource2),
      (
        number_of(void:properties, Resource2),
        number_of(su:predicates, Resource2)
      ),
      Resources2
    ),
    partition(
      same_number_of(void:properties, su:predicates),
      Resources2,
      In2,
      Out2
    ),

    % The number of subject terms is the same.
    Label3 = 'All resources have the same number of subjects.',
    aggregate_all(
      set(Resource3),
      (
        number_of(void:distinctSubject, Resource3),
        number_of(su:subjects, Resource3)
      ),
      Resources3
    ),
    partition(
      same_number_of(void:distinctSubject, su:subjects),
      Resources3,
      In3,
      Out3
    ),
    
    % ...
    Label4 = 'Compression on randomized URIs is the same for root and boundary TBox.',
    aggregate_all(
      set(Resource4),
      (
        number_of(su:boundary_tbox_result, Resource4),
        number_of(su:rnd_boundary_tbox_result, Resource4)
      ),
      Resources4
    )
  },
  html(
    \html_hypotheses(
      Graph,
      [[Label1,In1,Out1],[Label2,In2,Out2],[Label3,In3,Out3]]
    )
  ).


html_hypotheses(_, []) --> [].
html_hypotheses(Graph, [[Label,In,Out]|T]) -->
  html_hypothesis(Graph, Label, In, Out),
  html_hypotheses(Graph, T).

html_hypothesis(Graph, Label, In, Out) -->
  {
    flag(hypothesis, Id, Id + 1),
    maplist(length, [In,Out], [InL,OutL]),
    sum_list([InL,OutL], Sum),
    div_zero(InL, Sum, Validated),
    div_zero(OutL, Sum, Refuted),
    findall([Resource], member(Resource, Out), Rows)
  },
  html([
    h1(['Hypothesis ',Id,' - ',Label]),
    p(['Validated: ',\html_pl_term(Validated)]),
    p(['Refuted: ',\html_pl_term(Refuted)]),
    \rdf_html_table(
      [graph(Graph),location(su_hypotheses)],
      html('Resources for which the hypothesis is refuted.'),
      [['Resource']|Rows]
    )
  ]).


number_of(P, Resource):-
  number_of(P, Resource, _).

number_of(P, Resource, N):-
  %rdfs_individual_of(Resource, ckan:'Resource'),
  rdf_datatype(Resource, P, N, xsd:integer, _),
  integer(N).


same_number_of(P1, P2, Resource):-
  maplist(rdf_global_id, [P1,P2], [PP1,PP2]),
  number_of(PP1, Resource, N1),
  number_of(PP2, Resource, N2),
  N1 == N2.

