:- module(
  html_article,
  [
    general_information//2, % :Title
                            % +Authors:list(iri)
    paragraph//1, % :Content
    section//2, % +Title:atom
                % :Content
    toc//1 % -TOC:dom
  ]
).

/** <module> Article

Predicates for generating a Web article.

@author Wouter Beek
@version 2013/11, 2014/01-2014/03
*/

:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(generics(atom_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(plp(dcg_c)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

:- html_meta(general_information(html,+,?,?)).
:- html_meta(paragraph(html,?,?)).
:- html_meta(section(+,html,?,?)).

:- rdf_meta(author(r,?,?)).
:- rdf_meta(authors(t,?,?)).
:- rdf_meta(general_information(+,t,?,?)).

:- dynamic(section/2).



author(A_) -->
  {
    rdf_global_id(A_, A),
    rdf_string(A, foaf:firstName, FirstName, G),
    rdf_string(A, foaf:lastName, LastName, G)
  },
  html(div(class=author, ['Author: ',FirstName,' ',LastName])).

authors(L) -->
  {is_list(L)}, !,
  html(div(class=authors, \authors_(L))).
authors(A) -->
  authors([A]).

authors_([]) --> !.
authors_([H|T]) -->
  author(H),
  authors_(T).

general_information(Title, Authors) -->
  html([h1(class=article_title,Title),\authors(Authors)]).

paragraph(Content) -->
  html(p(class=paragraph, Content)).

section(Title, Content) -->
  {
    once(dcg_phrase(c_name, Title, SectionID)),
    assert(section(SectionID, Title))
  },
  html([h1([class=section_title,id=SectionID], Title), Content]).

title(Title) -->
  html(h1(class=article_title, Title)).

toc([h2('Table of Contents'),ol(class=toc,Refs)]) -->
  {findall(
    li(a(href=Id2,Title)),
    (
      retract(section(Id1, Title)),
      atomic_concat('#', Id1, Id2)
    ),
    Refs
  )}.

