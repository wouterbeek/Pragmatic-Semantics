:- module(rdf_online, []).

/** <module> RDF online

Web-based tools for loading/saving/searching RDF.

@tbd This is not working yet.
@version 2012/12-2013/01, 2013/03-2013/05, 2013/09, 2013/11-2014/01, 2014/03
*/

:- use_remote_module(dcg(dcg_generic)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_term(rdf_term)).



rdf_online(_Request):-
  reply_html_page(app_style, title('RDF filter'), \rdf_online).


rdf_online -->
  html([
    % Load RDF.
    h1('Load'),
    rdf_load,
    
    % Filter triples.
    h1('Filter'),
    rdf_filter
  ]).


rdf_filter -->
  {
    once(rdf_graph(G)), !,
    aggregate_all(
      set(option(value=SLabel,SLabel)),
      (
        rdf_subject(STerm, G),
        dcg_with_output_to(atom(SLabel), rdf_term_name(STerm))
      ),
      SItems
    ),
    aggregate_all(
      set(option(value=PLabel,PLabel)),
      (
        rdf_predicate(PTerm, G),
        dcg_with_output_to(atom(PLabel), rdf_term_name(PTerm))
      ),
      PItems
    ),
    aggregate_all(
      set(option(value=OLabel,OLabel)),
      (
        rdf_object(OTerm, G),
        dcg_with_output_to(atom(OLabel), rdf_term_name(OTerm))
      ),
      OItems
    )
  },
  rdf_filter_form(SItems, PItems, OItems).


rdf_filter_form(SItems, PItems, OItems) -->
  html(
    form([class='pure-form',id=explain_rdf_triple], [
      fieldset(class='pure-group', [
        input([
          class='pure-input-1-2',
          id=rdf_subject_input,
          list=rdf_subjects,
          placeholder='Subject term',
          type=text
        ]),
        datalist(id=rdf_subjects, SItems),
        input([
          class='pure-input-1-2',
          id=rdf_predicate_input,
          list=rdf_predicates,
          placeholder='Predicate term',
          type=text
        ]),
        datalist(id=rdf_predicates, PItems),
        input([
          class='pure-input-1-2',
          id=rdf_object_input,
          list=rdf_objects,
          placeholder='Object term',
          type=text
        ]),
        datalist(id=rdf_objects, OItems),
        button([
          class=['pure-button','pure-input-1-2','pure-button-primary'],
          type=submit
        ], 'Sign in')
      ])
    ])
  ).


rdf_load -->
  rdf_load_form.

rdf_load_form -->
  html(
    form([class='pure-form',id='rdf-load'], [
      fieldset(class='pure-group', [
        input([
          class='pure-input-1-2',
          id='rdf-file',
          type=file
        ])
      ])
    ])
  ).

