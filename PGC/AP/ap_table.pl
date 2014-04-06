:- module(
  ap_table,
  [
    ap_table/2 % :AugmentHeader
               % :AugmentRow
  ]
).

/** <module> AP table

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_remote_module(ap(ap_db)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(generics(uri_ext)).
:- use_remote_module(html(html_list)).
:- use_remote_module(html(html_table)).
:- use_remote_module(html(html_tuple)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(pl_web(html_pl_term)).
:- use_remote_module(rdf(rdf_container)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(rdf(rdf_list)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf_term(rdf_string)).
:- use_remote_module(rdf_web(rdf_html_table)).
:- use_remote_module(rdf_web(rdf_term_html)).
:- use_remote_module(server(app_ui)).
:- use_remote_module(server(web_modules)).

http:location(ap, root(ap), []).
:- http_handler(ap(table), ap_table, []).

user:web_module('AP table', ap_table).



ap_table(_Request):-
  ap_table(=, =).

%! ap_table(:HeaderAugmentation, :RowAugmentation) is det.
% Generates the HTML table that gives an overview of the results of
%  running automated processes.
%
% By default this creates a column for each AP stage.
% Sometimes we want to put in extra columns that do not correspond to
%  AP stages, e.g. displaying the name of a dataset
%  or some other meta-information.
%
% @arg HeaderAugmentation Binary predicate that adds cells to the header.
% @arg RowAugmentation Binary predicate that adds cells to each row.

:- meta_predicate(ap_table(2,2)).
ap_table(HeaderAugmentation, RowAugmentation):-
  (
    % We assume there is one AP collection we want to show,
    %  and we assume this is the first AP collection
    %  we happen to come accross.
    once(rdfs_individual_of(AP_Collection, ap:'AP-Collection')),
    rdf_collection(AP_Collection, APs, ap)
  ->
    maplist(ap_row, APs, Rows),
    reply_html_page(
      app_style,
      title([
        'Automated Processes - Collection ',
        \rdf_term_name(AP_Collection)
      ]),
      \ap_table(HeaderAugmentation, RowAugmentation, Rows)
    )
  ;
    reply_html_page(app_style, title('Automated Processes'), [])
  ).


%! ap_table(:HeaderAugmentation, :RowAugmentation, +Rows:list(list)) is det.
% HTML DSL DCG generating the AP table.

:- meta_predicate(ap_table(2,2,+,?,?)).
ap_table(_, _, []) --> !, [].
ap_table(HeaderAugmentation, RowAugmentation, [H|T]) -->
  {
    % The header is based on the first data row.
    ap_table_header(H, Header1),
    call(HeaderAugmentation, Header1, Header2),
    maplist(RowAugmentation, [H|T], Rows)
  },
  html(
    \html_table(
      [header_row(true),indexed(true)],
      html('Automated processes'),
      ap_stage_cell,
      [Header2|Rows]
    )
  ).

ap_row(AP, AP_Stages):-
  rdf_collection(AP, AP_Stages, ap).

ap_table_header(Row, Header):-
  maplist(ap_stage_name, Row, Header).

ap_stage_cell(ApStage) -->
  {
    atom(ApStage),
    rdfs_individual_of(ApStage, ap:'AP-Stage'), !,
    rdf_string(ApStage, ap:status, Class, ap)
  },
  html(div(class=Class, \ap_message(ApStage))).
% This covers both RDF terms and Prolog terms.
ap_stage_cell(Term) -->
  rdf_term_html(rdf_tabular, Term).


ap_message(ApStage) -->
  {rdfs_individual_of(ApStage, ap:'NeverReached')}, !,
  html('Never reached').
ap_message(ApStage) -->
  {
    rdfs_individual_of(ApStage, ap:'Error'), !,
    rdf_string(ApStage, ap:error, Atom, ap),
    read_term_from_atom(Atom, Error, [])
  },
  html(\html_pl_term(Error)).
ap_message(ApStage) -->
  {rdfs_individual_of(ApStage, ap:'Skip')}, !,
  html('Skip').
ap_message(ApStage) -->
  {
    rdfs_individual_of(ApStage, ap:'FileOperation'), !,
    rdf_string(ApStage, ap:file, File, ap),
    rdf_string(ApStage, ap:operation, Operation, ap)
  },
  ({
    findall(
      Modifier,
      rdf_string(ApStage, ap:has_modifier, Modifier, ap),
      Modifiers
    ),
    Modifiers \== []
  }->
    html(\html_list([], Modifiers))
  ;
    html([\operation(Operation),'@',\html_file(File)])
  ).
ap_message(ApStage) -->
  {
    rdfs_individual_of(ApStage, ap:'FileProperties'), !,
    findall(
      Name-Value,
      (
        rdf(ApStage, ap:has_property, NVPair, ap),
        rdf_string(NVPair, ap:name, Name, ap),
        rdf_string(NVPair, ap:value, Value, ap)
      ),
      NVPairs1
    ),
    keysort(NVPairs1, NVPairs2)
  },
  html(\html_pairs(html_pl_term, NVPairs2)).
ap_message(ApStage) -->
  {
    rdfs_individual_of(ApStage, ap:'Tables'), !,
    findall(
      Table,
      rdf(ApStage, ap:table, Table, ap),
      Tables
    )
  },
  html(\rdf_html_tables([header_column(true),header_row(true)], Tables)).
ap_message(ApStage) -->
  {
    rdfs_individual_of(ApStage, ap:'Filter'), !,
    rdf_string(ApStage, ap:name, Name, ap)
  },
  html(Name).
ap_message(ApStage) -->
  {
    forall(
      rdf(ApStage, P, O),
      (
        dcg_with_output_to(user_output, rdf_triple_name(ApStage, P, O)),
        nl(user_output),
        flush_output(user_output)
      )
    )
  }.

operation(Operation) -->
  html(span(class=operation, Operation)).

