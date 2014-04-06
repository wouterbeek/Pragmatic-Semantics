:- module(
  rdf_html_table,
  [
    rdf_html_table//2, % +Options:list(nvpair)
                       % +Table:iri
    rdf_html_table//3, % +Options:list(nvpair)
                       % :Caption
                       % +Rows:list(list(ground))
    rdf_html_tables//2 % +Options:list(nvpair)
                       % +Tables:list(iri)
  ]
).

/** <module> RDF HTML table

Generates HTML tables with RDF content.

@author Wouter Beek
@version 2014/01-2014/04
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(typecheck)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_table)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_web(rdf_term_html)).

:- meta_predicate(rdf_html_table(+,//,+,?,?)).

:- rdf_meta(rdf_html_table(+,r,?,?)).



%! header_row_preset(-HeaderName:atom)// .
% Grammar rules for singular header names.

header_row_preset('Graph')     --> `g`.
header_row_preset('Literal')   --> `l`.
header_row_preset('Object')    --> `o`.
header_row_preset('Predicate') --> `p`.
header_row_preset('Subject')   --> `s`.


%! header_row_presets(-HeaderNames:list(atom))// .
% Grammar rule for a list of header names.

header_row_presets([H|T]) -->
  header_row_preset(H),
  header_row_presets(T).
header_row_presets([]) --> [].


%! header_row_presets(
%!   +HeaderRowOption1:atom,
%!   +Rows1:list(list),
%!   -HeaderRowOption2:atom,
%!   -Rows2:list(list)
%! ) is det.

header_row_presets(X, Rows, X, Rows):-
  is_of_type(boolean, X), !.
header_row_presets(Abbr, T, true, [H|T]):-
  atom(Abbr), !,
  dcg_phrase(header_row_presets(H), Abbr).
header_row_presets(_, L, true, L).


% A location identifier is set.
location_option(O1, LocationId):-
  option(location(LocationId), O1), !.
% No location identifier is set: use the default location identifier.
location_option(_, rdf_tabular).


%! rdf_html_table(+Options:list(nvpair), +Table:iri)// is det.
% @see rdf_html_table//3 for the supported options.

rdf_html_table(O1, Table) -->
  {
    option(header_column(HasHeaderColumn), O1),
    option(header_row(HasHeaderRow), O1),
    rdf_string(Table, rdf_table:caption, Caption, _),
    rdf(Table, rdf_table:columns, Columns),
    rdf_list([], Columns, ColumnHeaders),
    rdf(Table, rdf_table:rows, Rows),
    rdf_list([], Rows, RowHeaders),
    rdf_table_get_rows(
      Table,
      HasHeaderColumn,
      ColumnHeaders,
      RowHeaders,
      Rows1
    ),
    (
      HasHeaderRow == true
    ->
      Rows2 = [ColumnHeaders|Rows1]
    ;
      Rows2 = Rows1
    ),
    location_option(O1, LocationId)
  },
  rdf_html_table(O1, rdf_term_html(LocationId, Caption), Rows2).


%! rdf_table_get_rows(
%!   +Table:iri,
%!   +HadHeaderColumn:boolean,
%!   +ColumnHeaders:list(iri),
%!   +RowHeaders:list(iri),
%!   -Rows:list(list(ground))
%! ) is det.

rdf_table_get_rows(_, _, _, [], []):- !.
rdf_table_get_rows(
  Table,
  HasHeaderColumn,
  ColumnHeaders,
  [RowHeader|RowHeaders],
  [Row2|Rows]
):-
  rdf_table_get_row(Table, ColumnHeaders, RowHeader, Row1),
  (
    HasHeaderColumn == true
  ->
    Row2 = [RowHeader|Row1]
  ;
    Row2 = Row1
  ),
  rdf_table_get_rows(Table, HasHeaderColumn, ColumnHeaders, RowHeaders, Rows).


%! rdf_table_get_row(
%!   +Table:iri,
%!   +ColumnHeaders:list(iri),
%!   +RowHeader:iri,
%!   -Row:list(ground)
%! ) is det.

rdf_table_get_row(_, [], _, []):- !.
rdf_table_get_row(Table, [ColumnHeader|ColumnHeaders], RowHeader, [H|T]):-
  rdf(Table, rdf_table:cell, Cell),
  rdf_string(Cell, rdf_table:column, ColumnHeader, _),
  rdf_string(Cell, rdf_table:row, RowHeader, _),
  rdf(Cell, rdf:value, H), !,
  rdf_table_get_row(Table, ColumnHeaders, RowHeader, T).


%! rdf_html_table(
%!   +Options:list(nvpair),
%!   :Caption,
%!   +Rows:list(list(ground))
%! )// is det.
% The following options are supported:
%   * =|graph(+RdfGraph:atom)|=
%     The RDF graph that is used for retrieving resources via hyperlinks.
%   * =|header_row(+or([atom,boolean]))|=
%     Support for often occurring header rows.
%     A boolean is passed on to html_table//4.
%     Often occurring header rows are atoms that consist of
%     the here enumerated characters,
%     where the characters correspond to columns,
%     in the order in which they occur.
%   * =|location(Location:iri)|=
%   * Other options are handed to html_table//4.
%
% The following characters are supported for the header row option:
%   | `g` | Graph     |
%   | `l` | Literal   |
%   | `o` | Object    |
%   | `p` | Predicate |
%   | `s` | Subject   |

% Do not fail for empty data lists.
rdf_html_table(_, _, []) --> !, [].
rdf_html_table(O1, Caption, Rows1) -->
  {
    % Retrieve the RDF graph relative to which hyperlinks work, if any.
    select_option(graph(Graph), O1, O2, _NoGraph),

    % See whether a header row should be added.
    select_option(header_row(HeaderRow1), O2, O3, false),
    header_row_presets(HeaderRow1, Rows1, HeaderRow2, Rows2),
    merge_options([header_row(HeaderRow2)], O3, O4),

    location_option(O1, LocationId)
  },
  html(
    \html_table(
      O4,
      Caption,
      %%%%dcg_nth0_call([minus(true)], rdf_term_html(LocationId, Graph), 0),
      rdf_term_html_dummy(LocationId, Graph),
      Rows2
    )
  ).
rdf_term_html_dummy(LocationId, Graph, RdfTerm) -->
  rdf_term_html(LocationId, RdfTerm, Graph).

rdf_html_tables(_, []) --> !, [].
rdf_html_tables(O1, [H|T]) -->
  rdf_html_table(O1, H),
  rdf_html_tables(O1, T).

