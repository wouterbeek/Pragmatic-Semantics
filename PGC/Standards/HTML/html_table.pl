:- module(
  html_table,
  [
    html_table//3, % :Options:list(nvpair)
                   % :Caption
                   % +Rows:list(list(ground))
    html_table//4 % :Options:list(nvpair)
                  % :Caption
                  % :Cell
                  % +Rows:list(list(ground))
  ]
).

/** <module> HTML tables

Support for generating HTML tables based on Prolog lists.
Rows are represented by Prolog lists.
Cell contents are represented by Prolog ground terms that are elements
 inside the list.

@author Wouter Beek
@version 2012/09-2013/06, 2013/09-2014/01
*/

:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_meta)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_remote_module(pl_web(html_pl_term)).

:- meta_predicate(html_table(:,//,+,?,?)).
:- meta_predicate(html_table(:,//,3,+,?,?)).
:- meta_predicate(html_table_caption(//,?,?)).
:- meta_predicate(html_table_cells(+,3,+,?,?)).
:- meta_predicate(html_table_cell(+,3,+,?,?)).
:- meta_predicate(html_table_data_rows(+,+,1,3,+,?,?)).
:- meta_predicate(html_table_data_row(+,+,1,3,+,?,?)).
:- meta_predicate(html_table_header(+,+,+,3,+,-,?,?)).
:- meta_predicate(html_table_header_row(3,+,?,?)).
:- meta_predicate(html_table_index_cell(+,+,3,+,?,?)).



%! html_table(+Options:list(nvpair), :Caption, +Rows:list(list(ground)))// is det.
%! html_table(+Options:list(nvpair), :Caption, :Cell, +Rows:list(list(ground)))// is det.
% Generates the HTML markup for a table.
%
% The following options are supported:
%   1. =|header_column(boolean)|=
%      Uses `th` tags for cells in the first column.
%      Default: `false`.
%   2. =|header_row(boolean)|=
%      Whether or not the first row should be
%      displayed as the table header row.
%      Default is `false`.
%   3. =|highlighted_row(:HighlightedRow)|=
%      A semidet predicate term that is missing its last parameter.
%      Default: `false` for no row highlighted.
%   4. =|indexed(+Indexed:boolean)|=
%      Whether or not each row should begin with a row index.
%      Counts starts at 0. The header row, if included, is not counted.
%      Default is `false`.

html_table(O1, Caption, Rows) -->
  html_table(O1, Caption, html_pl_term, Rows).

is_meta(highlighted_row).
html_table(O1, Caption, Cell, Rows) -->
  {
    flag(table_row, _, 0),
    meta_options(is_meta, O1, O2),
    option(header_column(HasHeaderColumn), O2, false),
    option(header_row(HasHeaderRow), O2, false),
    option(highlighted_row(HighlightedRow), O2, fail),
    option(indexed(IsIndexed), O2, false)
  },
  html(
    table(class=['pure-table','pure-table-bordered'], [
      \html_table_caption(Caption),
      \html_table_header(
        HasHeaderColumn,
        HasHeaderRow,
        IsIndexed,
        Cell,
        Rows,
        DataRows
      ),
      tbody(
        \html_table_data_rows(
          HasHeaderColumn,
          IsIndexed,
          HighlightedRow,
          Cell,
          DataRows
        )
      )
    ])
  ).
fail(_):-
  fail.



% CAPTION %

%! html_table_caption(:Caption)// is det.
% Generates the HTML table caption,
% where the content of the caption element is set by a DCG rule.
%
% @arg Caption A DCG rule generating the content of the caption element,
%      or uninstantiated, in which case no caption is generated at all.

html_table_caption(VAR) -->
  {var(VAR)}, !,
  [].
html_table_caption(Caption) -->
  html(caption(\dcg_call(Caption))).



% CELL %

%! html_table_cells(
%!   +Type:oneof([data,header]),
%!   :Cell,
%!   +Elements:list(ground)
%! )// is det.

html_table_cells(Type, Cell, [H|T]) -->
  html_table_cell(Type, Cell, H),
  html_table_cells(Type, Cell, T).
html_table_cells(_, _, []) --> [].


%! html_table_cell(
%!   +Type:oneof([data,header]),
%!   :Cell,
%!   +Element:ground
%! )// is det.
% Generated an the content for an HTML cell (both header and data).
%
% ### Matching arguments that are pairs
%
% First we try to call the `Cell` DCG on the `H` argument.
% `H` may be a pair of the form `X-Y`.
%
% If generating the cell for `H` failed,
% we interpret the first element in the pair as an options list
% (i.e. HTML attributes) to the list item tag.

html_table_cell(data, Cell, Element) -->
  html(td(\dcg_call(Cell, Element))), !.
html_table_cell(data, Cell, O1-Element) -->
  {is_list(O1)}, !,
  html(td(O1, \dcg_call(Cell, Element))).
html_table_cell(header, Cell, Element) -->
  html(th(\dcg_call(Cell, Element))), !.
html_table_cell(header, Cell, O1-Element) --> !,
  {is_list(O1)}, !,
  html(th(O1, \dcg_call(Cell, Element))).



% DATA %

%! html_table_data_rows(
%!   +HasHeaderColumn:boolean,
%!   +IsIndexed:boolean,
%!   :Highlighted,
%!   :Cell,
%!   +DataRows
%! )// is det.

html_table_data_rows(_, _, _, _, []) --> !, [].
html_table_data_rows(HasHeaderColumn, IsIndexed, Highlighted, Cell, [H|T]) -->
  html_table_data_row(HasHeaderColumn, IsIndexed, Highlighted, Cell, H),
  html_table_data_rows(HasHeaderColumn, IsIndexed, Highlighted, Cell, T).


%! html_table_data_row(
%!   +HasHeaderColumn:boolean,
%!   +IsIndexed:boolean,
%!   :Highlighted,
%!   :Cell,
%!   +DataRows:list(list(ground))
%! )// is det.

html_table_data_row(
  HasHeaderColumn,
  IsIndexed,
  Highlighted,
  Cell,
  DataRow
) -->
  % Set whether the row is highlighted or not.
  {
    flag(table_row, RowNumber, RowNumber + 1),
    (
      call(Highlighted, RowNumber)
    ->
      O1 = [class='pure-table-odd']
    ;
      O1 = []
    )
  },

  ({
    HasHeaderColumn == true,
    IsIndexed == false,
    DataRow = [HeaderCell|DataRow0]
  }->
    html(
      tr(O1, [
        \html_table_cell(header, Cell, HeaderCell),
        \html_table_cells(data, Cell, DataRow0)
      ])
    )
  ;
    html(
      tr(O1, [
        \html_table_index_cell(HasHeaderColumn, IsIndexed, Cell, RowNumber),
        \html_table_cells(data, Cell, DataRow)
      ])
    )
  ).



% HEADER %

%! html_table_header(
%!   +HasHeaderColumn:boolean,
%!   +HasHeaderRow:boolean,
%!   +IsIndexed:boolean,
%!   :Cell,
%!   +Rows:list(list(ground)),
%!   -DataRows:list(list(ground))
%! )// is det.

% The options state that there should be a header row, but there is no
% content to display.
html_table_header(_,true, _, _, [], []) --> !, [].
% Options state a header row should be included.
% We take the first row, and return the other rows for later processing.
% Only add a header if the corresponding option says so.
html_table_header(
  HasHeaderColumn,
  true,
  IsIndexed,
  Cell,
  [HeaderRow1|DataRows],
  DataRows
) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(
    IsIndexed == true
  ->
    HeaderRow2 = ['#'|HeaderRow1]
  ;
    HasHeaderColumn == true
  ->
    HeaderRow2 = ['\\'|HeaderRow1]
  ;
    HeaderRow2 = HeaderRow1
  )},
  html(thead(\html_table_header_row(Cell, HeaderRow2))).
% In case the header option is not set, simply return the given rows.
html_table_header(_, false, _, _, DataRows, DataRows) --> [].


%! html_table_header_row(:Cell, +HeaderRow:list(ground))// is det.
% Generates the HTML table header row with given contents.

html_table_header_row(Cell, HeaderRow) -->
  html(tr(\html_table_cells(header, Cell, HeaderRow))).



% INDEX %

%! html_table_index_cell(
%!   +HasHeaderColumn:boolean,
%!   +IsIndexed:boolean,
%!   :Cell,
%!   +Index:ground
%! )// is det.

html_table_index_cell(HasHeaderColumn, true, Cell, Index) -->
  {(
    HasHeaderColumn == true
  ->
    Type = header
  ;
    Type = data
  )},
  html(\html_table_cell(Type, Cell, Index)).
html_table_index_cell(_, false, _, _) --> [].

