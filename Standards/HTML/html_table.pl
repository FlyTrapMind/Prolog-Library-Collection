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

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(html(html_pl_term)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).



%! html_table(
%!   +Options:list(nvpair),
%!   :Caption,
%!   +Rows:list(list(ground))
%! )// is det.
%! html_table(
%!   +Options:list(nvpair),
%!   :Caption,
%!   :Cell,
%!   +Rows:list(list(ground))
%! )// is det.
% Generates the HTML markup for a table.
%
% The following options are supported:
%   1. =|header(boolean)|=
%      Whether or not the first row should be
%      displayed as the table header row.
%      Default is `false`.
%   2. =|highlighted_row(:HighlightedRow)|=
%      A semidet predicate term that is missing its last parameter.
%      Default: `false` for no row highlighted.
%   3. =|indexed(+Indexed:boolean)|=
%      Whether or not each row should begin with a row index.
%      Counts starts at 0. The header row, if included, is not counted.
%      Default is `false`.

:- meta_predicate(html_table(:,//,+,?,?)).
html_table(O1, Caption, Rows) -->
  html_table(O1, Caption, html_pl_term, Rows).

:- meta_predicate(html_table(:,//,3,+,?,?)).
is_meta(highlighted_row).
html_table(O1, Caption, Cell, Rows) -->
  {
    flag(table_row, _, 0),
    meta_options(is_meta, O1, O2),
    option(header(HasHeader), O2, false),
    option(highlighted_row(HighlightedRow), O2, fail),
    option(indexed(IsIndexed), O2, false)
  },
  html(
    table(class=['pure-table','pure-table-bordered'], [
      \html_table_caption(Caption),
      \html_table_header(HasHeader, IsIndexed, Cell, Rows, DataRows),
      tbody(
        \html_table_data_rows(IsIndexed, HighlightedRow, Cell, DataRows)
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

:- meta_predicate(html_table_caption(//,?,?)).
html_table_caption(VAR) -->
  {var(VAR)}, !,
  [].
html_table_caption(Caption1) -->
  {dcg_with_output_to(atom(Caption2), Caption1)},
  html(caption(Caption2)).



% CELL %

%! html_table_cells(
%!   +Type:oneof([data,header]),
%!   :Cell,
%!   +Elements:list(ground)
%! )// is det.

:- meta_predicate(html_table_cells(+,3,+,?,?)).
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

:- meta_predicate(html_table_cell(+,3,+,?,?)).
html_table_cell(data, Cell, Element) --> !,
  html(td(\dcg_call(Cell, Element))).
html_table_cell(header, Cell, Element) --> !,
  html(th(\dcg_call(Cell, Element))).



% DATA %

%! html_table_data_rows(
%!   +IsIndexed:boolean,
%!   :Highlighted,
%!   :Cell,
%!   +DataRows
%! )// is det.

:- meta_predicate(html_table_data_rows(+,1,3,+,?,?)).
html_table_data_rows(IsIndexed, Highlighted, Cell, [H|T]) -->
  html_table_data_row(IsIndexed, Highlighted, Cell, H),
  html_table_data_rows(IsIndexed, Highlighted, Cell, T).
html_table_data_rows(_, _, _, []) -->
  [].


%! html_table_data_row(
%!   +IsIndexed:boolean,
%!   :Highlighted,
%!   :Cell,
%!   +DataRows:list(list(ground))
%! )// is det.

:- meta_predicate(html_table_data_row(+,1,3,+,?,?)).
html_table_data_row(IsIndexed, Highlighted, Cell, DataRow) -->
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

  html(
    tr(O1, [
      \html_table_index_cell(IsIndexed, Cell, RowNumber),
      \html_table_cells(data, Cell, DataRow)
    ])
  ).



% HEADER %

%! html_table_header(
%!   +HasHeader:boolean,
%!   +IsIndexed:boolean,
%!   :Cell,
%!   +Rows:list(list(ground)),
%!   -DataRows:list(list(ground))
%! )// is det.

:- meta_predicate(html_table_header(+,+,3,+,-,?,?)).
% Options state a header row should be included.
% We take the first row, and return the other rows for later processing.
% Only add a header if the corresponding option says so.
html_table_header(true, IsIndexed, Cell, [HeaderRow1|DataRows], DataRows) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(
    IsIndexed == true
  ->
    HeaderRow2 = ['#'|HeaderRow1]
  ;
    HeaderRow2 = HeaderRow1
  )},
  html(thead(\html_table_header_row(Cell, HeaderRow2))).
% In case the header option is not set, simply return the given rows.
html_table_header(false, _, DataRows, DataRows) --> [].


%! html_table_header_row(:Cell, +HeaderRow:list(ground))// is det.
% Generates the HTML table header row with given contents.

:- meta_predicate(html_table_header_row(3,+,?,?)).
html_table_header_row(Cell, HeaderRow) -->
  html(tr(\html_table_cells(header, Cell, HeaderRow))).



% INDEX %

%! html_table_index_cell(+IsIndexed:boolean, :Cell, +Index:ground)// is det.

:- meta_predicate(html_table_index_cell(+,3,+,?,?)).
html_table_index_cell(true, Cell, Index) -->
  html(\html_table_cell(data, Cell, Index)).
html_table_index_cell(false, _, _) --> [].
