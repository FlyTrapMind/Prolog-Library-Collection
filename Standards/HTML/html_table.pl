:- module(
  html_table,
  [
    empty_row//0,
    html_table/3, % +O1:list(nvpair)
                  % +Rows:list(list)
                  % -Markup:list
    html_table//2 % +O1:list(nvpair)
                  % +Rows:list(list)
  ]
).

/** <module> HTML tables

Support for generating HTML tables based on Prolog lists.

@author Wouter Beek
@version 2012/09-2013/06, 2013/09-2013/11
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(uri(rfc3987_dcg)).



empty_row -->
  html(tr(td([]))).



% DCG %

%! html_table(+Options:list(nvpair), +Rows:list(list))// is det.
% Generates the HTML markup for a table.
%
% The following options are supported:
%   1. =|caption(atom)|=
%      The table caption, if any.
%      Default is no caption.
%   2. =|header(boolean)|=
%      Whether or not the first sublist should be
%      displayed as the table header row.
%      Default is `true`.
%   3. =|indexed(+Indexed:boolean)|=
%      Whether or not each row should begin with a row index.
%      Default is `false`.

html_table(O1, L1) -->
  html_table_caption(O1),
  html_table_header(O1, L1, L2),
  {flag(table_row, _, 0)},
  html_table_rows(O1, td, L2).

html_table_caption(O1) -->
  {option(caption(Caption), O1)},
  html(caption, Caption).

html_table_cell(td, H) --> !,
  html(td(H)).
html_table_cell(th, H) --> !,
  html(th(H)).

html_table_cells(CellType, [H|T]) -->
  html_table_cell(CellType, H),
  html_table_cells(CellType, T).
html_table_cells(_CellType, []) --> [].

html_table_header(O1, [H1|T], T) -->
  {
    option(header(true), O1), !,
    (
      option(indexed(true), O1)
    ->
      H2 = ['#'|H1]
    ;
      H2 = H1
    )
  },
  html_table_row(O1, th, H2).
html_table_header(_O1, T, T) --> [].

html_table_row(O1, CellType, L) -->
  {
    option(indexed(true), O1),
    CellType == td, !,
    flag(table_row, RowN, RowN + 1)
  },
  html_table_cell(CellType, RowN),
  html_table_cells(CellType, L).
html_table_row(_O1, CellType, L) -->
  html_table_cells(CellType, L).

html_table_rows(O1, CellType, [H|T]) -->
  html_table_row(O1, CellType, H),
  html_table_rows(O1, CellType, T).
html_table_rows(_O1, _CellType, []) --> [].



% COMPOUND %

%! html_table(+O1:list(nvpair), +Rows:list(list), -Markup) is det.
% Returns the HTML markup for a table.
%
% The following options are supported:
%   1. =|caption(atom)|=
%      The table caption, if any.
%      Default is no caption.
%   2. =|header(boolean)|=
%      Whether or not the first sublist should be
%      displayed as the table header row.
%      Default is `true`.
%   3. =|indexed(+Indexed:boolean)|=
%      Whether or not each row should begin with a row index.
%      Default is `false`.
%
% @param O1 A list of name-value pairs.
% @param Rows A 2D table of terms.
% @param Markup An HTML table element.

html_table(O1, Rows1, element(table, [border=1], TableContents)):-
  % Generate the table caption, if required.
  table_caption(O1, CaptionMarkup),
  % Generate the table header, if required.
  table_header(O1, Rows1, HeaderMarkup, Rows2),
  % Reset the row number flag. This is used for row indexing.
  flag(table_row, _, 0),
  % Generate the table rows.
  maplist(table_row(O1, td), Rows2, RowsMarkup),
  % Put it all together.
  append([CaptionMarkup,HeaderMarkup,RowsMarkup], TableContents).

table_caption(O1, [element(caption,[],[Caption])]):-
  option(caption(Caption), O1), !.
table_caption(_Options, []).

%! table_cell(+CellType:oneof([td,th]), +Content:term, -Cell:compound) is det.

table_cell(CellType, Content1, element(CellType,[],[Content2])):-
  (
    % The table may contain markup.
    Content1 = element(_,_,_)
  ->
    Content2 = Content1
  ;
    dcg_phrase('IRI', Content1)
  ->
    Content2 = element(a,[href=Content1],[Content1])
  ;
    % If we use term_to_atom/2 for atom terms, extra single quotes are added
    % in front and at the end of the atom. Therefore, we first check whether
    % the term is an atom.
    atom(Content1)
  ->
    Content2 = Content1
  ;
    % No other options are left, just make sure it does not break.
    term_to_atom(Content1, Content2)
  ).

%! table_header(
%!   +O1:list(nvpair),
%!   +AllRows:list(list),
%!   -HeaderMarkup:list,
%!   -NonHeaderRows:list(list)
%! ) is det.
% Returns the header row of an HTML table, if the header option is present.

table_header(O1, [Header1|NonheaderRows], [HeaderMarkup], NonheaderRows):-
  % The header is only created if the `header` option is `true`.
  option(header(true), O1), !,

  % If option `indexed` is `true`, then the first header cell contains `#`.
  (
    option(indexed(true), O1)
  ->
    Header2 = ['#'|Header1]
  ;
    Header2 = Header1
  ),

  % Create the cell markup.
  table_row(O1, th, Header2, HeaderMarkup).
table_header(_Options, Rows, [], Rows).

%! table_row(
%!   +Options:list(nvpair),
%!   +CellType:oneof([td,th]),
%!   +Elements:list(term),
%!   -RowMarkup
%! ) is det.
% Returns the row of an HTML table containing the given elements.

table_row(O1, CellType, Elements, element(tr, [], Cells2)):-
  % Create the markup cells for the given elements/contents.
  maplist(table_cell(CellType), Elements, Cells1),

  % Create the markup cell for the row index, if option `indexed` is `true`.
  (
    option(indexed(true), O1),
    CellType == td
  ->
    flag(table_row, RowNumber, RowNumber + 1),
    table_cell(CellType, RowNumber, IndexCell),
    Cells2 = [IndexCell|Cells1]
  ;
    Cells2 = Cells1
  ).

