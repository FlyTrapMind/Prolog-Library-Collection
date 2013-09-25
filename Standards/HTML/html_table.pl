:- module(
  html_table,
  [
    html_table/3 % +O1:list(nvpair)
                 % +List:list(list(term))
                 % -Markup:element
  ]
).

/** <module> HTML tables

Support for generating HTML tables based on Prolog lists.

@author Wouter Beek
@version 2012/09-2013/06, 2013/09
*/

:- use_module(library(apply)).
:- use_module(library(option)).



%! html_table(
%!   +O1:list(nvpair),
%!   +Rows:list(list(term)),
%!   -Markup
%! ) is det.
% Returns the HTML markup for a table.
%
% @apram O1 A list of name-value pairs. The following options are
%        supported:
%        1. =|caption(atom)|=
%           The table caption, if any.
%           Default is no caption.
%        2. =|header(boolean)|=
%           Whether or not the first sublist should be
%           displayed as the table header row.
%           Default is `true`.
%        3. =|indexed(+Indexed:boolean)|=
%           Whether or not each row should begin with a row index.
%           Default is `false`.
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

