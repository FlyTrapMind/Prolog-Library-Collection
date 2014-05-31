:- module(
  gv_html,
  [
    gv_html_like_label//1 % +Codes:list(code)
  ]
).

/** <module> GraphViz HTML

@author Wouter Beek
@version 2013/07, 2013/09, 2014/03-2014/06
*/

:- use_module(dcg(dcg_content)).
:- use_module(standards(html_dcg)).



%! gv_html_label(+Codes:list(code))// .
%
% @see http://www.graphviz.org/doc/info/shapes.html#html

gv_html_label --> gv_html_text, !.
gv_html_label --> gv_html_table, !.
gv_html_label --> [].

gv_html_like_label --> bracketed(angular, gv_html_label).

gv_html_like_label(Content) --> bracketed(angular, html_dcg(Content)).

gv_html_table --> html_element(table, _, gv_html_rows).
gv_html_table --> html_element(font, _, html_element(table, _, gv_html_rows)).

gv_html_rows --> gv_html_row, gv_html_rows.
gv_html_rows --> gv_html_row, html_element(hr, _), gv_html_rows.
gv_html_rows --> gv_html_row.

gv_html_row --> html_element(tr, _, gv_html_cells).

gv_html_cell --> html_element(td, _, gv_html_label).
gv_html_cell --> html_element(td, _, html_element(img, _)).

gv_html_cells --> gv_html_cell, gv_html_cells.
gv_html_cells --> gv_html_cell.
gv_html_cells --> gv_html_cell, html_element(vr, _), gv_html_cells.

gv_html_text --> gv_html_textitem, gv_html_text.
gv_html_text --> gv_html_textitem.

gv_html_textitem --> html_string, !.
gv_html_textitem --> html_entity, !.
gv_html_textitem --> html_element(br, _), !.
gv_html_textitem --> html_element(font, _, gv_html_text), !.
gv_html_textitem --> html_element(i, _, gv_html_text), !.
gv_html_textitem --> html_element(b, _, gv_html_text), !.
gv_html_textitem --> html_element(u, _, gv_html_text), !.
gv_html_textitem --> html_element(sub, _, gv_html_text), !.
gv_html_textitem --> html_element(sup, _, gv_html_text), !.

