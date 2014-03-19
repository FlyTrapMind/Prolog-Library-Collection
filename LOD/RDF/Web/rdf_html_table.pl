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
@version 2014/01-2014/03
*/

:- use_module(generics(typecheck)).
:- use_module(dcg(dcg_generic)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf_term(rdf_string)).
:- use_module(rdf_web(rdf_term_html)).
:- use_module(xml(xml_namespace)).

:- meta_predicate(rdf_html_table(+,//,+,?,?)).
:- rdf_meta(rdf_html_table(r,?,?)).

:- xml_register_namespace(rdf_table, 'http://www.wouterbeek.com/rdf_table#').



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


%! rdf_html_table(+Options:list(nvpair), +Table:iri)// is det.
% The following options are supported:
%   * =|header_column(HasHeaderColumn:boolean)|=
%   * =|header_row(HasHeaderRow:boolean)|=

rdf_html_table(O1, Table) -->
  {
    option(header_column(HasHeaderColumn), O1),
    option(header_row(HasHeaderRow), O1),
    rdf_string(Table, rdf_table:caption, Caption, _),
    rdf(Table, rdf_table:columns, Columns1),
    rdf_list([datatype(xsd:string)], Columns1, Columns2),
    rdf(Table, rdf_table:rows, Rows1),
    rdf_list([datatype(xsd:string)], Rows1, Rows2),
    table1(Table, HasHeaderColumn, Columns2, Rows2, L1),
    (
      HasHeaderRow == true
    ->
      L2 = [Columns2|L1]
    ;
      L2 = L1
    )
  },
  rdf_html_table(O1, rdf_term_html(Caption), L2).

table1(_, _, _, [], []):- !.
table1(Table, HasHeaderColumn, Columns, [Row|Rows], [H2|T]):-
  table2(Table, Columns, Row, H1),
  (
    HasHeaderColumn == true
  ->
    H2 = [Row|H1]
  ;
    H2 = H1
  ),
  table1(Table, HasHeaderColumn, Columns, Rows, T).

table2(_, [], _, []):- !.
table2(Table, [Column|Columns], Row, [H|T]):-
  rdf(Table, rdf_table:cell, Cell),
  rdf_string(Cell, rdf_table:column, Column, _),
  rdf_string(Cell, rdf_table:row, Row, _),
  rdf(Cell, rdf:value, H), !,
  table2(Table, Columns, Row, T).


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
%     Often occurring header rows are atoms that consist of the follow
%     characters, where the characters correspond to columns,
%     in the order in which they occur.
%   * Other options are handed to html_table//4.
%
% The following characters are supported for the row header option:
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
    select_option(header_row(HeaderRow), O2, O3, false),
    header_row_presets(HeaderRow1, Rows1, HeaderRow2, Rows2),
    merge_options([header_row(HeaderRow2)], O3, O4)
  },
  html(\html_table(O4, Caption, rdf_term_html(Graph), Rows2)).


rdf_html_tables(_, []) --> !, [].
rdf_html_tables(O1, [H|T]) -->
  rdf_html_table(O1, H),
  rdf_html_tables(O1, T).

