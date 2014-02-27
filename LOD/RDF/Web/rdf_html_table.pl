:- module(
  rdf_html_table,
  [
    rdf_html_table//1, % +Table:iri
    rdf_html_table//3, % +Options:list(nvpair)
                       % :Caption
                       % +Rows:list(list(ground))
    rdf_html_tables//1 % +Tables:list(iri)
  ]
).

/** <module> RDF HTML table

Generates HTML tables with RDF content.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- meta_predicate(rdf_html_table(+,//,+,?,?)).

:- rdf_meta(rdf_html_table(r,?,?)).

:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf_web(rdf_html_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf_table, 'http://www.wouterbeek.com/rdf_table.owl#').



rdf_html_table(Table) -->
  {
    rdf_datatype(Table, rdf_table:caption, xsd:string, Caption, _),
    rdf(Table, rdf_table:columns, Columns1),
    rdf_global_id(xsd:string, XSDString),
    rdf_list([datatype(XSDString)], Columns1, Columns2),
    rdf(Table, rdf_table:rows, Rows1),
    rdf_list([datatype(XSDString)], Rows1, Rows2),
    table1(Table, Columns2, Rows2, L)
  },
  rdf_html_table(
    [header_column(true),header_row(true)],
    rdf_html_term(Caption),
    [Columns2|L]
  ).

table1(_, _, [], []):- !.
table1(Table, Columns, [Row|Rows], [H|T]):-
  table2(Table, Columns, Row, H),
  table1(Table, Columns, Rows, T).

table2(_, [], _, []):- !.
table2(Table, [Column|Columns], Row, [H|T]):-
  rdf(Table, rdf_table:cell, Cell),
  rdf_datatype(Cell, rdf_table:column, xsd:string, Column, _),
  rdf_datatype(Cell, rdf_table:row, xsd:string, Row, _),
  rdf(Cell, rdf:value, H), !,
  table2(Table, Columns, Row, T).


%! rdf_html_table(
%!   +Options:list(nvpair),
%!   :Caption,
%!   +Rows:list(list(ground))
%! )// is det.

% Do not fail for empty data lists.
rdf_html_table(_, _, []) --> !, [].
rdf_html_table(O1, Caption, Rows) -->
  {select_option(graph(Graph), O1, O2, _NoGraph)},
  html(\html_table(O2, Caption, rdf_html_term(Graph), Rows)).


rdf_html_tables([]) --> [].
rdf_html_tables([H|T]) -->
  rdf_html_table(H),
  rdf_html_tables(T).

