:- module(
  rdf_table,
  [
    rdf_assert_table/6 % +Graph:atom
                       % +Caption:atom
                       % +ColumnHeaders:list(atom)
                       % +RowHeaders:list(atom)
                       % +Rows:list(compound)
                       % -Table:iri
  ]
).

/** <module> RDF table

A simple RDF vocabulary for representing tables.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(generics(row_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_list)).



%! rdf_assert_table(
%!   +Graph:atom,
%!   +Caption:atom,
%!   +ColumnHeaders:list(atom),
%!   +RowHeaders:list(atom),
%!   +Rows:list(compound),
%!   -Table:iri
%! ) is det.
% Asserts a table in a simple RDF vocabulary.

rdf_assert_table(Graph, Caption, ColumnHeaders, RowHeaders, Rows, Table):-
  % Assert caption.
  rdf_assert_string(Table, rdf_table:caption, Caption, Graph),
  
  % Assert headers.
  rdf_assert_column_headers(Graph, Table, ColumnHeaders),
  rdf_assert_row_headers(Graph, Table, ColumnHeaders),
  
  % Assert rows.
  forall(
    nth0(Y, Rows, Row),
    rdf_assert_row(Graph, Table, Y, Row)
  ).


%! rdf_assert_row(+Graph:atom, +Table:iri, +Y:nonneg, +Row:compound) is det.
% Asserts a table row in a simple RDF vocabulary.

rdf_assert_row(Graph, Table, Y, Row):-
  forall(
    nth0_column(X, Row, Value),
    rdf_assert_cell(Graph, Table, X, Y, Value)
  ).


%! rdf_assert_cell(
%!   +Graph:atom,
%!   +Table:iri,
%!   +X:nonneg,
%!   +Y:nonneg,
%!   +Value
%! ) is det.
% Asserts a table cell in a simple RDF vocabulary.

rdf_assert_cell(Graph, Table, X, Y, Value):-
  nth0(X, ColumnHeaders, ColumnHeader),
  nth0(Y, RowHeaders, RowHeader),
  rdf_bnode(Cell),
  rdf_assert_string(Cell, rdf_table:column, ColumnHeader, Graph),
  rdf_assert_string(Cell, rdf_table:row, RowHeader, Graph),
  rdf_assert_datatype(Cell, rdf:value, Value, xsd:float, Graph).
  rdf_assert(Table, rdf_table:cell, Cell, Graph).


%! rdf_assert_column_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +ColumnHeaders:list(atom)
%! ) is det.
% Asserts the column headers of a table.

rdf_assert_column_headers(Graph, Table, ColumnHeaders
  rdf_global_id(rdf_table:columns, Predicate),
  rdf_assert_headers(Graph, Table, Predicate, RowHeaders).


%! rdf_assert_row_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +RowHeaders:list(atom)
%! ) is det.
% Asserts the row headers of a table.

rdf_assert_row_headers(Graph, Table, RowHeaders):-
  rdf_global_id(rdf_table:rows, Predicate),
  rdf_assert_headers(Graph, Table, Predicate, RowHeaders).


%! rdf_assert_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +Predicate:iri,
%!   +RowHeaders:list(atom)
%! ) is det.
% Asserts either column or row headers of a table, depending on `Predicate`.

rdf_assert_headers(Graph, Table, Predicate, Headers):-
  rdf_assert_list([datatype(xsd:string)], Headers, HeadersList, Graph),
  rdf_assert(Table, Predicate, HeadersList, Graph).

