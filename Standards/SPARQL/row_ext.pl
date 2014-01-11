:- module(
  row_ext,
  [
    rows_to_lists/2, % +Rows:list(compound)
                     % -Lists:list(list)
    rows_to_ord_set/2 % +Rows:list(compound)
                      % -Set:ordset
  ]
).

/** <module> Row extensions

Support for row compound terms, i.e. terms of the following form:
~~~{.pl}
row(Arg1, ..., ArgN)
~~~

Row terms are used in [library(csv)] and [library(semweb/sparql_client)].

@author Wouter Beek
@version 2013/12
*/

:- use_module(library(apply)).
:- use_module(library(ordsets)).



%! rows_to_lists(+Rows:list(compound), -Lists:list(list)) is det.

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].



%! rows_to_ord_set(+Rows:list(compound), -Set:ordset) is det.
% Returns a set of elements that appear in one-column rows.

rows_to_ord_set(Rs, S):-
  rows_to_ord_set(Rs, [], S).

rows_to_ord_set([], S, S):- !.
rows_to_ord_set([row(H)|T], S1, S):-
  ord_add_element(S1, H, S2),
  rows_to_ord_set(T, S2, S).

