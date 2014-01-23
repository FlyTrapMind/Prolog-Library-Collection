:- module(
  row_ext,
  [
    rows_to_lists/2, % +Rows:list(compound)
                     % -Lists:list(list)
    rows_to_propositions/3, % +Prefix:list
                            % +Rows:list(compound)
                            % -Propositions:ordset(list)
    rows_to_resources/2 % +Rows:list(compound)
                        % -Resources:ordset([bnode,iri,literal])
  ]
).

/** <module> Row extensions

Support for row compound terms, i.e. terms of the following form:
~~~{.pl}
row(Arg1, ..., ArgN)
~~~

Row terms are used in [library(csv)] and [library(semweb/sparql_client)].

@author Wouter Beek
@version 2013/12-2014/01
*/

:- use_module(library(apply)).
:- use_module(library(ordsets)).



%! rows_to_lists(+Rows:list(compound), -Lists:list(list)) is det.

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].


%! rows_to_propositions(
%!   +Prefix:list([bnode,literal,iri]),
%!   +Rows:list(compound),
%!   -Propositions:ordset(list)
%! ) is det.
% Returns the ordered set of propositions that occur in
%  the given SPARQL result set rows.
%
% @arg Prefix This contains the stable prefix list of each proposition.
%      This is usually the singleton list of the subject term.
% @arg Rows
% @arg Propositions An ordered set of lists of length 3 (s-p-o).

rows_to_propositions(Prefix, Rows, Props):-
  rows_to_propositions(Prefix, Rows, [], Props).

rows_to_propositions(_, [], Sol, Sol):- !.
rows_to_propositions(Prefix, [H1|T], L1, Sol):-
  row_to_proposition(Prefix, H1, H2),
  ord_add_element(L1, H2, L2),
  rows_to_propositions(Prefix, T, L2, Sol).

row_to_proposition(Prefix, Row, L):-
  Row =.. [row|Suffix],
  append(Prefix,  Suffix, L).


%! rows_to_resources(
%!   +Rows:list(compound),
%!   -Resources:ordset([bnode,iri,literal])
%! ) is det.
% Returns the ordered set of resources that occur in
%  the given SPARQL result set rows.

rows_to_resources(Rows, Resources):-
  rows_to_resources(Rows, [], Resources).

rows_to_resources([], Resources, Resources).
rows_to_resources([Row|Rows], Resources1, Sol):-
  Row =.. [row|NewResources],
  ord_union(Resources1, NewResources, Resources2),
  rows_to_resources(Rows, Resources2, Sol).

