:- module(
  ordset_ext,
  [
    ord_sets_to_pairs/2, % +Sets:list(ordset)
                         % -Pairs:ordset(pair)
    pairs_to_ord_sets/2, % +Pairs:list(pair(iri))
                         % -Sets:list(ordset(iri))
    rows_to_ord_set/2 % +Rows:list(compound)
                      % -Set:ordset
  ]
).

/** <module> Ordered set extensions

Extensions for SWI-Prolog library `ordsets`.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12
*/

:- use_module(generics(list_ext)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).



ord_sets_to_pairs(Sets, Pairs):-
  ord_sets_to_pairs(Sets, [], Pairs).

ord_sets_to_pairs([], Sol, Sol).
ord_sets_to_pairs([H|T], L1, Sol):-
  findall(
    X-Y,
    (
      member(X, Y, H),
      % No reflexive cases.
      X \== Y
    ),
    L2
  ),
  ord_union(L1, L2, L3),
  ord_sets_to_pairs(T, L3, Sol).



%! pairs_to_ord_sets(
%!   +Pairs:list(pair(iri)),
%!   -Sets:ordset(ordset(iri))
%! ) is det.
% For instance, the following pairs:
% ~~~
% <a,b>
% <a,c>
% <d,e>
% ~~~
% result in the following set:
% ~~~
% {{a,b,c},{d,e}}
% ~~~

pairs_to_ord_sets(Pairs, Sets):-
  pairs_to_ord_sets(Pairs, [], Sets).

pairs_to_ord_sets([], Sol, Sol).
% Connect two sets.
pairs_to_ord_sets([X-Y|T], Sets1, Sol):-
  member(OldSet1, Sets1),
  member(X, OldSet1),
  member(OldSet2, Sets1),
  OldSet1 \== OldSet2,
  member(Y, OldSet2), !,
  ord_union(OldSet1, OldSet2, NewSet),
  ord_del_element(Sets1, OldSet1, Sets2),
  ord_del_element(Sets2, OldSet2, Sets3),
  ord_add_element(Sets3, NewSet, Sets4),
  pairs_to_ord_sets(T, Sets4, Sol).
% Add to an existing set.
pairs_to_ord_sets([X-Y|T], Sets1, Sol):-
  member(OldSet, Sets1),
  (
    member(X, OldSet)
  ->
    ord_add_element(OldSet, Y, NewSet)
  ;
    member(Y, OldSet)
  ->
    ord_add_element(OldSet, X, NewSet)
  ), !,
  ord_del_element(Sets1, OldSet, Sets2),
  ord_add_element(Sets2, NewSet, Sets3),
  pairs_to_ord_sets(T, Sets3, Sol).
% New set.
pairs_to_ord_sets([X-Y|T], Sets1, Sol):-
  list_to_ord_set([X,Y], NewSet),
  ord_add_element(Sets1, NewSet, Sets2),
  pairs_to_ord_sets(T, Sets2, Sol).

%! rows_to_ord_set(+Rows:list(compound), -Set:ordset) is det.
% Returns a set of elements that appear in one-column rows.

rows_to_ord_set(Rs, S):-
  rows_to_ord_set(Rs, [], S).

rows_to_ord_set([], S, S):- !.
rows_to_ord_set([row(H)|T], S1, S):-
  ord_add_element(S1, H, S2),
  rows_to_ord_set(T, S2, S).



:- begin_tests(ordset_ext).

% Base case.
pairs_to_ord_sets_example([], []).
% No multisets.
pairs_to_ord_sets_example([a-b,a-b], [[a,b]]).
% Reflexive case.
pairs_to_ord_sets_example([a-a], [[a]]).
% Symmetric case.
pairs_to_ord_sets_example([a-b,b-a], [[a,b]]).
% Separate sets.
pairs_to_ord_sets_example([a-b,c-d], [[a,b],[c,d]]).
% Merging sets.
pairs_to_ord_sets_example([a-b,c-d,d-b], [[a,b,c,d]]).

test(
  pairs_to_ord_sets,
  [forall(pairs_to_ord_sets_example(Pairs,Sets)),true]
):-
  pairs_to_ord_sets(Pairs, Sets).

:- end_tests(ordset_ext).

