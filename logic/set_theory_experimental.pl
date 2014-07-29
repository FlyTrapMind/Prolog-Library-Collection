:- module(
  set_theory_experimental,
  [
    binary_subsets/2, % +Universe:list
                      % -Subsets:list(list(boolean))
    delete_supersets/4 % +Original:ordset(ordset)
                       % +Compare:ordset(ordset)
                       % -Result:ordset(ordset)
                       % -Rest:ordset(ordset)
  ]
).

/** <module> Set theory experimental

Predicates for set theory that are considered experimental,
i.e. not (yet) ready for use in applications.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(apply)).

:- use_module(generics(list_ext)).
:- use_module(logic(set_theory)).
:- use_module(pl(pl_mode)).



%! binary_subsets(+Set:ordset, -Subsets:list(list(bit))) is det.
% Returns all subsets of the given set as a list of binary lists.
%
% @arg Set An ordered set.
% @arg Subsets A list of bitlists representing subsets of =Set=.
%
% @tbd What is this?

binary_subsets(Set, Subsets):-
  cardinality(Set, Cardinality),
  repeating_list(0, Cardinality, BinarySet),
  call_complete(next_binary_subset, BinarySet, BinarySubsets),
  maplist(binary_overlay(Set), BinarySubsets, Subsets).


%! delete_supersets(
%!   +Original:ordset(ordset),
%!   +Compare:ordset(ordset),
%!   -Result:ordset(ordset),
%!   -Rest:ordset(ordset)
%! ) is det.
% Splits the `Original` sets into those that are and those that are not
% a superset of some member of `Compare`.
%
% @arg Original The original sets.
% @arg Compare The sets to compare with.
% @arg Result Supersets of some compare set.
% @arg Rest Strict subsets of some compare set.
%
% @tbd What is this?

delete_supersets(Original, Compare, Result, Rest):-
  partition(contains_superset_of(Compare), Original, Rest, Result).
contains_superset_of(Compare, Set):-
  member(Superset, Compare),
  superset(Superset, Set), !.


%! next_binary_subset(+Subset:list(bit), -NextSubset:list(bit)) is det.
% Returns the next subset.
%
% Subsets are represented as lists of bits.
%
% Positions in the list correspond to potential elements in the set.
% For example
% ~~~{.pl}
% [1,0,0,1,1,0]
% ~~~
% may denote the set
% ~~~{.txt}
% {a,d,e}
% ~~~
%
% @arg Subset A list of bits.
% @arg NextSubset A list of bits.

next_binary_subset([0|T], [1|T]).
next_binary_subset([1|T1], [0|T2]):-
  next_binary_subset(T1, T2).



% Helpers.

%! binary_overlay(+Original:list, +Overlay:list, -Result:list) is det.
%! binary_overlay(+Original:list, -Overlay:list, +Result:list) is det.
%! binary_overlay(+Original:list, ?Overlay:list, ?Result:list) is nondet.
% The result is the sublist of the original list,
%  where the overlay decides on which elements are kept (`1`)
%  and which are not (`0`).

binary_overlay(Original, Overlay, Result):-
  enforce_mode(
    '_binary_overlay'(Original, Overlay, Result),
    [Original,Overlay,Result],
    [[+,+,+]-semidet,[+,+,-]-det,[+,-,+]-det]
  ).
'_binary_overlay'([], [], []).
'_binary_overlay'([H|T1], [1|Overlay], [H|T2]):-
  '_binary_overlay'(T1, Overlay, T2).
'_binary_overlay'([_|T1], [0|Overlay], T2):-
  '_binary_overlay'(T1, Overlay, T2).

