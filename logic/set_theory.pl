:- module(
  set_theory,
  [
    cardinality/2, % +Set:ordset
                   % -Cardinality:integer
    cartesian_product/2, % +Sets:list(ordset)
                         % -CartesianProduct:ordset(list)
    delete_supersets/4, % +Original:ordset(ordset)
                        % +Compare:ordset(ordset)
                        % -Result:ordset(ordset)
                        % -Rest:ordset(ordset)
    equinumerous/2, % +Set1:ordset
                    % +Set2:ordset
    minimal/2, % +Minimal:ordset
               % +Sets:ordset(ordset)
    random_subset/2, % +Set:ordset
                     % -Subset:ordset
    subset/2, % ?Subset:ordset
              % +Superset:ordset
    subsets/2, % +Universe:list
               % -Subsets:list(list(boolean))
    superset/2 % +Superset:ordset
               % ?Subset:ordset
  ]
).

/** <module> Set theory

Extra set functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/11-2011/12, 2012/02, 2012/08, 2012/10, 2013/05, 2013/12, 2014/03,
         2014/07
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([subset/2])).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).
:- use_module(pl(pl_mode)).



%! cardinality(+Set:ordset, -Cardinality:nonneg) is det.
% Returns the cardinality of the given set.

cardinality(Set, Cardinality):-
  is_ordset(Set),
  length(Set, Cardinality).


%! cartesian_product(
%!   +Sets:list(ordset),
%!   -CartesianProduct:ordset(list)
%! ) is det.

cartesian_product([], []).
cartesian_product([Set|Sets], [H|T]):-
  cartesian_product(Sets, T),
  member(H, Set).


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

delete_supersets(Original, Compare, Result, Rest):-
  partition(contains_superset_of(Compare), Original, Rest, Result).
contains_superset_of(Compare, Set):-
  member(Superset, Compare),
  superset(Superset, Set), !.


%! equinumerous(+Set1:ordset, +Set2:ordset) is semidet.
% Succeeds if the given sets are equinumerous, i.e.,
% if they have the same cardinality.

% @see cardinality/2 takes care of the list-to-set conversion.
equinumerous(Set1, Set2):-
  maplist(is_ordset, [Set1,Set2]),
  same_length(Set1, Set2).


%! minimal(+Minimal:ordset, +Sets:ordset(ordset)) is semidet.
%! minimal(-Minimal:ordset, +Sets:ordset(ordset)) is nondet.
% Set $s \in S$ is a *|minimal set|* of a set of sets $S$ iff
% $S$ contains no strict subset of $s$.

minimal(Minimal, Compare1):-
  select(Minimal, Compare1, Compare2),
  \+((
    member(Subset, Compare2),
    subset(Subset, Minimal)
  )).


%! next_subset(+Subset:list(bit), -NextSubset:list(bit)) is det.
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

next_subset([0|T], [1|T]).
next_subset([1|T1], [0|T2]):-
  next_subset(T1, T2).


%! random_subset(+Set:ordset, -Subset:ordset) is det.

random_subset(S1, S2):-
  random_sublist(S1, S2).


%! subset(+Subset:ordset, +Superset:ordset) is semidet.
%! subset(-Subset:ordset, +Superset:ordset) is multi.

subset(Subset, Superset):-
  ord_subset(Subset, Superset).


%! subsets(+Set:ordset, -Subsets:list(list(bit))) is det.
% Returns all subsets of the given set as a list of binary lists.
%
% @arg Set An ordered set.
% @arg Subsets A list of bitlists representing subsets of =Set=.

subsets(Set, Subsets):-
  cardinality(Set, Cardinality),
  repeating_list(0, Cardinality, BinarySet),
  call_complete(next_subset, BinarySet, BinarySubsets),
  maplist(binary_overlay(Set), BinarySubsets, Subsets).


%! superset(+Superset:ordset, +Subset:ordset) is semidet.
%! superset(+Superset:ordset, -Subset:ordset) is multi.
% @see Inverse of subset/2.

superset(Superset, Subset):-
  ord_subset(Subset, Superset).



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

