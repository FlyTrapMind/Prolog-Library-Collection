:- module(
  set_theory,
  [
    arbitrary_intersection/2, % +Sets:list(ordset)
                              % -Intersection:ordset
    arbitrary_union/2, % +Sets:list(ordset)
                       % -Union:ordset
    cardinality/2, % +Set:ordset
                   % -Cardinality:integer
    cartesian_product/2, % +Sets:list(ordset)
                         % -CartesianProduct:ordset(list)
    emptyset/1, % ?Set:ordset
    equinumerous/2, % +Set1:ordset
                    % +Set2:ordset
    minimal_set/2, % +MinimalSet:ordset
                   % +Sets:ordset(ordset)
    random_subset/2, % +Set:ordset
                     % -Subset:ordset
    strict_subset/2, % ?Subset:ordset
                     % +Superset:ordset
    strict_superset/2, % +Superset:ordset
                       % ?Subset:ordset
    subset/2, % ?Subset:ordset
              % +Superset:ordset
    superset/2 % +Superset:ordset
               % ?Subset:ordset
  ]
).

/** <module> Set theory

Extra set functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/11-2011/12, 2012/02, 2012/08, 2012/10, 2013/05, 2013/12,
         2014/03, 2014/07
*/

:- use_module(library(error)).
:- use_module(library(lists), except([subset/2])).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).



%! arbitrary_intersection(+Sets:list(ordset), -Intersection:ordset) is det.

arbitrary_intersection(Sets, Intersection):-
  ord_intersection(Sets, Intersection).


%! arbitrary_union(+Sets:list(ordset), -Union:ordset) is det.

arbitrary_union(Sets, Union):-
  ord_union(Sets, Union).


%! cardinality(+Set:ordset, -Cardinality:nonneg) is det.
% Returns the cardinality of the given set.
%
% @throws type_error If `Set` is not an ordered set.

cardinality(Set, _):-
  \+ is_ordset(Set), !,
  type_error(ordset, Set).
cardinality(Set, Cardinality):-
  length(Set, Cardinality).


%! cartesian_product(
%!   +Sets:list(ordset),
%!   -CartesianProduct:ordset(list)
%! ) is det.

cartesian_product([], []).
cartesian_product([Set|Sets], [H|T]):-
  cartesian_product(Sets, T),
  member(H, Set).


%! emptyset(+Set:ordset) is semidet.
%! emptyset(-Set:ordset) is det.

emptyset([]).


%! equinumerous(+Set1:ordset, +Set2:ordset) is semidet.
% Succeeds if the given sets are *equinumerous*, i.e.,
% if they have the same cardinality.
%
% @throws type_error if one of the arguments is not an ordered set.

equinumerous(Set1, _):-
  \+ is_ordset(Set1), !,
  type_error(ordset, Set1).
equinumerous(_, Set2):-
  \+ is_ordset(Set2), !,
  type_error(ordset, Set2).
equinumerous(Set1, Set2):-
  same_length(Set1, Set2).


%! minimal_set(+MinimalSet:ordset, +Sets:ordset(ordset)) is semidet.
%! minimal_set(-MinimalSet:ordset, +Sets:ordset(ordset)) is nondet.
% Set $s \in S$ is a *|minimal set|* of a set of sets $S$ iff
% $S$ contains no strict subset of $s$.

minimal_set(MinimalSet, Compare1):-
  select(MinimalSet, Compare1, Compare2),
  \+((
    member(Subset, Compare2),
    subset(Subset, MinimalSet)
  )).


%! random_subset(+Set:ordset, -RandomSubset:ordset) is det.
% Returns a random subset of the given set.
%
% @throws type_error If `Set` is not an ordered set.

random_subset(Set, _):-
  \+ is_ordset(Set), !,
  type_error(ordset, Set).
random_subset(Set, RandomSubset):-
  random_sublist(Set, RandomSubset).


%! strict_subset(+Subset:ordset, +Superset:ordset) is semidet.
%! strict_subset(-Subset:ordset, +Superset:ordset) is nondet.
% A *|strict subset|* is a non-equivalent subset.
%
% @throws type_error If `Superset` is not an ordered set.

strict_subset(Subset, Superset):-
  % Predicate subset/2 throws the type_error.
  subset(Subset, Superset),
  Subset \== Superset.


%! strict_superset(+Superset:ordset, +Subset:ordset) is semidet.
%! strict_superset(+Superset:ordset, -Subset:ordset) is nondet.
% A *|strict superset|* is a non-equivalent superset.
%
% @throws type_error If `Superset` is not an ordered set.

strict_superset(Subset, Superset):-
  % Predicate superset/2 throws the type_error.
  superset(Superset, Subset),
  Superset \== Subset.


%! subset(+Subset:ordset, +Superset:ordset) is semidet.
%! subset(-Subset:ordset, +Superset:ordset) is multi.
% A *|subset|* contains at least the same objects.
%
% @see Inverse of superset/2.
% @throws type_error If `Superset` is not an ordered set.

subset(_, Superset):-
  \+ is_ordset(Superset), !,
  type_error(ordset, Superset).
subset(Subset, Superset):-
  ord_subset(Subset, Superset).


%! superset(+Superset:ordset, +Subset:ordset) is semidet.
%! superset(+Superset:ordset, -Subset:ordset) is multi.
%
% @see Inverse of subset/2.
% @throws type_error If `Superset` is not an ordered set.

superset(Superset, Subset):-
  % Predicate subset/2 throws the type_error.
  subset(Subset, Superset).

