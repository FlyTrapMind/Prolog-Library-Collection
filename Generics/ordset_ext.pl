:- module(
  ordset_ext,
  [
    equivalence_sets_to_number_of_equivalence_pairs/2, % +EquivalenceSets:list(ordset)
                                                       % +NumberOfEquivalencePairs:nonneg
    ord_sets_to_pairs/2, % +Sets:list(ordset)
                         % -Pairs:ordset(pair)
    pairs_to_ord_sets/2 % +Pairs:list(pair(iri))
                        % -Sets:list(ordset(iri))
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



%! equivalence_sets_to_number_of_equivalence_pairs(
%!   +EquivalenceSets:list(ordset),
%!   +NumberOfEquivalencePairs:nonneg
%! ) is det.
% Returns the number of equivalence pairs that are encoded in
% the given collection of equivalence sets.
%
% We do not count reflexive cases.
% We do count symmetric cases.

equivalence_sets_to_number_of_equivalence_pairs(EqSets, NumberOfEqPairs):-
  aggregate_all(
    sum(NumberOfEqPairs__),
    (
      member(EqSet, EqSets),
      length(EqSet, NumberOfMembers),
      NumberOfEqPairs__ is NumberOfMembers * (NumberOfMembers - 1)
    ),
    NumberOfEqPairs
  ).

ord_sets_to_pairs(Sets, Pairs):-
  ord_sets_to_pairs(Sets, [], Pairs).

ord_sets_to_pairs([], Sol, Sol).
ord_sets_to_pairs([H|T], L1, Sol):-
  findall(
    X-Y,
    (
      member(X, Y, H),
      X \== Y
    ),
    L2
  ),
  ord_union(L1, L2, L3),
  ord_sets_to_pairs(T, L3, Sol).

%! pairs_to_ord_set(
%!   +OldPairs:list(pair),
%!   -Set:ordset,
%!   -NewPairs:list(pair)
%! ) is det.
% Returns the equivalence closure of the first of the given pairs
% w.r.t. all the other pairs.
% The pairs that do not occur in the closure are returned.

pairs_to_ord_set([X-Y|Pairs1], Set2, Pairs2):-
  % We took the first alignment pair.
  % Turn the pair into a sorted list.
  list_to_ord_set([X,Y], Set1),

  % Add the members from all the other alignment pairs
  % that reach into this alignment set.
  pairs_to_ord_set(Set1, Pairs1, Set2, Pairs2).

%! pairs_to_ord_set(
%!   +OldSet:ordset,
%!   +OldPairs:list(pair),
%!   -OldSet:ordset,
%!   -OldPairs:list(pair)
%! ) is det.

pairs_to_ord_set(Set1, Pairs1, Set3, Pairs3):-
  % The next alignments we add must relate to a resource
  % that is already in the alignment set.
  memberchk(X, Set1),

  % Look up a related resource in the alginment pairs.
  % The pair can be found in either direction.
  (
    selectchk(X-Y, Pairs1, Pairs2)
  ;
    selectchk(Y-X, Pairs1, Pairs2)
  ),
  % We can safely add a cut here, since we will come back for any
  % `Y-X` option that appears after a `X-Y` option.
  !,

  % The added resource must not already appear in the alignment set.
  % Maybe some alignments contain double occurrences of the same pair.
  % The alignment set stays the same in those cases.
  ord_add_element(Set1, Y, Set2),

  % Recurse.
  pairs_to_ord_set(Set2, Pairs2, Set3, Pairs3).
pairs_to_ord_set(Set, Pairs, Set, Pairs).

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
% are converted to the following set:
% ~~~
% {{a,b,c},{d,e}}
% ~~~

pairs_to_ord_sets(Pairs, Sets):-
  pairs_to_ord_sets(Pairs, [], Sets).

pairs_to_ord_sets([], Sets, Sets):- !.
pairs_to_ord_sets(Pairs1, Sets1, Sets3):-
  pairs_to_ord_set(Pairs1, Set, Pairs2),
  ord_add_element(Sets1, Set, Sets2),
  pairs_to_ord_sets(Pairs2, Sets2, Sets3).

