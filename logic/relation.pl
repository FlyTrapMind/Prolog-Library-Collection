:- module(
  relation,
  [
    equivalence/1, % +Relation:ugraph
    equivalence_class/4, % +Set:ordset
                         % +EquivalenceRelation:ugraph
                         % +Element
                         % -EquivalenceClass:ordset
    quotient_set/3, % +Set:ordset
                    % +EquivalenceRelation:ugraph
                    % -QuotientSet:ordset(ordset)
    reflexive/1, % +Relation:ugraph
    reflexive_closure/2, % +Relation:ugraph
                         % -ReflexiveRelation:ugraph
    relation/3, % ?Relation:ugraph
                % ?Set:ordset
                % ?Pairs:ordset(pair)
    relation_element/2, % +Relation:ugraph
                        % ?Element
    relation_pair/2, % +Relation:ugraph
                     % ?Pair:pair
    symmetric/1, % +Relation:ugraph
    symmetric_closure/2, % +Relation:ugraph
                         % -SymmetricRelation:ugraph
    transitive/1, % +Relaton:ugraph
    transitive_closure/2 % +Relation:ugraph
                         % -TransitiveRelation:ugraph
  ]
).

/** <module> Relation

Support for properties of relations.

@author Wouter Beek
@version 2012/11, 2013/08, 2014/08
*/

:- use_module(library(lambda)).
:- use_module(library(lists), except([subset/2])).

:- use_module(graph_theory(graph_theory)).
:- use_module(logic(set_theory)).
:- use_module(pl(pl_mode)).

:- meta_predicate(relation_closure(+,1,+,-)).



%! equivalence(+Relation:ugraph) is semidet.
% Succeeds if the given relation is an equivalence relation.

equivalence(Relation):-
  reflexive(Relation),
  symmetric(Relation),
  transitive(Relation).


%! equivalence_class(
%!   +Set:ordset,
%!   +EquivalenceRelation:ugraph,
%!   +Element,
%!   -EquivalenceClass:ordset
%! ) is det.
% Returns the equivalence class of =X= relative to equivalence relation =R=.
%
% The function that maps from elements onto their equivalence classes is
% sometimes calles the *|canonical projection map|*.
%
% @arg Set The universe of discource.
%      This must comprise `Element`.
%      Clearly, `EquivalenceClass` \subseteq `Set`.
%      Represented as an ordered set.
% @arg Equivalence An binary equivalence relation,
%      i.e., a relation that is:
%        1. Reflexive
%        2. Symmetric
%        3. Transitive
%      Represented as a directed graph (see [ugraph]).
% @arg Element The element whose equivalence class is returned.
% @arg EquivalenceClass The equivalence class of `Element`.
%      This is an ordered set.

equivalence_class(Set, EquivalenceRelation, Element, EquivalenceClass):-
  % Use of findall/3 ensures that order is preserved in `EquivalenceClass`,
  % since `Set` is assumed to be ordered.
  findall(
    EquivalentElement,
    (
      member(EquivalentElement, Set),
      adjacent(EquivalenceRelation, Element, EquivalentElement)
    ),
    EquivalenceClass
  ).


%! quotient_set(
%!   +Set:ordset,
%!   +EquivalenceRelation:ugraph,
%!   -QuotientSet:ordset(ordset)
%! ) is det.
% Returns the quotient set for `Set`,
% closed under equivalence relation `EquivalenceRelation`.
%
% The quotient set of a set `Set` is the set of all equivalence sets of
% elements in `Set`.
%
% A quotient set of `Set` is also a partition of `Set`.
%
% The standard notation for a quotient set is $S / \approx$.
%
% @arg Set An ordered set.
% @arg EquivalenceRelation A (binary) equivalence relation.
%      Represented as a directed graph (see [ugraph]).
% @arg QuotientSet The quotient set of `Set`.
%      An ordered set.
%
% @tbd Use the algorithm for calculating graph components for this?

quotient_set(Set, EquivalenceRelation, QuotientSet):-
  aggregate_all(
    set(EquivalenceClass),
    (
      member(Element, Set),
      equivalence_class(Set, EquivalenceRelation, Element, EquivalenceClass)
    ),
    QuotientSet
  ).


%! reflextive(+Relation:ugraph) is semidet.
% Succeeds if the given binary relation is reflexive.

reflexive(Relation):-
  forall(
    member(X-Ns, Relation),
    memberchk(X, Ns)
  ).


%! reflexive_closure(+Relation:ugraph, -ReflexiveRelation:ugraph) is det.

reflexive_closure(Relation, ReflexiveRelation):-
  relation_closure(
    Relation,
    \Pair^(member(X-_, Pair) ; member(_-X, Pair)),
    X-X,
    ReflexiveRelation
  ).


%! relation(+Relation:ugraph, -Set:ordset, -Pairs:ordset(pair)) is det.
%! relation(-Relation:ugraph, +Set:ordset, +Pairs:ordset(pair)) is det.

relation(Relation, Set, Pairs):-
  graph(Relation, Set, Pairs).


%! relation_element(+Relation:ugraph, +Element) is semidet.
%! relation_element(+Relation:ugraph, -Element) is nondet.

relation_element(Relation, Element):-
  call_ground_as_semidet(member(Element-_, Relation)).


%! relation_pair(+Relation:ugraph, +Pair:pair) is semidet.
%! relation_pair(+Relation:ugraph, -Pair:pair) is nondet.
% The extension of a binary relation.

relation_pair(Relation, Pair):-
  edge(Relation, Pair).


%! symmetric(+Relation:ugraph) is semidet.
% Succeeds if the given relation is symmetric.

symmetric(Relation):-
  forall(
    relation_pair(Relation, X-Y),
    relation_pair(Relation, Y-X)
  ).


%! symmetric_closure(+Relation:ugraph, -SymmetryRelation:ugraph) is det.

symmetric_closure(Relation, SymmetryRelation):-
  relation_closure(
    Relation,
    \Relation^relation_pair(Relation, X-Y),
    Y-X,
    SymmetryRelation
  ).


%! transitive(+Relation:ugraph) is semidet.
% Suceeds if the given binary relation is transitive.

transitive(Relation):-
  forall(
    (
      relation_pair(Relation, X-Y),
      relation_pair(Relation, Y-Z)
    ),
    relation_pair(Relation, X-Z)
  ).


%! transitive_closure(+Relation:ugraph, -TransitiveRelation:ugraph) is det.

transitive_closure(Relation, TransitiveRelation):-
  relation_closure(
    Relation,
    \Pairs^(member(X-Y, Pairs), member(Y-Z, Pairs)),
    X-Z,
    TransitiveRelation
  ).



% Helpers.

%! relation_closure(
%!   +Relation:ugraph,
%!   :Goal,
%!   +Pattern:compound,
%!   -ClosedRelation:ugraph
%! ) .
% Allows the calculation of the closure of a relation directly.
% Internally, the closure is calculated for the extension of the relation,
% i.e., its edge pairs.
%
% The mode is the same as for `Goal`.

relation_closure(Relation, Goal, Pattern, ClosedRelation):-
  relation(Relation, Set, Pairs),
  closure(Pairs, Goal, Pattern, ClosedPairs),
  relation(ClosedRelation, Set, ClosedPairs).

