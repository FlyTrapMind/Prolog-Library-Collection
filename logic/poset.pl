:- module(
  poset,
  [
    identity/3, % +Poset:ugraph
                % ?Element1
                % ?Element2
    maximal/2, % +Poset:ugraph
               % ?Maximal
    maximum/2, % +Poset:ugraph
               % ?Maximum
    minimal/2, % +Poset:ugraph
               % ?Minimal
    minimum/2, % +Poset:ugraph
               % ?Minimum
    poset/3 % ?PartiallyOrderedSet:ugraph
            % ?Set:ordset
            % ?Order:ordset(pair)
  ]
).

/** <module> Poset

Support for partially ordered sets.

Design decisions:
  * Posets are represented by ugraphs (see [ugraph]).

@author Wouter Beek
@version 2014/08
*/

:- use_module(graph_theory(graph_theory)).
:- use_module(logic(relation)).



%! identity(+Poset:ugraph, +Element1, +Element2) is semidet.
%! identity(+Poset:ugraph, +Element1, -Element2) is nondet.
%! identity(+Poset:ugraph, -Element1, +Element2) is nondet.
%! identity(+Poset:ugraph, -Element1, -Element2) is nondet.

identity(Poset, Element, Element):- !,
  vertex(Poset, Element).
identity(Poset, Element1, Element2):-
  edge(Poset, Element1-Element2),
  edge(Poset, Element2-Element1).


%! maximal(+Poset:ugraph, +Maximal) is semidet.
%! maximal(+Poset:ugraph, -Maximal) is nondet.

maximal(Poset, Maximal):-
  vertex(Poset, Maximal),
  forall(
    walk(Poset, Maximal, Element),
    identity(Poset, Maximal, Element)
  ).


%! maximum(+Poset:ugraph, +Maximum) is semidet.
%! maximum(+Poset:ugraph, -Maximum) is semidet.

maximum(Poset, Maximum):-
  maximal(Poset, Maximum),
  forall(
    maximal(Poset, Maximal),
    Maximal == Maximum
  ).


%! minimal(+Poset:ugraph, +Minimal) is semidet.
%! minimal(+Poset:ugraph, -Minimal) is nondet.

minimal(Poset, Minimal):-
  vertex(Poset, Minimal),
  forall(
    walk(Poset, Element, Minimal),
    identity(Poset, Element, Minimal)
  ).


%! minimum(+Poset:ugraph, +Minimum) is semidet.
%! minimum(+Poset:ugraph, -Minimum) is semidet.

minimum(Poset, Minimum):-
  minimal(Poset, Minimal),
  forall(
    minimal(Poset, Minimal),
    Minimal == Minimum
  ).


/*
%! partial_order(+Order:list(pair)) is semidet.
% Succeeds if the given order is
%   1. reflexive,
%   2. antisymmetric, and
%   3. transitive.
% @tbd How to implement the check for anti-symmetry?

partial_order(Order):-
  reflexive(Order),
  transitive(Order).
*/


%! poset(
%!   +PartiallyOrderedSet:ugraph,
%!   -Set:ordset,
%!   -Order:ordset(pair)
%! ) is det.
%! poset(
%!   -PartiallyOrderedSet:ugraph,
%!   +Set:ordset,
%!   +Order:ordset(pair)
%! ) is det.
% Here we make use of the fact that posets are represented as ugraphs.

poset(Poset, Set, Order):-
  relation(Poset, Set, Order).

