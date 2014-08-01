:- module(
  poset,
  [
    poset/3, % ?PartiallyOrderedSet:ugraph
             % ?Set:ordset
             % ?Order:ordset(pair)
  ]
).

/** <module> Poset

Support for partially ordered sets.

Design decisions:
  * Posets are represented by ugraphs (see [ugraph]).

@author Wouter Beek
@version 2014/07
*/

:- use_module(logic(relation)).



%! partial_order(+Order:list(pair)) is semidet.
% Succeeds if the given order is
%   1. reflexive,
%   2. antisymmetric, and
%   3. transitive.

partial_order(Order):-
  reflexive(Order).


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

