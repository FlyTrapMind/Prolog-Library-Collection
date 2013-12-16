:- module(
  pair_ext,
  [
    pairs_to_members/2 % +Pairs:list(pair)
                       % -Members:list
  ]
).

/** <module> Pair extensions

Support predicates for working with pairs.

@author Wouter Beek
@version 2013/12
*/

:- use_module(library(apply)).
:- use_module(library(ordsets)).



pairs_to_members(Pairs, Members):-
  pairs_keys_values(Pairs, Keys1, Values1),
  maplist(sort, [Keys1,Values1], [Keys2,Values2]),
  ord_union(Keys2, Values2, Members).

