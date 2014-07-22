:- module(
  equiv,
  [
    number_of_equivalence_pairs/3 % +EquivalenceSets:list(ordset)
                                  % -NumberOfPairs:nonneg
                                  % +Options:list(nvpair)
  ]
).

/** <module> Equivalence

Support for equivalence relations.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declarations.

:- predicate_options(number_of_equivalence_pairs/3, 3, [
     pass_to(cardinality_to_number_of_pairs/3, 3)
   ]).
:- predicate_options(cardinality_to_number_of_pairs/3, 3, [
     reflexive(+boolean)
   ]).



%! number_of_equivalence_pairs(
%!   +EquivalenceSets:list(ordset),
%!   -NumberOfPairs:nonneg,
%!   +Options:list(nvpair)
%! ) is det.
% Returns the number of equivalence pairs that are encoded in
% the given collection of equivalence sets.
%
% The following options are supported:
%   * =|reflexive(+boolean)|=
%     Whether to count reflexive cases. Default: `true`.

number_of_equivalence_pairs(EqSets, NumberOfPairs, Options):-
  aggregate_all(
    sum(NumberOfPairs),
    (
      member(EqSet, EqSets),
      length(EqSet, Cardinality),
      cardinality_to_number_of_pairs(Cardinality, NumberOfPairs, Options)
    ),
    NumberOfPairs
  ).

cardinality_to_number_of_pairs(Cardinality, NumberOfPairs, Options):-
  NumberOfSymmetricAndTransitivePairs is Cardinality * (Cardinality - 1),
  (
    option(reflexive(true), Options, true)
  ->
    NumberOfPairs is NumberOfSymmetricAndTransitivePairs + Cardinality
  ;
    NumberOfPairs = NumberOfSymmetricAndTransitivePairs
  ).

