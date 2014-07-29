:- module(
  formal_language,
  [
    algebraic_signature/1, % +Signature:compound
    atomic_formula/1, % ?Formula:compound
    formula/1 , % ?Formula:compound
    individual_constant/2, % ?Name:atom
                           % ?GroundInstance
    predicate/2, % ?Name:atom
                 % ?Arity:positive_integer
    predicate/3, % ?Name:atom
                 % ?Arity:positive_integer
                 % ?GroundTuple:list
    relational_signature/1, % +Signature:compound
    signature/1, % -Signature:compound
    term/1 % ?Term:compound
  ]
).

/** <module> Formal language

@author Wouter Beek
@tbd Function symbols.
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(pairs)).

:- op(900, xf,  ¬). % U+00AC
:- op(900, xfx, ∧). % U+2227
:- op(900, xfx, ∨). % U+2228
:- op(900, xfx, ⊕). % U+2295
:- op(900, xfx, →). % U+2192
:- op(900, xfx, ↔). % U+2194

%! individual_constant(+Name:atom, +GroundInstance) is semidet.
%! individual_constant(+Name:atom, -GroundInstance) is semidet.
%! individual_constant(-Name:atom, +GroundInstance) is nondet.
%! individual_constant(-Name:atom, -GroundInstance) is multi.
% Individual constants.
% Initially only the truth values.

:- dynamic(individual_constant/2).
individual_constant(true, 'Truth').
individual_constant(false, 'Falsity').

%! logical_connective(+Name:atom, +Arity:positive_integer) is semidet.
%! logical_connective(+Name:atom, -Arity:positive_integer) is semidet.
%! logical_connective(-Name:atom, +Arity:positive_integer) is nondet.
%! logical_connective(-Name:atom, -Arity:positive_integer) is multi.

:- dynamic(logical_connective/2).
logical_connective(¬, 1).
logical_connective(∧, 2).
logical_connective(∨, 2).
logical_connective(⊕, 2).
logical_connective(→, 2).
logical_connective(↔, 2).

%! predicate(+Name, +Arity:positive_integer, +GroundTurple:list) is semidet.
%! predicate(+Name, +Arity:positive_integer, -GroundTurple:list) is nondet.

:- dynamic(predicate/3).



%! algebraic_signature(+Signature:compound) is semidet.

algebraic_signature(signature([_|_],[],_)).


%! atomic_formula(+Formula:compound) is semidet.
%! atomic_formula(-AtomicFormula:compound) is nondet.

atomic_formula(Formula):-
  nonvar(Formula), !,
  compound_name_arity(Formula, Name, Arity),
  predicate(Name, Arity).
atomic_formula(AtomicFormula):-
  var(AtomicFormula),
  predicate(Name, Arity),
  findall(
    Argument,
    (
      between(1, Arity, _),
      term(Argument)
    ),
    Arguments
  ),
  compound_name_arguments(AtomicFormula, Name, Arguments),


%! formula(+Formula:compound) is semidet.
%! formula(-Formula:compound) is nondet.

formula(AtomicFormula):-
  atomic_formula(AtomicFormula).
formula(Formula):-
  (
    nonvar(Formula)
  ->
    logical_connective(UnaryConnective, 1),
    compound_name_arguments(Formula, UnaryConnective, [Subformula]).
    formula(Subformula)
  ;
    formula(Formula1),
    logical_connective(UnaryConnective, 1),
    compound_name_arguments(Formula, UnaryConnective, [Subformula])
  ).
formula(Formula):-
  (
    nonvar(Formula)
  ->
    compound_name_arguments(
      Formula,
      BinaryConnective,
      [Subformula1,Subformula2]
    ),
    logical_connective(BinaryConnective, 2),
    formula(Subformula1),
    formula(Subformula2)
  ;
    formula(Subformula1),
    formula(Subformula2),
    logical_connective(BinaryConnective, 2),
    compound_name_arguments(
      Formula,
      BinaryConnective,
      [Subformula1,Subformula2]
    )
  ).


%! predicate(+Name, +Arity:positive_integer) is semidet.
%! predicate(+Name, -Arity:positive_integer) is nondet.
%! predicate(-Name, +Arity:positive_integer) is nondet.
%! predicate(-Name, -Arity:positive_integer) is nondet.

predicate(Name, Arity):-
  once(predicate(Name, Arity, _)).


%! relational_signature(+Signature:compound) is semidet.

relational_signature(signature([_|_],[],_)).


%! signature(-Signature:compound) is det.
% Returns the signature of the current formal language.
%
% A signature has the following form
% ~~~{.pl}
% signature(
%   -Predicates:ordset(atom),
%   -FunctionSymbols:ordset(atom),
%   -ArityFunction:list(pair(atom,positive_integer))
% )
% ~~~
%
% @tbd Add support for function symbols.

signature(Predicates, FunctionSymbols, ArityFunction):-
  aggregate_all(
    set(Name),
    predicate(Name, _),
    Predicates
  ),
  FunctionSymbols = [],
  maplist(\Name^predicate(Name, Arity), Predicates, Arities),
  pairs_keys_values(ArityFunction, Predicates, Arities).


%! term(+Term:compound) is semidet.
%! term(-Term:compound) is multi.

term(IndividualConstant):-
  individual_constant(IndividualConstant).

