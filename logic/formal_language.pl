:- module(
  formal_language,
  [
    add_individual_constant/2, % +Name:atom
                               % +Object
    add_predicate_tuple/2, % +Name:atom
                           % +Objects:list
    algebraic_signature/1, % +Signature:compound
    atomic_formula/1, % ?Formula:compound
    formula/1 , % ?Formula:compound
    individual_constant/1, % ?Name:atom
    individual_constant/2, % ?Name:atom
                           % ?Object
    logical_connective/1, % ?Name:atom
    logical_connective/2, % ?Name:atom
                          % ?Arity:positive_integer
    operator_depth/2, % +Formula:compound
                      % ?Depth:nonneg
    predicate/2, % ?Name:atom
                 % ?Objects:list
    predicate_arity/2, % ?Name:atom
                       % ?Arity:positive_integer
    relational_signature/1, % +Signature:compound
    signature/1, % -Signature:compound
    subformula/2, % ?Subformula:compound
                  % +Formula:compound
    term/1 % ?Term:compound
  ]
).

/** <module> Formal language

@author Wouter Beek
@tbd Add support for function symbols.
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(plunit)).

:- use_module(generics(db_ext)).
:- use_module(generics(typecheck)).
:- use_module(math(math_ext)).

:- op(900, xf,  ¬). % U+00AC
:- op(900, xfx, ∧). % U+2227
:- op(900, xfx, ∨). % U+2228
:- op(900, xfx, ⊕). % U+2295
:- op(900, xfx, →). % U+2192
:- op(900, xfx, ↔). % U+2194

%! individual_constant(+Name:atom, +Object) is semidet.
%! individual_constant(+Name:atom, -Object) is semidet.
%! individual_constant(-Name:atom, +Object) is nondet.
%! individual_constant(-Name:atom, -Object) is multi.
% Individual constants.
% Initially only the truth values.

:- dynamic(individual_constant/2).
%individual_constant(true, 'Truth').
%individual_constant(false, 'Falsity').

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

%! predicate(+Name, +Objects:list) is semidet.
%! predicate(+Name, -Objects:list) is nondet.

:- dynamic(predicate/2).



%! add_individual_constant(+Name:atom, +Object) is det.
% Adds the extension of the given individual constant symbol.

add_individual_constant(Name, Object):-
  db_add_novel(individual_constant(Name, Object)).


%! add_predicate_tuple(+Name:atom, +Objects:list) is det.
% Adds a tuple to the extension of the given predicate symbol.
%
% @throws instantiation_error if `Name` is uninstantiated.
% @throws instantiation_error if `Objects` is uninstantiated.
% @throws type_error if `Objects` is not a list.
% @throws domain_error if `Objects` is the empty list.

add_predicate_tuple(Name, _):-
  var(Name), !,
  instantiation_error(Name).
add_predicate_tuple(Name, Objects):-
  var(Name), !,
  instantiation_error(Objects).
add_predicate_tuple(_, Objects):-
  \+ is_list(Objects), !,
  type_error(list, Objects).
add_predicate_tuple(Name, Objects):-
  length(Objects, Arity),
  add_predicate_tuple(Name, Arity, Objects).

%! add_predicate_tuple(
%!   +Name:atom,
%!   +Arity:positive_integer,
%!   +Objects:list
%! ) is det.
% @throws domain_error if `Artiy` is zero,
%         i.e. `Objects` is the empty list.

% Tuples for this predicate exists:
% check whether they have the same arity.
add_predicate_tuple(Name, Arity, Objects):-
  predicate_arity(Name, Arity), !,
  length(Objects, Arity),
  db_add_novel(predicate(Name, Objects)).
% Tuples for this predicate do not yet exist:
% check whether the arity is acceptable.
add_predicate_tuple(_, Arity, _):-
  \+ positive_integer(Arity), !,
  domain_error(positive_integer, Arity).
add_predicate_tuple(Name, _, Objects):-
  db_add_novel(predicate(Name, Objects)).


%! algebraic_signature(+Signature:compound) is semidet.
% Succeeds if the given signature contains no predicate symbols.
%
% Silently fails for non-signature arguments.
%
% @throws instantiation_error(Signature)

algebraic_signature(Signature):-
  var(Signature), !,
  instantiation_error(Signature).
algebraic_signature(signature([],[_|_],_)).


%! atomic_formula(+Formula:compound) is semidet.
%! atomic_formula(-AtomicFormula:compound) is nondet.
% ### Instantiation `(+)`
%
% Succeeds if the given formula is an atomic formula
% with respect to the current formal language.
%
% ### Instantiation `(-)`
%
% Enumerates the atomic formulas that can be built
% based on the current formal language.
%
% There are no quarantees on the order in which
% atomic formulas are returned.

% Check whether something is an atomic formula.
atomic_formula(Formula):-
  nonvar(Formula), !,

  % An atomic formula is a compound term with a specified arity.
  % For example, this excludes `loves(andrea,wouter,teddy)`.
  functor(Formula, Name, Arity),
  % This ensures that arity > 0.
  predicate_arity(Name, Arity),

  % Make sure that all arguments are terms.
  % For example, this excludes `loves(dachshund(wouter),andrea)`.
  compound_name_arguments(Formula, Name, Terms),
  maplist(term, Terms).
% Generate an atomic formula.
atomic_formula(Formula):-
  predicate_arity(Name, Arity),
  between(1, Arity, _),
  length(Arguments, Arity),
  maplist(term, Arguments),
  compound_name_arguments(Formula, Name, Arguments).

:- begin_tests(atomic_formula).

test(
  'atomic_formula(+) is semidet. TRUE',
  [
    forall(atomic_formula_test(AtomicFormula,true)),
    setup(load_fl1)
  ]
):-
  atomic_formula(AtomicFormula).
test(
  'atomic_formula(+) is semidet. FAIL',
  [
    fail,
    forall(atomic_formula_test(AtomicFormula,fail)),
    setup(load_fl1)
  ]
):-
  atomic_formula(AtomicFormula).
test(
  'atomic_formula(-) is nondet.',
  [
    set(AtomicFormula == [
      dachshund(andrea),
      dachshund(teddy),
      dachshund(wouter),
      loves(andrea,andrea),
      loves(andrea,teddy),
      loves(andrea,wouter),
      loves(teddy,andrea),
      loves(teddy,teddy),
      loves(teddy,wouter),
      loves(wouter,andrea),
      loves(wouter,teddy),
      loves(wouter,wouter)
    ]),
    setup(load_fl1)
  ]
):-
  atomic_formula(AtomicFormula).

atomic_formula_test(dachshund(andrea), true).
atomic_formula_test(dachshund(teddy), true).
atomic_formula_test(dachshund(wouter), true).
atomic_formula_test(loves(andrea,andrea), true).
atomic_formula_test(loves(andrea,teddy), true).
atomic_formula_test(loves(andrea,wouter), true).
atomic_formula_test(loves(teddy,andrea), true).
atomic_formula_test(loves(teddy,teddy), true).
atomic_formula_test(loves(teddy,wouter), true).
atomic_formula_test(loves(wouter,andrea), true).
atomic_formula_test(loves(wouter,teddy), true).
atomic_formula_test(loves(wouter,wouter), true).

atomic_formula_test(loves(dachshund(wouter),wouter), fail).
atomic_formula_test(wouter(dachshund), fail).
atomic_formula_test(andrea, fail).
atomic_formula_test(dachshund(teddy,teddy), fail).
atomic_formula_test(loves(andrea,wouter,teddy), fail).

:- end_tests(atomic_formula).


%! formula(+Formula:compound) is semidet.
%! formula(-Formula:compound) is nondet.

formula(Formula):-
  formula(Formula, _).

%! formula(+Formula:compound, +Depth:nonneg) is semidet.
%! formula(+Formula:compound, -Depth:nonneg) is det.
%! formula(-Formula:compound, +Depth:nonneg) is nondet.
%! formula(-Formula:compound, -Depth:nonneg) is nondet.

% Atomic formulas have depth 0.
formula(AtomicFormula, 0):-
  atomic_formula(AtomicFormula).
% Recursive case: unary logical connective.
formula(Formula, Depth):-
  % If we do not perform this check, formula/2 will loop.
  (
    nonvar(Formula)
  ->
    % We cannot use compound_name_arguments/3 here,
    % since this would throw a type_error for
    % atomic instantiation of `Formula`.
    Formula =.. [UnaryConnective,Subformula]
  ;
    true
  ),

  % Make sure the logical connective is unary.
  logical_connective(UnaryConnective, 1),

  % Based on whether the operator depth is given,
  % we can generate the subformulas more/less efficient.
  (
    nonvar(Depth)
  ->
    succ(Subdepth, Depth),
    formula(Subformula, Subdepth)
  ;
    formula(Subformula, Subdepth),
    succ(Subdepth, Depth)
  ),

  % If we were not given a formula, we need to construct it
  % based on its subformulas.
  (
    var(Formula)
  ->
    compound_name_arguments(Formula, UnaryConnective, [Subformula])
  ;
    true
  ).
% Recursive case: binary logical connective.
formula(Formula, Depth):-
  (
    nonvar(Formula)
  ->
    % We cannot use compound_name_arguments/3 here,
    % since this would throw a type_error for
    % atomic instantiation of `Formula`.
    Formula =.. [BinaryConnective|Subformulas]
  ;
    true
  ),

  logical_connective(BinaryConnective, 2),

  (
    nonvar(Depth)
  ->
    succ(Subdepth1, Depth),
    formula(Subformula1, Subdepth1),
    betwixt(0, Subdepth1, Subdepth2),
    formula(Subformula2, Subdepth2)
  ;
    formula(Subformula1, Subdepth1),
    succ(Subdepth1, Depth),
    betwixt(0, Subdepth1, Subdepth2),
    formula(Subformula2, Subdepth2)
  ),
  (
    Subformulas = [Subformula1,Subformula2]
  ;
    Subformulas = [Subformula2,Subformula1]
  ),

  (
    var(Formula)
  ->
    compound_name_arguments(Formula, BinaryConnective, Subformulas)
  ;
    true
  ).


%! individual_constant(+Name:atom, +Object) is semidet.

individual_constant(Name):-
  individual_constant(Name, _).


%! logical_connective(+Name:atom) is semidet.
%! logical_connective(-Name:atom) is multi.

logical_connective(Name):-
  logical_connective(Name, _).


%! operator_depth(+Formula:compound, +Depth:nonneg) is semidet.
%! operator_depth(+Formula:compound, -Depth:nonneg) is det.

operator_depth(Formula, Depth):-
  formula(Formula, Depth).


%! predicate_arity(+Name, +Arity:positive_integer) is semidet.
%! predicate_arity(+Name, -Arity:positive_integer) is nondet.
%! predicate_arity(-Name, +Arity:positive_integer) is nondet.
%! predicate_arity(-Name, -Arity:positive_integer) is nondet.

% Check/find the arity for a given predicate.
% Instantiations `(+,+)` and `(+,-)`.
predicate_arity(Name, Arity):-
  nonvar(Name), !,
  once(predicate(Name, Objects)),
  length(Objects, Arity).
% Enumerate predicates.
% Instantiations `(-,+)` and `(-,-)`.
predicate_arity(Name, Arity):-
  % Since we want to avoid duplicate answers,
  % we first collect all predicate names.
  aggregate_all(
    set(Name),
    predicate(Name, _),
    Names
  ),
  % Choicepoint for predicate names.
  member(Name, Names),
  predicate_arity(Name, Arity).


%! relational_signature(+Signature:compound) is semidet.
% Succeeds if the given signature contains no function symbols.
%
% Silently fails for non-signature arguments.
%
% @throws instantiation_error(Signature)

relational_signature(Signature):-
  var(Signature), !,
  instantiation_error(Signature).
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

signature(signature(Predicates,FunctionSymbols,ArityFunction)):-
  aggregate_all(
    set(Name),
    predicate(Name, _),
    Predicates
  ),
  FunctionSymbols = [],
  maplist(predicate_arity, Predicates, Arities),
  pairs_keys_values(ArityFunction, Predicates, Arities).


%! subformula(+Subformula:compound, +Formula:compound) is semidet.
%! subformula(-Subformula:compound, +Formula:compound) is multi.
% @throws instantiation_error if `Formula` is uninstantiated.
% @throws type_error if `Formula` is not a compound term.

subformula(_, Formula):-
  var(Formula), !,
  instantiation_error(Formula).
subformula(_, Formula):-
  \+ compound(Formula), !,
  type_error(compound, Formula).
% Generative case. Instantiation `(-,+)`.
subformula(Subformula, Formula):-
  var(Subformula), !,
  % Mode `nondet`: allow backtracking.
  subformula0(Subformula, Formula).
% Checking case. Instantiaion `(+,+)`.
subformula(Subformula, Formula):-
  % Enforse determinism after first result (if any).
  % Otherwise mode would be `nondet` rather than `semidet`.
  subformula0(Subformula, Formula), !.

% Base case.
subformula0(Formula, Formula).
% Recursive case.
subformula0(Subsubformula, Formula):-
  compound_name_arguments(Formula, Name, Subformulas),
  logical_connective(Name),
  member(Subformula, Subformulas),
  subformula0(Subsubformula, Subformula).

:- begin_tests(subformula).

test(
  'subformula(+,+) is semidet. TRUE',
  [
    forall(subformula_test(Subformula, Formula, true)),
    setup(load_fl1)
  ]
):-
  subformula(Subformula, Formula).

subformula_test(dachshund(teddy), dachshund(teddy), true).

:- end_tests(subformula).


%! term(+Term:compound) is semidet.
%! term(-Term:compound) is multi.

term(IndividualConstant):-
  individual_constant(IndividualConstant).



% Debug

load_fl1:-
  maplist(add_individual_constant, [andrea,teddy,wouter], [obj1,obj2,obj3]),
  maplist(add_predicate_tuple(dachshund), [[teddy]]),
  maplist(
    add_predicate_tuple(loves),
    [
      [andrea,wouter],
      [andrea,teddy],
      [teddy,teddy],
      [wouter,andrea]
    ]
  ).

