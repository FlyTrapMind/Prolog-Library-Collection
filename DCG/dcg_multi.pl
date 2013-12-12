:- module(
  dcg_multi,
  [
    dcg_multi//1, % :DCG_Body
    dcg_multi//2, % :DCG_Body
                  % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
    dcg_multi//3, % :DCG_Body
                  % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                  % :Options:list(nvpair)
    dcg_multi//4, % :DCG_Body
                  % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                  % :Options:list(nvpair)
                  % -Count:nonneg
% SINGLE ARGUMENT
    dcg_multi1//2, % :DCG_Body
                   % ?Arguments1:list
    dcg_multi1//3, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
    dcg_multi1//4, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
                   % :Options:list(nvpair)
    dcg_multi1//5, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
                   % :Options:list(nvpair)
                   % -Count:nonneg
% TWO ARGUMENTS
    dcg_multi2//3, % :DCG_Body
                   % ?Arguments1:list
                   % ?Arguments2:list
    dcg_multi2//4, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
                   % ?Arguments2:list
    dcg_multi2//5, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
                   % ?Arguments2:list
                   % :Options:list(nvpair)
    dcg_multi2//6, % :DCG_Body
                   % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                   % ?Arguments1:list
                   % ?Arguments2:list
                   % :Options:list(nvpair)
                   % -Count:nonneg
% OUTPUT FORMAT MODIFIERS
    codes_atom/2, % ?Codes:list(code)
                  % ?Atom:atom
    codes_number/2 % ?Codes:list(code)
                   % ?Number:number
  ]
).

/** <module> DCG_MULTI

Call a DCG rule multiple times while aggregating the arguments.

@author Wouter Beek
@version 2013/08-2013/09, 2013/12
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(option)).

:- meta_predicate(dcg_multi(2,?,?)).
:- meta_predicate(dcg_multi(2,?,?,?)).
:- meta_predicate(dcg_multi(2,?,:,?,?)).
:- meta_predicate(dcg_multi(2,?,:,-,?,?)).
:- meta_predicate(dcg_multi1(3,+,?,?)).
:- meta_predicate(dcg_multi1(3,?,+,?,?)).
:- meta_predicate(dcg_multi1(3,?,+,:,?,?)).
:- meta_predicate(dcg_multi1(3,?,+,:,-,?,?)).
:- meta_predicate(dcg_multi2(4,+,+,?,?)).
:- meta_predicate(dcg_multi2(4,?,+,+,?,?)).
:- meta_predicate(dcg_multi2(4,?,+,+,:,?,?)).
:- meta_predicate(dcg_multi2(4,?,+,+,:,-,?,?)).
:- meta_predicate(dcg_multi_no_arguments(2,+,+,-,+,?,?)).
:- meta_predicate(dcg_multi_nonvar(3,+,+,-,+,+,?,?)).
:- meta_predicate(dcg_multi_nonvar(4,+,+,-,+,+,+,?,?)).
:- meta_predicate(dcg_multi_var(3,+,+,-,+,?,?)).
:- meta_predicate(dcg_multi_var(4,+,+,-,-,+,?,?)).



% DCG_MULTI %

%! dcg_multi(:DCG)//
% @see dcg_multi//5

dcg_multi(DCG) -->
  dcg_multi(DCG, _Rep).

%! dcg_multi(:DCG, ?Repetitions:or([nonneg,pair(nonneg,or([nonneg,inf]))]))//
% @see dcg_multi//5

dcg_multi(DCG, Rep) -->
  dcg_multi(DCG, Rep, []).

dcg_multi(DCG, Rep, O1) -->
  dcg_multi(DCG, Rep, O1, _C).

%! dcg_multi(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   +Options:list(nvpair),
%!   -Count:nonneg
%! )//
% @see Like dcg_multi//5 but with no arguments.

dcg_multi(DCG, Rep, O1, C) -->
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_no_arguments(DCG, Max, 0, C, O2),
  {in_between(Min, Max, C)}.
  % There used to be a cut here, but this is not correct.
  % For example, the following has two solutions:
  % ~~~{.pl}
  % phrase(dcg_multi((a_lowercase ; b_lowercase), 1-1), X).
  % ~~~



% DCG_MULTI1 %

%! dcg_multi1(:DCG_Rule, -Arguments1:list)//
% @see dcg_multi1//5

dcg_multi1(DCG, L1) -->
  dcg_multi1(DCG, _Rep, L1).

%! dcg_multi1(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
%!   -Arguments1:list
%! )//
% @see dcg_multi1//5

dcg_multi1(DCG, Rep, L1) -->
  dcg_multi1(DCG, Rep, L1, []).

%! dcg_multi1(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list
%!   -Count:nonneg
%! )//
% @see dcg_multi1//5

dcg_multi1(DCG, Rep, L1, O1) -->
  dcg_multi1(DCG, Rep, L1, O1, _C).

%! dcg_multi1(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   +Options:list(nvpair),
%!   -Count:nonneg
%! )//
% @see Like dcg_multi2//6 but wich a single argument.

dcg_multi1(DCG, Rep, L1, O1, C) -->
  {nonvar(L1)}, !,
  {meta_options(is_meta, O1, O2)},
  % Apply conversion: atom_to_codes/2.
  {(atomic(L1), option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)},
  {repetition(Rep, Min, Max)},
  dcg_multi_nonvar(DCG, Max, 0, C, L2, O2),
  {in_between(Min, Max, C)}, !.
dcg_multi1(DCG, Rep, L2, O1, C) -->
  {var(L2)}, !,
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_var(DCG, Min, Max, L1, O2),
  {length(L1, C), in_between(Min, Max, C)},
  % Apply conversion: atom_to_codes/2.
  {(option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)}, !.



% DCG_MULTI2 %

%! dcg_multi2(:DCG_Rule, -Arguments1:list, -Arguments2:list)//
% @see dcg_multi2//6

dcg_multi2(DCG_Rule, L1, L2) -->
  dcg_multi2(DCG_Rule, _Rep, L1, L2).

%! dcg_multi2(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   -Arguments2:list
%! )//
% @see dcg_multi2//6

dcg_multi2(DCG_Rule, Rep, L1, L2) -->
  dcg_multi2(DCG_Rule, Rep, L1, L2, []).

%! dcg_multi2(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   -Arguments2:list,
%!   +Options:list(nvpair)
%! )//
% @see dcg_multi2//6

dcg_multi2(DCG_Rule, Rep, L1, L2, O1) -->
  dcg_multi2(DCG_Rule, Rep, L1, L2, O1, _C).

%! dcg_multi2(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   -Arguments2:list,
%!   +Options:list(nvpair),
%!   -Count:nonneg
%! )//
% Executes the given DCG rule for the given number of repetitions,
% and applied to the given two argument lists.
%
% The following options are supported:
%   * =|convert(:ConversionPredicate)|=
%   * =|separator(:SeparatorDCG)|=

dcg_multi2(DCG, Rep, L1, M1, O1, C) -->
  {nonvar(L1), nonvar(M1)}, !,
  {meta_options(is_meta, O1, O2)},
  % Apply conversion: atom_to_codes/2.
  {(atomic(L1), option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)},
  {(atomic(M1), option(convert(Pred), O2) -> call(Pred, M1, M2) ; M2 = M1)},
  {repetition(Rep, Min, Max)},
  dcg_multi_nonvar(DCG, Max, 0, C, L2, M2, O2),
  {in_between(Min, Max, C)}, !.
dcg_multi2(DCG, Rep, L2, M2, O1, C) -->
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_var(DCG, Min, Max, L1, M1, O2),
  % Apply conversion: atom_to_codes/2.
  {(option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)},
  {(option(convert(Pred), O2) -> call(Pred, M1, M2) ; M2 = M1)}, !,
  {length(L1, C)}.



% DCG NONVAR ARGUMENTS %

% One argument.
dcg_multi_nonvar(DCG, Max, C1, C3, [H1|T1], O1) -->
  dcg_call(DCG, H1),
  % Process the separator, if any.
  ({option(separator(Separator), O1)} -> Separator ; ""),
  % Check that counter does not exeed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_nonvar(DCG, Max, C2, C3, T1, O1).
dcg_multi_nonvar(DCG, Max, C1, C2, [H1], _O1) -->
  dcg_call(DCG, H1),
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)}.
dcg_multi_nonvar(_DCG, _Max, C, C, [], _O1) -->
  [].

% Two arguments.
dcg_multi_nonvar(DCG, Max, C1, C, [H1|T1], [H2|T2], O1) -->
  dcg_call(DCG, H1, H2),
  % Process the separator, if any.
  ({option(separator(Separator), O1)} -> Separator ; ""),
  % Check that counter does not exeed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_nonvar(DCG, Max, C2, C, T1, T2, O1).
dcg_multi_nonvar(DCG, Max, C1, C2, [H1], [H2], _O1) -->
  dcg_call(DCG, H1, H2),
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)}.
dcg_multi_nonvar(_DCG, _Max, C, C, [], [], _O1) -->
  [].



% DCG VAR ARGUMENTS %

% One argument.
dcg_multi_var(DCG, Min, Max1, [H1|T1], O1) -->
  dcg_call(DCG, H1),
  % Process the separator, if any.
  ({option(separator(Separator), O1), T1 \= []} -> Separator ; ""),
  {count_down(Max1, Max2)},
  dcg_multi_var(DCG, Min, Max2, T1, O1).
dcg_multi_var(DCG, Min, Min, [H1], _O1) --> !,
  dcg_call(DCG, H1).
dcg_multi_var(_DCG, _Min, inf, [], _O1) -->
  [].

% Two arguments
dcg_multi_var(DCG, Min, Max1, [H1|T1], [H2|T2], O1) -->
  dcg_call(DCG, H1, H2),
  % Process the separator, if any.
  ({option(separator(Separator), O1), T1 \= []} -> Separator ; ""),
  {count_down(Max1, Max2)},
  dcg_multi_var(DCG, Min, Max2, T1, T2, O1).
dcg_multi_var(DCG, Min, Min, [H1], [H2], _O1) --> !,
  dcg_call(DCG, H1, H2).
dcg_multi_var(_DCG, _Min, inf, [], [], _O1) -->
  [].



% DCG NO ARGUMENTS %

% Zero arguments: no distinction between `var` and `nonvar`.
dcg_multi_no_arguments(_DCG, _Max, C, C, _O1) -->
  [].
dcg_multi_no_arguments(DCG, Max, C1, C3, O1) -->
  dcg_call(DCG),
  % Process the separator, if any.
  ({C1 =\= 0, option(separator(Separator), O1)} -> Separator ; ""),
  % Check that counter does not exceed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_no_arguments(DCG, Max, C2, C3, O1).



% SUPPORT PREDICATES %

codes_atom(Codes, Atom):-
  atom_codes(Atom, Codes).

codes_number(Codes, Number):-
  number_codes(Number, Codes).

count_down(inf, inf):- !.
count_down(N1, N2):-
  succ(N2, N1).

greater_than_or_equal_to(inf, _):- !.
greater_than_or_equal_to(_, inf):- !, fail.
greater_than_or_equal_to(X, Y):-
  X >= Y.

in_between(Min, Max, N):-
  greater_than_or_equal_to(N, Min),
  greater_than_or_equal_to(Max, N).

is_meta(convert).
is_meta(separator).

%! is_repetition_value(+Value) is semidet.
% Succeeds if the given value could be used to designate
% a DCG-multi repetition interval,
% i.e., is either an integer or the atom `inf`.

is_repetition_value(V):-
  nonneg(V), !.
is_repetition_value(V):-
  V == inf.

%! repetition(
%!   +Repetitions:or([pair(or([nonneg,oneof([inf])])),or([nonneg,oneof([inf])])]),
%!   -Minimum:nonneg,
%!   -Maximum:nonneg
%! ) is det.
% Determines a repetition interval for DCG-multi.
%
% ## Examples
%
% ~~~
% ?- dcg_multi:repetition(10, Min, Max).
% Min = Max, Max = 10.
% ~~~
%
% ~~~
% ?- dcg_multi:repetition(45-_, Min, Max).
% Min = 45,
% Max = inf.
% ~~~

repetition(Rep, Min2, Max2):-
  (
    Rep == 0
  ->
    Min2 = 0,
    Max2 = 0
  ;
    % A single value.
    is_repetition_value(Rep)
  ->
    Min2 = 1,
    Max2 = Rep
  ;
    Rep = Min1-Max1,
    default(Min1, 0, Min2),
    is_repetition_value(Min2),
    default(Max1, inf, Max2),
    is_repetition_value(Max2)
  ),
  greater_than_or_equal_to(Max2, Min2).

