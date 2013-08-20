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
                  % ?Arguments1:list
                  % :Options:list(nvpair)
    dcg_multi//5 % :DCG_Body
                 % ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])])
                 % ?Arguments1:list
                 % ?Arguments2:list
                 % :Options:list(nvpair)
  ]
).

/** <module> DCG_MULTI

Call a DCG rule multiple times while aggregating the arguments.

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(library(option)).

% DCG_MULTI
:- meta_predicate(dcg_multi(2,?,?)).
%:- meta_predicate(dcg_multi(//,?,?)).
:- meta_predicate(dcg_multi(2,?,?,?)).
%:- meta_predicate(dcg_multi(//,?,?,?)).
:- meta_predicate(dcg_multi(2,?,:,?,?)).
%:- meta_predicate(dcg_multi(//,?,:,?,?)).
:- meta_predicate(dcg_multi(3,?,+,:,?,?)).
%:- meta_predicate(dcg_multi(//,?,+,:,?,?)).
:- meta_predicate(dcg_multi(4,?,+,+,:,?,?)).
%:- meta_predicate(dcg_multi(//,?,+,+,:,?,?)).
:- meta_predicate(dcg_multi_no_arguments(2,+,+,-,+,?,?)).
%:- meta_predicate(dcg_multi_no_arguments(//,+,+,-,+,?,?)).
% DCG_NONVAR
:- meta_predicate(dcg_multi_nonvar(3,+,+,-,+,+,?,?)).
%:- meta_predicate(dcg_multi_nonvar(//,+,+,-,+,+,?,?)).
:- meta_predicate(dcg_multi_nonvar(4,+,+,-,+,+,+,?,?)).
%:- meta_predicate(dcg_multi_nonvar(//,+,+,-,+,+,+,?,?)).
% DCG_VAR
:- meta_predicate(dcg_multi_var(3,+,+,-,+,?,?)).
%:- meta_predicate(dcg_multi_var(//,+,+,-,+,?,?)).
:- meta_predicate(dcg_multi_var(4,+,+,-,-,+,?,?)).
%:- meta_predicate(dcg_multi_var(//,+,+,-,-,+,?,?)).



% DCG_MULTI %

%! dcg_multi(:DCG)//
% @see dcg_multi//5

dcg_multi(DCG) -->
  dcg_multi(DCG, _Rep).

%! dcg_multi(:DCG, ?Repetitions:or([nonneg,pair(nonneg,or([nonneg,inf]))]))//
% @see dcg_multi//5

dcg_multi(DCG, Rep) -->
  dcg_multi(DCG, Rep, []).

%! dcg_multi(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   +Options:list(nvpair)
%! )//
% @see dcg_multi//5

dcg_multi(DCG, Rep, O1) -->
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_no_arguments(DCG, Max, 0, C, O2),
  {in_between(Min, Max, C)}.

%! dcg_multi(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   +Options:list(nvpair)
%! )//
% @see dcg_multi//5

dcg_multi(DCG, Rep, L1, O1) -->
  {nonvar(L1)}, !,
  {meta_options(is_meta, O1, O2)},
  % Apply conversion: atom_to_codes/2.
  {(atomic(L1), option(convert(Pred), O1) -> call(Pred, L1, L2) ; L2 = L1)},
  {repetition(Rep, Min, Max)},
  dcg_multi_nonvar(DCG, Max, 0, Count, L2, O2),
  {in_between(Min, Max, Count)}.
dcg_multi(DCG, Rep, L2, O1) -->
  {var(L2)}, !,
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_var(DCG, Min, Max, L1, O2),
  % Apply conversion: atom_to_codes/2.
  {(option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)}.

%! dcg_multi(
%!   :DCG_Rule,
%!   ?Repetition:or([nonneg,pair([nonneg,or([nonneg,inf])])]),
%!   -Arguments1:list,
%!   -Arguments2:list,
%!   +Options:list(nvpair)
%! )//
% The following options are supported:
%   * =|convert(:ConversionPredicate)|=
%   * =|separator(:SeparatorDCG)|=

dcg_multi(DCG, Rep, L1, M1, O1) -->
  {nonvar(L1), nonvar(M1)}, !,
  {meta_options(is_meta, O1, O2)},
  % Apply conversion: atom_to_codes/2.
  {(atomic(L1), option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)},
  {(atomic(M1), option(convert(Pred), O2) -> call(Pred, M1, M2) ; M2 = M1)},
  {repetition(Rep, Min, Max)},
  dcg_multi_nonvar(DCG, Max, 0, Count, L2, M2, O2),
  {in_between(Min, Max, Count)}.
dcg_multi(DCG, Rep, L2, M2, O1) -->
  {meta_options(is_meta, O1, O2)},
  {repetition(Rep, Min, Max)},
  dcg_multi_var(DCG, Min, Max, L1, M1, O2),
  % Apply conversion: atom_to_codes/2.
  {(option(convert(Pred), O2) -> call(Pred, L1, L2) ; L2 = L1)},
  {(option(convert(Pred), O2) -> call(Pred, M1, M2) ; M2 = M1)}.

% Zero arguments: no distinction between `var` and `nonvar`.
dcg_multi_no_arguments(_DCG, _Max, C, C, _O) --> [].
dcg_multi_no_arguments(DCG, Max, C1, C, O) -->
  dcg_call(DCG),
  % Process the separator, if any.
  ({option(separator(Separator), O)} -> Separator ; ""),
  % Check that counter does not exeed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_no_arguments(DCG, Max, C2, C, O).



% DCG_NONVAR %

% One argument.
dcg_multi_nonvar(DCG, Max, C1, C, [H1|T1], O) -->
  dcg_call(DCG, H1),
  % Process the separator, if any.
  ({option(separator(Separator), O)} -> Separator ; ""),
  % Check that counter does not exeed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_nonvar(DCG, Max, C2, C, T1, O).
dcg_multi_nonvar(_DCG, _Max, C, C, [], _O) --> [].

% Two arguments.
dcg_multi_nonvar(DCG, Max, C1, C, [H1|T1], [H2|T2], O) -->
  dcg_call(DCG, H1, H2),
  % Process the separator, if any.
  ({option(separator(Separator), O)} -> Separator ; ""),
  % Check that counter does not exeed maximum.
  {succ(C1, C2), greater_than_or_equal_to(Max, C2)},
  dcg_multi_nonvar(DCG, Max, C2, C, T1, T2, O).
dcg_multi_nonvar(_DCG, _Max, C, C, [], [], _O) --> [].



% DCG_VAR %

% One argument.
dcg_multi_var(DCG, Min, Min, [H1], _O) --> !,
  dcg_call(DCG, H1).
dcg_multi_var(DCG, Min, Max1, [H1|T1], O) -->
  dcg_call(DCG, H1),
  % Process the separator, if any.
  ({option(separator(Separator), O), T1 \= []} -> Separator ; ""),
  {count_down(Max1, Max2)},
  dcg_multi_var(DCG, Min, Max2, T1, O).
dcg_multi_var(_DCG, _Min, inf, [], _O) --> [].

% Two arguments
dcg_multi_var(DCG, Min, Min, [H1], [H2], _O) --> !,
  dcg_call(DCG, H1, H2).
dcg_multi_var(DCG, Min, Max1, [H1|T1], [H2|T2], O) -->
  dcg_call(DCG, H1, H2),
  % Process the separator, if any.
  ({option(separator(Separator), O), T1 \= []} -> Separator ; ""),
  {count_down(Max1, Max2)},
  dcg_multi_var(DCG, Min, Max2, T1, T2, O).
dcg_multi_var(_DCG, _Min, inf, [], [], _O) --> [].



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

repetition(Rep, Min, Max):-
  repetition_(Rep, Min, Max),
  greater_than_or_equal_to(Max, Min).
repetition_(N, N, N):-
  integer(N), !.
repetition_(Min1-Max1, Min2, Max2):-
  default(Min1, 0, Min2),
  default(Max1, inf, Max2).

