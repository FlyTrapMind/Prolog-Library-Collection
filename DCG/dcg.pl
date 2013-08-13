:- module(
  dcg,
  [
    dcg_nonvar//2, % :DCG_Body
                   % -Occurrences:nonneg
    dcg_nonvar//3, % :DCG_Body
                   % -Occurrences:nonneg
                   % -Arguments1:list
    dcg_nonvar//4, % :DCG_Body
                   % -Occurrences:nonneg
                   % -Arguments1:list
                   % -Arguments2:list
    dcg_var//2, % :DCG_Body
                % ?Repetition:or([integer,pair(integer,integer)])
    dcg_var//3, % :DCG_Body
                % ?Repetition:or([integer,pair(integer,integer)])
                % -Arguments:list
    dcg_var//4 % :DCG_Body
               % ?Repetition:or([integer,pair(integer,integer)])
               % -Arguments1:list
               % -Arguments2:list
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

:- meta_predicate(dcg_nonvar(2,-,?,?)).
%:- meta_predicate(dcg_nonvar(//,-,?,?)).
:- meta_predicate(dcg_nonvar(3,-,+,?,?)).
%:- meta_predicate(dcg_nonvar(//,-,+,?,?)).
:- meta_predicate(dcg_nonvar(4,-,+,+,?,?)).
%:- meta_predicate(dcg_nonvar(//,-,+,+,?,?)).
:- meta_predicate(dcg_var(2,?,?,?)).
%:- meta_predicate(dcg_var(//,?,?,?)).
:- meta_predicate(dcg_var(3,?,-,?,?)).
%:- meta_predicate(dcg_var(//,?,-,?,?)).
:- meta_predicate(dcg_var(4,?,-,-,?,?)).
%:- meta_predicate(dcg_var(//,?,-,-,?,?)).
:- meta_predicate(dcg_var_(2,+,+,?,?)).
%:- meta_predicate(dcg_var_(//,+,+,?,?)).
:- meta_predicate(dcg_var_(3,+,+,-,?,?)).
%:- meta_predicate(dcg_var_(//,+,+,-,?,?)).
:- meta_predicate(dcg_var_(4,+,+,-,-,?,?)).
%:- meta_predicate(dcg_var_(//,+,+,-,-,?,?)).



% DCG_NONVAR %

dcg_nonvar(ModDCG, N2) -->
  {strip_module(ModDCG, _Mod, DCG)},
  dcg_call(DCG),
  dcg_nonvar(ModDCG, N1),
  {succ(N1, N2)}.
dcg_nonvar(_ModDCG, 0) --> [].

dcg_nonvar(ModDCG, N2, [H|T]) -->
  {strip_module(ModDCG, Mod, DCG1)},
  {DCG2 =.. [DCG1,H]},
  dcg_call(Mod:DCG2),
  dcg_nonvar(ModDCG, N1, T),
  {succ(N1, N2)}.
dcg_nonvar(_ModDCG, 0, []) --> [].

dcg_nonvar(ModDCG, N2, [H1|T1], [H2|T2]) -->
  {strip_module(ModDCG, Mod, DCG1)},
  {DCG2 =.. [DCG1,H1,H2]},
  dcg_call(Mod:DCG2),
  dcg_nonvar(ModDCG, N1, T1, T2),
  {succ(N1, N2)}.
dcg_nonvar(_ModDCG, 0, [], []) --> [].



% DCG_VAR %

%! dcg_var(:DCG_Rule, ?Repetition)//

dcg_var(DCG, Rep) -->
  {repetition(Rep, Min, Max)},
  dcg_var_(DCG, Min, Max).

dcg_var(DCG, Rep, Arguments) -->
  {repetition(Rep, Min, Max)},
  dcg_var_(DCG, Min, Max, Arguments).

dcg_var(DCG, Rep, Arguments1, Arguments2) -->
  {repetition(Rep, Min, Max)},
  dcg_var_(DCG, Min, Max, Arguments1, Arguments2).

dcg_var_(DCG, Min, Min) --> !,
  dcg_call(DCG).
dcg_var_(DCG, Min, Max1) -->
  dcg_call(DCG),
  {count_down(Max1, Max2)},
  dcg_var_(DCG, Min, Max2).

dcg_var_(DCG, Min, Min, [H1]) --> !,
  dcg_call(DCG, H1).
dcg_var_(DCG, Min, Max1, [H1|T1]) -->
  dcg_call(DCG, H1),
  {count_down(Max1, Max2)},
  dcg_var_(DCG, Min, Max2, T1).
dcg_var_(_DCG, _Min, inf, []) --> [].

dcg_var_(DCG, Min, Min, [H1], [H2]) --> !,
  dcg_call(DCG, H1, H2).
dcg_var_(DCG, Min, Max1, [H1|T1], [H2|T2]) -->
  dcg_call(DCG, H1, H2),
  {count_down(Max1, Max2)},
  dcg_var_(DCG, Min, Max2, T1, T2).



% SUPPORT PREDICATES %

count_down(inf, inf):- !.
count_down(N1, N2):-
  succ(N2, N1).

greater_than_or_equal_to(inf, _):- !.
greater_than_or_equal_to(_, inf):- !, fail.
greater_than_or_equal_to(X, Y):-
  X >= Y.

repetition(Rep, Min, Max):-
  repetition_(Rep, Min, Max),
  greater_than_or_equal_to(Max, Min).
repetition_(N, 1, N):-
  integer(N), !.
repetition_(Min1-Max1, Min2, Max2):-
  default(Min1, 0, Min2),
  default(Max1, inf, Max2).
