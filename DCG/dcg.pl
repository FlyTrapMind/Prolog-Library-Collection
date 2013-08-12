:- module(
  dcg,
  [
    dcg//1, % :DCG_Body:dcg
    dcg//2, % :DCG_Body:dcg
            % +Repetition:or([integer,pair(integer,integer)])
    dcg//3, % :DCG_Body:dcg
            % +Repetition:or([integer,pair(integer,integer)])
            % -Arguments:list(list)
    dcg//4 % :DCG_Body:dcg
           % +Arity:nonneg
           % +Repetition:or([integer,pair(integer,integer)])
           % -Arguments:list(list)
  ]
).

/** <module> DCG_MULTI

Call a DCG rule multiple times while aggregating the arguments.

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(library(apply)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).

:- meta_predicate(dcg(//,+,+,-,?,?)).
:- meta_predicate(dcg(//,+,+,+,-,?,?)).
:- meta_predicate(dcg_call_(//,+,-,?,?)).



count_down(inf, inf):- !.
count_down(N1, N2):-
  succ(N2, N1).

dcg(ModDCG) -->
  dcg(ModDCG, Rep_-_).

dcg(ModDCG, Rep) -->
  dcg(ModDCG, Rep, _Lists).

dcg(ModDCG, Rep, Lists) -->
  {
    strip_module(ModDCG, Mod, DCG1),
    functor(DCG2, DCG1, Arity)
  },
  dcg(DCG, Arity, Rep, Lists).

dcg(DCG, Arity, Rep, Lists) -->
  {
    repetition(Rep, Min, Max),
    Max >= Min
  },
  dcg(DCG, Arity, Min, Max, Lists).

dcg(DCG, Arity, Min, Min, List) --> !,
  dcg_call_(DCG, Arity, Heads),
  {maplist(in_list, Heads, List)}.
dcg(DCG, Arity, Min, Max1, Lists) -->
  dcg_call_(DCG, Arity, Heads),
  {count_down(Max1, Max2)},
  dcg(DCG, Arity, Min, Max2, Tails),
  {maplist(prepend, Heads, Tails, Lists)}.

dcg_call_(ModDCG, Arity, Args) -->
  {
    strip_module(ModDCG, Mod, DCG1),
    functor(DCG2, DCG1, Arity)
  },
  dcg_call(Mod:DCG2),
  {DCG2 =.. [DCG1|Args]}.

in_list(X, [X]).

repetition(N, 1, N):-
  integer(N), !.
repetition(Min1-Max1, Min2, Max2):-
  default(Min1, 0, Min2),
  default(Max1, inf, Max2).

prepend(H, T, [H|T]).

