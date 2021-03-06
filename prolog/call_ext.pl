:- module(
  call_ext,
  [
    call_n_sol/3,       % +N, :Select_1, :Goal_1
    concurrent_n_sols/3 % +N, :Select_1, :Goal_1
  ]
).

/** <module> Call extensions

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(lists)).
:- use_module(library(thread)).

:- meta_predicate
    call_n_sol(+, 1, 1),
    concurrent_n_sols(+, 1, 1).





%! call_n_sol(+N, :Select_1, :Goal_1) is det.

call_n_sol(N, Select_1, Goal_1) :-
  findnsols(N, X, call(Select_1, X), Xs),
  last(Xs, X),
  call(Goal_1, X).



%! concurrent_n_sols(+N, :Select_1, :Goal_1) is det.

concurrent_n_sols(N, Select_1, Mod:Goal_1) :-
  findnsols(N, Mod:Goal_0, (call(Select_1, X), Goal_0 =.. [Goal_1,X]), Goals),
  concurrent(N, Goals, []).
