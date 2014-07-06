:- module(
  flag_ext,
    [
      call_every_n/3, % +Flag:atom
                      % +N:positive_integer
                      % :Goal
      reset_thread_flag/1, % +Flag:atom
      thread_flag/2, % +Flag:atom
                     % ?Current
      thread_flag/3, % +Flag:atom
                     % -Old
                     % +New
% State
      state_init/1, % -State:compound
      state_read/2, % +State:compound
                    % -N:nonneg
      state_tick/1, % +State:compound
      state_tick/2 % +State:compound
                   % -N:nonneg
    ]
  ).

/** <module> Flag extensions

Extensions for flags and shared variables.

@auhtor Wouter Beek
@version 2014/06
*/

:- meta_predicate(call_every_n(+,+,1)).



call_every_n(Flag, N, Goal):-
  thread_flag(Flag, M, M + 1),
  (
    M > 0,
    0 =:= M mod N
  ->
    call(Goal, M)
  ;
    true
  ).


%! reset_thread_flag(+Flag:atom) is det.

reset_thread_flag(Flag):-
  thread_self(Thread),
  flag(Thread-Flag, _, 0).


thread_flag(Flag, X):-
  thread_flag(Flag, X, X).

thread_flag(Flag, X, Y):-
  thread_self(Thread),
  flag(Thread-Flag, X, Y).



% State

state_init(state(0)).


state_read(State, N):-
  arg(1, State, N).


state_tick(State):-
  state_tick(State, _).

state_tick(State, C1):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).

