:- module(
  flag_ext,
    [
      init_call_every_n/1, % +Flag:atom
      call_every_n/3, % +Flag:atom
                      % +N:positive_integer
                      % :Goal
      thread_flag/3 % +Flag:atom
                    % -Old
                    % +New
    ]
  ).

/** <module> Flag extensions

Extensions for flags and shared variables.

@auhtor Wouter Beek
@version 2014/06
*/

:- meta_predicate(call_every_n(+,+,1)).



init_call_every_n(Flag):-
  thread_self(Thread),
  flag(Thread-Flag, _, 0).


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


thread_flag(Flag, X, Y):-
  thread_self(Thread),
  flag(Thread-Flag, X, Y).

