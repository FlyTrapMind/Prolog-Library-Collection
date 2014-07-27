:- module(
  flag_ext,
  [
    state_init/1, % -State:compound
    state_read/2, % +State:compound
                  % -N:nonneg
    state_tick/1, % +State:compound
    state_tick/2, % +State:compound
                  % -N:nonneg
    temporarily_set_flag/3 % +Flag:atom
                           % +TemporaryValue
                           % :Goal
  ]
).

/** <module> Flag extensions

Extensions for flags and shared variables.

@author Wouter Beek
@version 2014/06-2014/07
*/

:- meta_predicate(temporarily_set_flag(+,+,0)).
:- meta_predicate(temporarily_set_existing_flag(+,+,+,0)).



state_init(state(0)).


state_read(State, N):-
  arg(1, State, N).


state_tick(State):-
  state_tick(State, _).

state_tick(State, C1):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).


%! temporarily_set_flag(+Flag:atom, +TemporaryValue, :Goal) is det.

temporarily_set_flag(Flag, TemporaryValue, Goal):-
  current_prolog_flag(Flag, MainValue), !,
  temporarily_set_existing_flag(Flag, MainValue, TemporaryValue, Goal).
temporarily_set_flag(Flag, Value, Goal):-
  create_prolog_flag(Flag, Value, []),
  temporarily_set_flag(Flag, Value, Goal).

temporarily_set_existing_flag(_, Value, Value, Goal):- !,
  call(Goal).
temporarily_set_existing_flag(Flag, MainValue, TemporaryValue, Goal):-
  setup_call_cleanup(
    set_prolog_flag(Flag, TemporaryValue),
    call(Goal),
    set_prolog_flag(Flag, MainValue)
  ).

