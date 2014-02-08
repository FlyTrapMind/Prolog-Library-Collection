:- module(
  ap_stat,
  [
    ap_stage_init/1, % +Potential:nonneg
    ap_stage_tick/0
  ]
).

/** <module> Auto-process statistics

Statistics for tracking the progress of automated processes.

@author Wouter Beek
@version 2013/10-2014/01
*/

:- use_module(ap(ap_dir)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(os(datetime_ext)).

:- dynamic(stage_alias/3).



%! ap_stage_eval is det.

ap_stage_eval(AP):-
  ap_dirs(AP, StageDirs),
  (
    StageDirs == []
  ->
    debug(ap_stat, 'No stage to evaluate.', [])
  ;
    length(StageDirs, Length),
    forall(
      between(0, Length, Stage),
      (
        ap_stage_eval(Alias, Stage, A, P),
        progress_bar(A, P, Bar),
        debug(ap, '[Alias:~w,Stage:~w] ~w', [Alias,Stage,Bar])
      )
    )
  ).

%! ap_stage_eval(
%!   +Alias:atom,
%!   +Stage:nonneg,
%!   -Actual:nonneg,
%!   -Potential:nonneg
%! ) is det.

ap_stage_eval(Alias, Stage, A, P):-
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, A, A),
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(Potential):-
  ap_stage_alias(Alias, Stage), !,

  % Create the potential flag.
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, _, Potential),

  % Create the actual flag.
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, _, 0),

  % Create the statistics tracking thread.
  % @tbd Not possible to set this yet.
  %option(stat_lag(Interval), O1, 10),
  intermittent_thread(
    ap_stage_eval,
    ap_stage_done(Alias, Stage),
    10000,
    _Id,
    []
  ).
ap_stage_init(_Potential):-
  debug(ap_stat, 'Unable to initialize stage.', []).



% FLAG UPDATES %

ap_stage_tick:-
  ap_stage_alias(Alias, Stage), !,
  atomic_list_concat([Alias,Stage,a], '_', Flag),
  flag(Flag, Actual, Actual + 1).
ap_stage_tick:-
  debug(ap_stat, 'Unable to tick stage.', []).



% GENERICS %

%! ap_stage_alias(-Alias:atom, -Stage:atom) is det.

ap_stage_alias(Alias, Stage):-
  thread_self(ThisThread),
  stage_alias(ThisThread, Alias, Stage).

