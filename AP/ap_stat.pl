:- module(
  ap_stat,
  [
    ap_debug/3, % +Options:list(nvpair)
                % +Message:atom
                % +Arguments:list
    ap_stage_done/2, % +Alias:atom
                     % +Stage:nonneg
    ap_stage_init/1, % +Potential:nonneg
    ap_stage_tick/0,
    ap_store_stage_alias/2 % +Alias:atom
                           % +Stage:or([nonneg,oneof([input,output])])
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



% DEBUG %

%! ap_debug(+Alias:atom, +Format, :Arguments) is det.

ap_debug(Alias, Msg1, Args):-
  iso8601_dateTime(Time),
  format(atom(Msg2), Msg1, Args),
  debug(ap, '[Time:~w][Alias:~w] ~w', [Time,Alias,Msg2]).



% EVALUATION %

%! ap_stage_done(+Alias:atom, +Stage:nonneg) is semidet.
% Succeeds if the given stage completed successfully.

ap_stage_done(Alias, Stage):-
  ap_stage_eval(Alias, Stage, X, X), !.
ap_stage_done(_, _):-
  debug(ap_stat, 'No stage to conclude.', []).

%! ap_stage_eval is det.

ap_stage_eval(Alias):-
  ap_stage_directories(Alias, StageDirs),
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
        ap_debug(_O1, '[Alias:~w,Stage:~w] ~w', [Alias,Stage,Bar])
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

ap_store_stage_alias(Stage, Alias):-
  thread_self(ThisThread),
  db_replace_novel(stage_alias(ThisThread, Alias, Stage), [r,r,r]).



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

