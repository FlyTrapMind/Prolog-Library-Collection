:- module(
  ap_stat,
  [
    ap_debug/3, % +Options:list(nvpair)
                % +Message:atom
                % +Arguments:list
    ap_stage_done/0, % +StageAlias:atom
    ap_stage_init/1, % +Potential:nonneg
    ap_stage_tick/0,
    ap_store_stage_alias/2 % +Options:list(nvpair)
                           % +Stage:or([nonneg,oneof([input,output])])
  ]
).

/** <module> Auto-process statistics

Statistics for tracking the progress of automated processes.

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_module(ap(ap_dir)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(os(datetime_ext)).

:- dynamic(stage_alias/2).

:- debug(ap_stat).



% DEBUG %

%! ap_debug(+Options:list(nvpair), +Format, :Arguments) is det.

ap_debug(O1, Msg1, Args):-
  iso8601_dateTime(Time),
  option(project(Project), O1, project),
  option(process(Process), O1, process),
  format(atom(Msg2), Msg1, Args),
  debug(
    ap,
    '[Time:~w][Project:~w][Process:~w] ~w',
    [Time,Project,Process,Msg2]
  ).



% EVALUATION %

%! ap_stage_done is semidet.
% Succeeds if the given stage completed successfully.

ap_stage_done:-
  ap_stage_alias(StageAlias), !,
  ap_stage_eval_(StageAlias, X, X).
ap_stage_done:-
  debug(ap_stat, 'No stage to conclude.', []).

%! ap_stage_eval is det.

ap_stage_eval:-
  ap_stage_alias(StageAlias), !,
  ap_stage_eval_(StageAlias, A, P),
  progress_bar(A, P, Bar),
  ap_debug(_O1, '[Stage:~w] ~w', [StageAlias,Bar]).
ap_stage_eval:-
  debug(ap_stat, 'No stage to evaluate.', []).

%! ap_stage_eval_(+StageAlias:atom, -Actual:nonneg, -Potential:nonneg) is det.

ap_stage_eval_(StageAlias, A, P):-
  atomic_list_concat([StageAlias,a], '_', FlagA),
  flag(FlagA, A, A),
  atomic_list_concat([StageAlias,p], '_', FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(Potential):-
  ap_stage_alias(StageAlias), !,
  
  % Create the potential flag.
  atomic_list_concat([StageAlias,p], '_', FlagP),
  flag(FlagP, _, Potential),

  % Create the actual flag.
  atomic_list_concat([StageAlias,a], '_', FlagA),
  flag(FlagA, _, 0),

  % Create the statistics tracking thread.
  % @tbd Not possible to set this yet.
  %option(stat_lag(Interval), O1, 10),
  intermittent_thread(
    ap_stage_eval,
    ap_stage_done,
    10000,
    _Id,
    [alias(StageAlias)]
  ).
ap_stage_init(_Potential):-
  debug(ap_stat, 'Unable to initialize stage.', []).

ap_store_stage_alias(O1, Stage):-
  ap_stage_alias(O1, Stage, StageAlias),
  thread_self(ThisThread),
  db_replace_novel([r,r], stage_alias(ThisThread, StageAlias)).



% FLAG UPDATES %

ap_stage_tick:-
  ap_stage_alias(StageAlias), !,
  atomic_list_concat([StageAlias,a], '_', Flag),
  flag(Flag, Actual, Actual + 1).
ap_stage_tick:-
  debug(ap_stat, 'Unable to tick stage.', []).



% GENERICS %

%! ap_stage_alias(-StageAlias:atom) is det.

ap_stage_alias(StageAlias):-
  thread_self(ThisThread),
  stage_alias(ThisThread, StageAlias).
