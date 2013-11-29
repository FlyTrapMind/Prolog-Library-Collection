:- module(
  ap_stat,
  [
    ap_debug/3, % +Options:list(nvpair)
                % +Message:atom
                % +Arguments:list
    ap_stage_done/1, % +StageAlias:atom
    ap_stage_init/2, % +StageAlias:atom
                     % +Potential:nonneg
    ap_stage_tick/1 % +StageAlias:atom
  ]
).

/** <module> Auto-process statistics

Statistics for tracking the progress of automated processes.

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(ap(ap_dir)).
:- use_module(generics(atom_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(os(datetime_ext)).

:- debug(ap).



% DEBUG %

%! ap_debug(+Options:list(nvpair), +Format, :Arguments) is det.

ap_debug(O1, Msg1, Args):-
  date_time(Time),
  option(project(Project), O1, project),
  option(process(Process), O1, process),
  format(atom(Msg2), Msg1, Args),
  debug(
    ap,
    '[Time:~w][Project:~w][Process:~w] ~w',
    [Time,Project,Process,Msg2]
  ).



% EVALUATION %

%! ap_stage_done(+StageAlias:atom) is semidet.
% Succeeds if the given stage completed successfully.

ap_stage_done(StageAlias):-
  ap_stage_eval_(StageAlias, X, X).

%! ap_stage_eval(+StageAlias:atom) is det.

ap_stage_eval(StageAlias):-
  ap_stage_eval_(StageAlias, A, P),
  progress_bar(A, P, Bar),
  ap_debug(_O1, '[Stage:~w] ~w', [StageAlias,Bar]).

%! ap_stage_eval_(+StageAlias:atom, -Actual:nonneg, -Potential:nonneg) is det.

ap_stage_eval_(StageAlias, A, P):-
  atomic_list_concat([StageAlias,a], '_', FlagA),
  flag(FlagA, A, A),
  atomic_list_concat([StageAlias,p], '_', FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(StageAlias, Potential):-
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
    ap_stage_eval(StageAlias),
    ap_stage_done(StageAlias),
    10000,
    _Id,
    [alias(StageAlias)]
  ).



% FLAG UPDATES %

ap_stage_tick(StageAlias):-
  atomic_list_concat([StageAlias,a], '_', Flag),
  flag(Flag, Actual, Actual + 1).

