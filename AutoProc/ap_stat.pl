:- module(
  ap_stat,
  [
    ap_debug/3, % +Options:list(nvpair)
                % +Message:atom
                % +Arguments:list
    ap_process_eval/1, % +Options:list(nvpair)
    ap_stage_tick/1, % +ProcessStageAlias:atom
    ap_stage_done/2, % +Options:list(nvpair)
                          % +StageNumber:nonneg)
    ap_stage_eval/2, % +Options:list(nvpair)
                          % +StageNumber:nonneg)
    ap_stage_init/2 % +Options:list(nvpair)
                         % +StageNumber:nonneg
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



% FLAG EVALUATION %

%! ap_process_eval(+Options:list(nvpair)) is det.

ap_process_eval(O1):-
  ap_process_eval(O1, 0).

ap_process_eval(O1, StageNumber):-
  ap_stage_eval(O1, StageNumber), !,
  NextStageNumber is StageNumber + 1,
  ap_process_eval(O1, NextStageNumber).
ap_process_eval(_Process, _StageNumber).

%! ap_stage_done(+Options:list(nvpair), +StageNumber:nonneg) is semidet.
% Succeeds if the given stage completed successfully.

ap_stage_done(O1, StageNumber):-
  ap_stage_eval(O1, StageNumber, X, X).

%! ap_stage_eval(+Options:list(nvpair), +StageNumber:nonneg) is det.

ap_stage_eval(O1, StageNumber):-
  ap_stage_eval(O1, StageNumber, A, P),
  progress_bar(A, P, Bar),
  ap_debug(O1, '[Stage:~w] ~w', [StageNumber,Bar]).

%! ap_stage_eval(
%!   +Options:list(nvpair),
%!   +StageNumber:nonneg,
%!   -Actual:nonneg,
%!   -Potential:nonneg
%! ) is det.

ap_stage_eval(O1, StageNumber, A, P):-
  ap_flag_actual(O1, StageNumber, FlagA),
  flag(FlagA, A, A),
  ap_flag_potential(O1, StageNumber, FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(O1, StageNumber):-
  ap_stage_init_actual(O1, StageNumber),
  ap_stage_init_potential(O1, StageNumber),
  ap_stage_directory(O1, StageNumber, StageAlias),
  intermittent_thread(
    ap_stage_eval(O1, StageNumber),
    ap_stage_done(O1, StageNumber),
    10,
    _Id,
    [alias(StageAlias)]
  ).

ap_stage_init_actual(O1, StageNumber):-
  ap_flag_actual(O1, StageNumber, FlagA),
  option(actual(A), O1, 0),
  flag(FlagA, _, A).

ap_stage_init_potential(O1, StageNumber):-
  ap_flag_potential(O1, StageNumber, FlagP),
  option(potential(P), O1, 0),
  flag(FlagP, _, P).



% FLAG NAMES %

ap_flag_actual(O1, StageNumber, FlagA):-
  ap_flag_prefix(O1, StageNumber, Prefix),
  atomic_list_concat([Prefix,a], '_', FlagA).

ap_flag_potential(O1, StageNumber, FlagP):-
  ap_flag_prefix(O1, StageNumber, Prefix),
  atomic_list_concat([Prefix,p], '_', FlagP).

ap_flag_prefix(O1, StageNumber, Prefix):-
  option(project(Project), O1, project),
  option(process(Process), O1, process),
  ap_stage_name(StageNumber, StageName),
  atomic_list_concat([Project,Process,StageName], '_', Prefix).



% FLAG UPDATES %

ap_stage_tick(FlagA):-
  flag(FlagA, X, X + 1).

