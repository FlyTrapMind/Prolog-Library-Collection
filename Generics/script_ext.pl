:- module(
  script_ext,
  [
    script_begin/1, % +Process:atom
    script_end/1, % +Process:atom
    script_stage/3, % +Process:atom
                    % +Stage:nonneg
                    % :Goal
    script_stage/5, % +Process:atom
                    % +Stage:nonneg
                    % :Goal
                    % ?FromFileName:atom
                    % ?ToFileName:atom
    script_stage_eval/2, % +Process:atom
                         % +Stage:nonneg
    script_stage_overview/1, % +Process:atom
    script_stage_tick/2 % +Process:atom
                        % +Stage:nonneg
  ]
).

/** <module> Script extensions

Extensions for running automated scripts in stages.

@author Wouter Beek
@version 2013/06, 2013/10
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- meta_predicate(script_stage(+,+,3)).
:- meta_predicate(script_stage(+,+,3,+,+)).

:- debug(script_ext).



%! find_last_stage(-LastStageDirectory:atom) is det.

find_last_stage(LastStageDir):-
  find_stage_directories(StageDirs),
  last(StageDirs, LastStageDir).

%! find_stage_directories(-StageDirectories:list(atom)) is det.

find_stage_directories(StageDirs):-
  find_stage_directories(StageDirs, 1).

%! find_stage_directories(-StageDirectories:list(atom), +Stage:nonneg) is det.

find_stage_directories([H|T], Stage):-
  atomic_list_concat([stage,Stage], '_', StageName),
  absolute_file_name2(
    data(StageName),
    H,
    [access(write),file_type(directory)]
  ),
  NextStage is Stage + 1,
  find_stage_directories(T, NextStage).
find_stage_directories([], _Stage):- !.

%! init_data_directory is det.
% Makes sure there exists a `Data` subdirectory of the current project.

init_data_directory:-
  file_search_path(data, _DataDir), !.
init_data_directory:-
  create_project_subdirectory('Data', DataDir),
  db_add_novel(user:file_search_path(data, DataDir)).

%! script_begin(+Process:atom) is det.

script_begin(Process):-
  date_time(Start),
  debug(script_ext, '~w script started at ~w.', [Process,Start]),
  init_data_directory,
  script_clean,
  create_nested_directory(data('Output'), OutputDir),
  db_add_novel(user:file_search_path(output, OutputDir)).

%! script_clean is det.
% This is run after results have been saved to the `Output` directory.

script_clean:-
  find_stage_directories(StageDirs),
  maplist(safe_delete_directory_contents, StageDirs).

%! script_end(+Process:atom) is det.
% End the script, saving the results to the `Output` directory.

script_end(Process):-
  find_last_stage(FromDir),
  absolute_file_name(
    output('.'),
    ToDir,
    [access(write),file_type(directory)]
  ),
  safe_copy_directory(FromDir, ToDir),
  script_clean,
  date_time(End),
  debug(stcn, '~w script ended at ~w.', [Process,End]).

%! script_stage(+Process:atom, +Stage:nonneg, :Goal) is det.

script_stage(Process, Stage, Goal):-
  stage_directory(Stage, FromDir),
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  call(Goal, Process-Stage, FromDir, ToDir),
  debug(script_ext, '~w stage ~w is done.', [Process,Stage]).

%! script_stage(
%!   +Process:atom,
%!   +Stage:nonneg,
%!   :Goal,
%!   ?FromFileName:atom,
%!   ?ToFileName:atom
%! ) is det.
% `Goal` receives the from and to files as arguments.

% This stage was already performed in the past, since the to file is
% already in the next stage directory.
script_stage(Process, Stage, _Goal, _FromFileName, ToFileName):-
  nonvar(ToFileName),
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  absolute_file_name2(
    ToFileName,
    _ToFile,
    [access(read),relative_to(ToDir)]
  ), !,
  debug(script_ext, '~w stage ~w skipped.', [Process,Stage]).
% This stage has not been perfomed yet.
script_stage(Process, Stage, Goal, FromFileName, ToFileName):-
  % From the previous stage.
  stage_directory(Stage, FromDir),
  (
    var(FromFileName)
  ->
    % Read from the previous stage directory.
    access_file(FromDir, read),
    FromArg = FromDir
  ;
    % Read the from file located in the previous stage directory.
    absolute_file_name2(
      FromFileName,
      FromArg,
      [access(read),relative_to(FromDir)]
    )
  ),
  
  % To the next stage.
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  (
    var(ToFileName)
  ->
    % Write to the next stage directory.
    access_file(ToDir, write),
    ToArg = ToDir
  ;
    % Write to the to file located in the next stage directory.
    absolute_file_name2(
      ToFileName,
      ToArg,
      [access(write),relative_to(ToDir)]
    )
  ),
  
  script_stage_begin(Process, Stage),
  % Execute goal on the from and to arguments (either files or directories).
  call(Goal, Process-Stage, FromArg, ToArg),
  script_stage_end(Process, Stage).

script_stage_begin(Process, Stage):-
  format(atom(FlagA), '~w_~w_a', [Process,Stage]),
  flag(FlagA, _, 0),
  format(atom(FlagP), '~w_~w_p', [Process,Stage]),
  flag(FlagP, _, 0).

script_stage_end(Process, Stage):-
  script_stage_eval(Process, Stage).

script_stage_eval(Process, Stage):-
  script_stage_progress(Process, Stage, A, P),
  progress_bar(A, P, Bar),
  debug(script_ext, '[~w:~w] ~w', [Process,Stage,Bar]).

%! script_stage_overview(+Process:atom) is det.

script_stage_overview(Process):-
  script_stage_overview(Process, 0).

%! script_stage_overview(+Process:atom, +Stage:nonneg) is det.

script_stage_overview(Process, Stage):-
  script_stage_eval(Process, Stage), !,
  NextStage is Stage + 1,
  script_stage_overview(Process, Stage).
script_stage_overview(_Process, _Stage).

%! script_stage_progress(
%!   +Process:atom,
%!   +Stage:nonneg,
%!   -Actual:nonneg,
%!   -Potential:nonneg
%! ) is det.

script_stage_progress(Process, Stage, A, P):-
  format(atom(FlagA), '~w_~w_a', [Process,Stage]),
  flag(FlagA, A, A),
  format(atom(FlagP), '~w_~w_p', [Process,Stage]),
  flag(FlagP, P, P),
  (
    P == 0
  ->
    Perc = 100.0
  ;
    Perc is A / P * 100
  ).

script_stage_tick(Process, Stage):-
  format(atom(Flag), '~w_~w_a', [Process,Stage]),
  flag(Flag, X, X + 1).

%! stage_directory(+Stage:nonneg, -StageDirectory:atom) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% In stage 0 we use stuff form the `Input` directory.
stage_directory(0, StageDir):- !,
  create_nested_directory(data('Input'), StageDir),
  db_add_novel(user:file_search_path(data_input, StageDir)).
% For stages `N >= 1` we use stuff from directories called `stage_N`.
stage_directory(Stage, StageDir):-
  atomic_list_concat([stage,Stage], '_', StageName),
  create_nested_directory(data(StageName), StageDir),
  db_add_novel(user:file_search_path(Stage, StageDir)).
