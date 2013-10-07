:- module(
  script_ext,
  [
    script/3, % +Options:list(nvpair)
              % +Process:atom
              % :Stages:list:compound
    script_stage_eval/2, % +Process:atom
                         % +Stage:nonneg
    script_stage_overview/1, % +Process:atom
    script_stage_tick/1 % +ProcessStage:pair(atom,nonneg)
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
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- meta_predicate(script(+,+,:)).
:- meta_predicate(script_stage(+,+,+,3)).

:- debug(script_ext).



%! find_last_stages(-LastStageDirectories:list(atom)) is det.

find_last_stages(LSDirs):-
  find_stage_directories(SDirs),
  (
    (SDirs = [] ; SDirs = [_])
  ->
    LSDirs = SDirs
  ;
    reverse(SDirs, [Dir1,Dir2|_]),
    LSDirs = [Dir1,Dir2]
  ).

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

script(O1, Process, Mod:Stages):-
  script_begin(O1, Process),
  (
    option(to_file(ToFileName), O1),
    absolute_file_name2(output(ToFileName), _ToFile, [access(read)])
  ->
    debug(script_ext, '~w script was skipped.', [Process])
  ;
    forall(
      nth0(I, Stages, stage(O2,Goal)),
      script_stage(O2, Process, I, Mod:Goal)
    ),
    script_end(O1, Process)
  ).

script_begin(_O1, Process):-
  date_time(Start),
  debug(script_ext, '~w script started at ~w.', [Process,Start]),
  init_data_directory,
  create_nested_directory(data('Output'), OutputDir),
  db_add_novel(user:file_search_path(output, OutputDir)).

%! script_clean is det.
% This is run after results have been saved to the `Output` directory.

script_clean:-
  find_stage_directories(StageDirs),
  maplist(safe_delete_directory_contents, StageDirs).

%! script_end(+Process:atom) is det.
% End the script, saving the results to the `Output` directory.

script_end(_O1, Process):-
  % Retrieve the last two stage directories, if present.
  find_last_stages(LastDirs),
  (
    LastDirs = [], !
  ;
    LastDirs = [CopyDir], !
  ;
    LastDirs = [RemDir,CopyDir]
  ),

  % The next-to-last stage directory contains the output of the script.
  (
    var(CopyDir), !
  ;
    absolute_file_name(
      output('.'),
      ToDir,
      [access(write),file_type(directory)]
    ),
    safe_copy_directory(CopyDir, ToDir)
  ),

  % The last stage directory is superfluous.
  (
    var(RemDir), !
  ;
    safe_delete_directory(RemDir)
  ),

  date_time(End),
  debug(script_ext, '~w script ended at ~w.', [Process,End]).

%! script_stage(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +Stage:nonneg,
%!   :Goal
%! ) is det.
% `Goal` receives the from and to files as arguments.
%
% The following options are supported:
%   * =|actual(ActualNumberOfApplications:nonneg)|=
%   * =|from_file(FromFile:atom)|+
%   * =|potential(PotentialNumberOfApplications:nonneg)|=
%   * =|to_file(ToFile:atom)|+

% This stage was already performed in the past, since the to file is
% already in the next stage directory.
script_stage(O1, Process, Stage, _Goal):-
  option(to_file(ToFileName), O1),
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  absolute_file_name2(
    ToFileName,
    _ToFile,
    [access(read),relative_to(ToDir)]
  ), !,
  debug(script_ext, '~w stage ~w was skipped.', [Process,Stage]).
% This stage has not been perfomed yet.
script_stage(O1, Process, Stage, Goal):-
  % From the previous stage.
  stage_directory(Stage, FromDir),
  (
    option(from_file(FromFileName), O1)
  ->
    % Read the from file located in the previous stage directory.
    absolute_file_name2(
      FromFileName,
      FromArg,
      [access(read),relative_to(FromDir)]
    )
  ;
    % Read from the previous stage directory.
    access_file(FromDir, read),
    FromArg = FromDir
  ),

  % To the next stage.
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  (
    option(to_file(ToFileName), O1)
  ->
    % Write to the to file located in the next stage directory.
    absolute_file_name2(
      ToFileName,
      ToArg,
      [access(write),relative_to(ToDir)]
    )
  ;
    % Write to the next stage directory.
    access_file(ToDir, write),
    ToArg = ToDir
  ),

  script_stage_begin(O1, Process, Stage),
  % Execute goal on the from and to arguments (either files or directories).
  call(Goal, Process-Stage, FromArg, ToArg),
  script_stage_end(O1, Process, Stage).

script_stage_begin(O1, Process, Stage):-
  % The number of actual applications.
  option(actual(A), O1, 0),
  format(atom(FlagA), '~w_~w_a', [Process,Stage]),
  flag(FlagA, _, A),

  % The number of potential applications.
  option(potential(P), O1, 0),
  format(atom(FlagP), '~w_~w_p', [Process,Stage]),
  flag(FlagP, _, P),
  
  debug(script_ext, 'Starting ~w stage ~w.', [Process,Stage]).

script_stage_end(_O1, Process, Stage):-
  script_stage_eval(Process, Stage),
  debug(script_ext, 'Ending ~w stage ~w.', [Process,Stage]).

script_stage_eval(Process, Stage):-
  script_stage_progress(Process, Stage, A, P),
  progress_bar(A, P, Bar),
  debug(script_ext, '[~w:~w] ~w', [Process,Stage,Bar]).

%! script_stage_overview(+Process:atom) is det.
%! script_stage_overview(+Id:pair(atom,nonneg)) is det.

script_stage_overview(Process):-
  atom(Process), !,
  script_stage_overview(Process, 0).

script_stage_overview(Process, Stage):-
  script_stage_eval(Process, Stage), !,
  NextStage is Stage + 1,
  script_stage_overview(Process, NextStage).
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
  flag(FlagP, P, P).

script_stage_tick(Process-Stage):-
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

