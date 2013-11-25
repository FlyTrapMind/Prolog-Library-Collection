:- module(
  script_ext,
  [
    script/3, % +Options:list(nvpair)
              % +Process:atom
              % :Stages:list:compound
    script_clean/1, % +Process:atom
    script_stage_eval/2, % +Process:atom
                         % +Stage:nonneg
    script_stage_overview/1, % +Process:atom
    script_stage_tick/1, % +ProcessStage:pair(atom,nonneg)
% ACTIONS
    script_copy_file/3, % +ProcessStage
                        % +FromFile:atom
                        % +ToFile:atom
    script_extract_archive/3 % +ProcessStage
                             % +FromFile:atom
                             % +ToFile:atom
  ]
).

/** <module> Script extensions

Extensions for running automated scripts in stages.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(user_input)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).
:- use_module(os(process_ext)).

:- meta_predicate(script(+,+,:)).
:- meta_predicate(script_stage(+,+,+,+,3)).
:- meta_predicate(script_stage(+,+,+,+,+,+3)).
:- meta_predicate(script_stages(+,+,:)).

:- db_add_novel(user:prolog_file_type('tar.gz', archive)).
:- db_add_novel(user:prolog_file_type(txt,      text   )).

:- debug(script_ext).



% STAGE DIRECTORIES %

%! find_last_stage_directories(
%!   +Process:atom,
%!   -LastStageDirectories:list(atom)
%! ) is det.

find_last_stage_directories(Process, LSDirs):-
  find_stage_directories(Process, SDirs),
  (
    (SDirs = [] ; SDirs = [_])
  ->
    LSDirs = SDirs
  ;
    reverse(SDirs, [Dir1,Dir2|_]),
    LSDirs = [Dir1,Dir2]
  ).

%! find_stage_directories(+Process:atom, -StageDirectories:list(atom)) is det.

find_stage_directories(Process, StageDirs):-
  find_stage_directories(Process, StageDirs, 1).

%! find_stage_directories(
%!   +Process:atom,
%!   -StageDirectories:list(atom),
%!   +Stage:nonneg
%! ) is det.

find_stage_directories(Process, [H|T], Stage):-
  atomic_list_concat([stage,Stage], '_', StageName),
  process_absolute_file_name(
    Process,
    data(StageName),
    H,
    [access(write),file_errors(fail),file_type(directory)]
  ),
  NextStage is Stage + 1,
  find_stage_directories(T, NextStage).
find_stage_directories(_Process, [], _Stage):- !.

%! script_clean(+Process:atom) is det.
% This is run after results have been saved to the `Output` directory.

script_clean(Process):-
  find_stage_directories(Process, StageDirs),
  maplist(safe_delete_directory_contents([]), StageDirs).



% SCRIPT %

%! script(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +Stages:list(compound)
%! ) is det.
% The following options are supported:
%   * =|actual(+ActualNumberOfApplications:nonneg)|=
%     This is used to display progress bars.
%   * =|from(?FromDirectory:or([oneof([input]),positive_integer]),?FromFile:atom,?FromFileType:atom)|=
%     Identifies the input for a script stage.
%   * =|potential(+PotentialNumberOfApplications:nonneg)|=
%     This is used to display progress bars.
%   * =|to(?ToDirectory:or([oneof([output]),positive_integer]),?ToFile:atom,?ToFileType:atom)|=
%     Identifies the output from a script stage.
%
% @param Options A list of name-value pairs.
% @param Process The atomic name of the overall script.
% @param Stages A list of compound terms identifying script stages.
%
% @tbd If the `to/3` option is not set, then it is not clear whether a
%      stage has been processed or not.

script(O1, Process, Stages):-
  % Beginning the script.
  script_begin(O1, Process),

  % Option `to/3` is used to skip the entire script in case its output
  % is already available.
  (
    option(to(ToDir, ToFileName, ToFileType), O1),
    absolute_file_name(
      ToFileName,
      _ToFile,
      [
        access(read),
        file_errors(fail),
        file_type(ToFileType),
        relative_to(ToDir)
      ]
    )
  ->
    debug(script_ext, '~w script was skipped.', [Process])
  ;
    script_stages(Process, 0, Stages)
  ),

  % Ending the script.
  script_end(O1, Process).

%! script_begin(+Options:list(nvpair), +Process:atom) is det.

script_begin(_O1, Process):-
  % Timestamp the beginning of the script.
  date_time(Start),
  debug(script_ext, '~w script started at ~w.', [Process,Start]),

  % Make sure the data directory is there.
  (
    process_file_search_path(Process, data, _DataDir), !
  ;
    Spec =.. [Process,'Data'],
    create_project_subdirectory(Spec, DataDir),
    process_file_search_path(Process, data, DataDir)
  ),

  % Make sure the input directory is there.
  (
    process_file_search_path(Process, input, _InputDir), !
  ;
    process_create_nested_directory(Process, data('Input'), StageDir),
    process_file_search_path(Process, input, StageDir)
  ),

  % Make sure the output directory is there.
  (
    process_file_search_path(Process, output, _OutputDir), !
  ;
    process_create_nested_directory(Process, data('Output'), OutputDir),
    process_file_search_path(Process, output, OutputDir)
  ).

%! script_end(+Options:list(nvpair), +Process:atom) is det.
% End the script, saving the results to the `Output` directory.

script_end(_O1, Process):-
  % Retrieve the last two stage directories, if present.
  find_last_stage_directories(Process, LastDirs),
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
    process_absolute_file_name(
      Process,
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

  % Timestamp the end of the script.
  date_time(End),
  debug(script_ext, '~w script ended at ~w.', [Process,End]).



% SCRIPT STAGES %

%! script_stage(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +FromStage:nonneg,
%!   +ToStage:nonneg,
%!   :Goal
%! ) is det.
% `Goal` receives the from and to files as arguments.
%
% The following options are supported:
%   * =|actual(+ActualNumberOfApplications:nonneg)|=
%   * =|from(+FromDirectory:or([oneof([input]),positive_integer]),+FromFile:atom,+FromFileType:atom)|=
%   * =|potential(+PotentialNumberOfApplications:nonneg)|=
%   * =|to(+ToDirectory:or([oneof([output]),positive_integer]),+ToFile:atom,+ToFileType:atom)|=

script_stage(O1, Process, Stage1, Stage2, Goal):-
  script_stage_from_directory(O1, Process, Stage1, FromDir),
  script_stage_to_directory(O1, Process, Stage1, Stage2, ToDir),
  script_stage(O1, Process, Stage1, FromDir, ToDir, Goal).

% This stage was alreay completed previously. Skip this stage.
script_stage(_O1, Process, Stage, _FromDir, ToDir, _Goal):-
  absolute_file_name(
    'FINISHED',
    _ToFile,
    [access(read),file_errors(fail),relative_to(ToDir)]
  ), !,
  debug(script_ext, '~w stage ~w was skipped.', [Process,Stage]).
% This stage has not been perfomed yet.
script_stage(O1, Process, Stage, FromDir, ToDir, Goal):-
  % From directory or file.
  script_stage_from_arg(O1, Stage, FromDir, FromArg),

  % To directory or file.
  script_stage_to_arg(O1, ToDir, ToArg),

  % Beginning of a script stage.
  script_stage_begin(O1, Process, Stage),

  % Execute goal on the 'from' and 'to' arguments
  % (either files or directories).
  call(Goal, Process-Stage, FromArg, ToArg),

  % Ending of a script stage.
  script_stage_end(O1, Process, Stage, ToDir).

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

script_stage_end(_O1, Process, Stage, ToDir):-
  script_stage_eval(Process, Stage),
  absolute_file_name(
    'FINISHED',
    ToFile,
    [access(write),file_errors(fail),relative_to(ToDir)]
  ),
  atom_to_file('', ToFile),
  debug(script_ext, 'Ending ~w stage ~w.', [Process,Stage]).

%! script_stage_from_arg(
%!   +Options:list(nvpair),
%!   +Stage:nonneg,
%!   +FromDir:atom,
%!   -FromArg:atom
%! ) is det.

% Read the from file located in the previous stage directory.
script_stage_from_arg(O1, Stage, FromDir, FromArg):-
  option(from(_FromDir,FromFileName,FromFileType), O1),
  nonvar(FromFileName),
  nonvar(FromFileType), !,

  (
    absolute_file_name(
      FromFileName,
      FromArg,
      [
        access(read),
        file_errors(fail),
        file_type(FromFileType),
        relative_to(FromDir)
      ]
    ), !
  ;
    % For the input stage we allow the file to be absent.
    % The user is asked to provide a file location.
    Stage == 0,
    file_name_type(FromFileName, FromFileType, RelativeFile),
    user_input_directory(RelativeFile, AbsoluteFile),

    % Now that we have the location of the input file,
    % we copy it into project folder `Data/Input`.
    absolute_file_name(
      FromFileName,
      FromArg,
      [
        access(write),
        file_errors(fail),
        file_type(FromFileType),
        relative_to(FromDir)
      ]
    ),
    safe_copy_file(AbsoluteFile, FromArg)
  ).
% Read from the previous stage directory.
script_stage_from_arg(_O1, _Stage, FromDir, FromDir):-
  access_file(FromDir, read).

%! script_stage_from_directory(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +Stage:nonneg,
%!   -Directory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Specific directory specified as an option.
script_stage_from_directory(O1, _Process, _Stage, Dir):-
  option(from(FromDir1,_FromFileName,_FromFileType), O1),
  nonvar(FromDir1),
  FromDir2 =.. [FromDir1,'.'],
  absolute_file_name(
    FromDir2,
    Dir,
    [access(read),file_errors(fail),file_type(directory)]
  ), !.
% Before the first stage directory we start in `0` aka `Input`.
script_stage_from_directory(_O1, Process, 0, Dir):- !,
  process_create_nested_directory(Process, input('.'), Dir),
  process_absolute_file_name(
    Process,
    input('.'),
    Dir,
    [access(read),file_type(directory)]
  ).
% For stages `N >= 1` we use stuff from directories called `stage_N`.
script_stage_from_directory(_O1, Process, Stage, Dir):-
  atomic_list_concat([stage,Stage], '_', StageName),
  process_create_nested_directory(Process, data(StageName), Dir),
  process_file_search_path(Process, StageName, Dir).

script_stage_to_arg(O1, ToDir, ToArg):-
  option(to(_ToDir,ToFileName,ToFileType), O1),
  nonvar(ToFileName),
  nonvar(ToFileType), !,
  % Write to the to file located in the next stage directory.
  absolute_file_name(
    ToFileName,
    ToArg,
    [
      access(write),
      file_errors(fail),
      file_type(ToFileType),
      relative_to(ToDir)
    ]
  ).
% Write to the next stage directory.
script_stage_to_arg(_O1, ToDir, ToDir):-
  access_file(ToDir, write).

%! script_stage_to_directory(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +FromStage:nonneg,
%!   -ToStage:nonneg,
%!   -Directory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Specific directory specified as option.
script_stage_to_directory(O1, _Process, Stage, Stage, Dir):-
  option(to(FromDir1,_FromFileName,_FromFileType), O1),
  nonvar(FromDir1), !,
  FromDir2 =.. [FromDir1,'.'],
  absolute_file_name(
    FromDir2,
    Dir,
    [access(write),file_errors(fail),file_type(directory)]
  ).
% Stage directories.
script_stage_to_directory(_O1, Process, Stage1, Stage2, Dir):-
  Stage2 is Stage1 + 1,
  atomic_list_concat([stage,Stage2], '_', StageName),
  process_create_nested_directory(Process, data(StageName), Dir),
  process_file_search_path(Process, StageName, Dir).

%! script_stages(
%!   +Process:atom,
%!   +Stage:nonneg,
%!   +Stages:list(compound)
%! ) is det.

script_stages(_Process, _Stage, _Mod:[]):- !.
script_stages(Process, Stage1, Mod:[stage(O1,H)|T]):- !,
  script_stage(O1, Process, Stage1, Stage2, Mod:H),
  script_stages(Process, Stage2, Mod:T).



% DEFAULT ACTIONS %

script_copy_file(_PS, FromFile, ToFile):-
  safe_copy_file(FromFile, ToFile).

script_extract_archive(_PS, FromFile, ToFile):-
  directory_file_path(ToDir, _, ToFile),
  archive_extract(FromFile, ToDir, []).



% STATISTICS %

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

