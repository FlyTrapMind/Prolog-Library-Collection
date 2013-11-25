:- module(
  ap_stage,
  [
    ap_stages/2, % +Options:list(nvpair)
                 % +Stages:list(compound)
  ]
).

/** <module> Auto-process stages

Runs stages in an automated process.

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stat)).
:- use_module(generics(user_input)).



%! ap_stage(
%!   +Options:list(nvpair),
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

ap_stage(O1, Stage1, Stage2, Goal):-
  ap_stage_from_directory(O1, Stage1, FromDir),
  ap_stage_to_directory(O1, Stage1, Stage2, ToDir),
  ap_stage(O1, Stage1, FromDir, ToDir, Goal).

% This stage was alreay completed previously. Skip this stage.
ap_stage(O1, Stage, _FromDir, ToDir, _Goal):-
  absolute_file_name(
    'FINISHED',
    _ToFile,
    [access(read),file_errors(fail),relative_to(ToDir)]
  ), !,
  ap_debug(O1, 'Skipped. Finished file found.').
% This stage has not been perfomed yet.
ap_stage(O1, Process, Stage, FromDir, ToDir, Goal):-
  % From directory or file.
  ap_stage_from_arg(O1, Stage, FromDir, FromArg),

  % To directory or file.
  ap_stage_to_arg(O1, ToDir, ToArg),

  % Beginning of a script stage.
  ap_stage_begin(O1, Stage),

  % Execute goal on the 'from' and 'to' arguments
  % (either files or directories).
  call(Goal, Process-Stage, FromArg, ToArg),

  % Ending of a script stage.
  ap_stage_end(O1, Stage, ToDir).

ap_stage_begin(O1, Stage):-
  % Initialize the number of actual and potential applications.
  ap_stage_init(O1, Stage),
  
  ap_debug(O1, 'Start stage ~w.', [Stage]).

ap_stage_end(O1, Stage, ToDir):-
  % Send a progress bar to the debug chanel.
  ap_stage_done(O1, Stage),
  
  % Add an empty file that indicates this stage completed successfully
  absolute_file_name(
    'FINISHED',
    ToFile,
    [access(write),file_errors(fail),relative_to(ToDir)]
  ),
  atom_to_file('', ToFile),
  
  ap_debug(O1, '[Stage:~w] Ended successfully.', [Stage]).

%! ap_stage_from_arg(
%!   +Options:list(nvpair),
%!   +Stage:nonneg,
%!   +FromDir:atom,
%!   -FromArg:atom
%! ) is det.

% Read the from file located in the previous stage directory.
ap_stage_from_arg(O1, Stage, FromDir, FromArg):-
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
ap_stage_from_arg(_O1, _Stage, FromDir, FromDir):-
  access_file(FromDir, read).

%! ap_stage_from_directory(
%!   +Options:list(nvpair),
%!   +Stage:nonneg,
%!   -FromDirectory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Use the directory that is specified as an option, if it exists.
ap_stage_from_directory(O1, _Stage, FromDir):-
  option(from(FromDirName,_FromFileName,_FromFileType), O1),
  nonvar(FromDirName),
  ap_directory(O1, FromDirName, FromDirAlias),
  FromDirSpec =.. [FromDirAlias,'.'],
  absolute_file_name(
    FromDirSpec,
    FromDir,
    [access(read),file_errors(fail),file_type(directory)]
  ), !.
% Before the first stage directory we start in `0` aka `Input`.
ap_stage_from_directory(O1, 0, InputDir):- !,
  ap_input_directory(O1, InputDir).
% For stages `N >= 1` we use stuff from directories called `stage_N`.
ap_stage_from_directory(O1, StageNumber, StageDir):-
  ap_create_stage_directory(O1, StageNumber, StageDir).

%! ap_stage_to_arg(
%!   +Options:list(nvpair),
%!   +ToDirectory:atom,
%!   -ToArgument
%! ) is det.

ap_stage_to_arg(O1, ToDir, ToArg):-
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
ap_stage_to_arg(_O1, ToDir, ToDir):-
  access_file(ToDir, write).

%! ap_stage_to_directory(
%!   +Options:list(nvpair),
%!   +Process:atom,
%!   +FromStage:nonneg,
%!   -ToStage:nonneg,
%!   -Directory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Specific directory specified as option.
ap_stage_to_directory(O1, Stage, Stage, Dir):-
  option(to(FromDir1,_FromFileName,_FromFileType), O1),
  nonvar(FromDir1), !,
  FromDir2 =.. [FromDir1,'.'],
  absolute_file_name(
    FromDir2,
    Dir,
    [access(write),file_errors(fail),file_type(directory)]
  ).
% Stage directories.
ap_stage_to_directory(O1, Stage1, Stage2, StageDir):-
  Stage2 is Stage1 + 1,
  ap_create_stage_directory(O1, Stage2, StageDir).

%! ap_stages(
%!   +Option:list(nvpair),
%!   +Stage:nonneg,
%!   +Stages:list(compound)
%! ) is det.

ap_stages(_O1, _Stage, _Mod:[]):- !.
ap_stages(O1, Stage1, Mod:[ap_stage(O1,H)|T]):- !,
  ap_stage(O1, Stage1, Stage2, Mod:H),
  ap_stages(O1, Stage2, Mod:T).

