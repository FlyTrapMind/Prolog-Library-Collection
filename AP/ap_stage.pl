:- module(
  ap_stage,
  [
    ap_stages/3 % +Alias:atom
                % +Stages:list(compound)
                % -Row:list(atom)
  ]
).

/** <module> Auto-process stages

Runs stages in an automated process.

@author Wouter Beek
@version 2013/10-2014/01
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stat)).
:- use_module(generics(list_ext)).
:- use_module(generics(user_input)).
:- use_module(os(io_ext)).



%! ap_stage(
%!   +Options:list(nvpair),
%!   +Alias:atom,
%!   +FromStage:nonneg,
%!   +ToStage:nonneg,
%!   :Goal,
%!   -ToDirectory:atom,
%!   -Message:atom
%! ) is det.
% `Goal` receives the from and to files as arguments.
%
% The following options are supported:
%   * =|args(+Arguments:list)|=
%     Additional, goal-specific arguments.
%     Default: the empty list.
%   * =|finished(+Finished:boolean)|=
%     Whether or not a `FINISHED` file is create after stage completion.
%   * =|stat_lag(+Interval:positive_interval)|=
%     The lag between statistics updates in seconds.
%     Default: =10=.
%   * =|to(?ToFile:atom,?ToFileType:atom)|=
%     Identifies the output from a script stage.
%     The directory is not included since this is fixed to
%     the process' output directory.
%
% @arg Options
% @arg Alias Alias of the process+stage.
% @arg FromStage
% @arg ToStage
% @arg Goal
% @arg ToDirectory The atomic name of the directory where
%      the results are stored.
% @arg Message An atomic message that describes how this stage went.

:- meta_predicate(ap_stage(+,+,+,+,:,-,-)).
ap_stage(O1, Alias, Stage1, Stage2, Goal, ToDir, Msg):-
  ap_stage_from_directory(O1, Alias, Stage1, FromDir),
  ap_stage_to_directory(O1, Alias, Stage1, Stage2, ToDir),
  ap_stage2(O1, Alias, Stage1, FromDir, ToDir, Goal, Msg).


% This stage was alreay completed previously. Skip this stage.
ap_stage_init(_, Alias, ToDir, _, ap(status(skip),'FINISHED')):-
  absolute_file_name(
    'FINISHED',
    _,
    [access(read),file_errors(fail),relative_to(ToDir)]
  ), !,
  ap_debug(Alias, 'Skipped. Finished file found.', []).
% This stage has not been perfomed yet.
ap_stage_init(O1, _, ToDir, Goal, Msg):-
  % To directory or file.
  ap_stage_to_arg(O1, ToDir, ToArg),

  % Make sure the arguments option is present.
  option(args(Args), O1, []),

  (
    option(between(Low,High), O1)
  ->
    Potential is  High - Low + 1,
    ap_stage_init(Potential),
    forall(
      between(Low, High, N),
      execute_goal(Goal, [ToArg,Msg,N|Args])
    )
  ;
    execute_goal(Goal, [ToArg,Msg|Args])
  ), !.
ap_stage_init(_, _, _, _, ap(status(fail),unknown)).


:- meta_predicate(ap_stage2(+,+,+,+,+,:,-)).
% This stage was alreay completed previously. Skip this stage.
ap_stage2(_, Alias, _, _, ToDir, _, ap(status(skip),'FINISHED')):-
  absolute_file_name(
    'FINISHED',
    _,
    [access(read),file_errors(fail),relative_to(ToDir)]
  ), !,
  ap_debug(Alias, 'Skipped. Finished file found.', []).
% This stage has not been perfomed yet.
ap_stage2(O1, Alias, Stage, FromDir, ToDir, Goal, Msg):-
  % From directory or file.
  ap_stage_from_arg(O1, Stage, FromDir, FromArg),

  % To directory or file.
  ap_stage_to_arg(O1, ToDir, ToArg),

  % Beginning of a script stage.
  ap_stage_begin(Alias, Stage),

  % Allow this thread to refer to the stage alias later,
  % in order to keep track of the stage's progress.
  ap_store_stage_alias(Alias, Stage),

  % Make sure the arguments option is present.
  option(args(Args), O1, []),

  (
    option(between(Low,High), O1)
  ->
    Potential is  High - Low + 1,
    ap_stage_init(Potential),
    forall(
      between(Low, High, N),
      execute_goal(Goal, [FromArg,ToArg,Msg,N|Args])
    )
  ;
    execute_goal(Goal, [FromArg,ToArg,Msg|Args])
  ), !.
ap_stage2(_, _, _, _, _, _, ap(status(fail), unknown)).


ap_stage_begin(Alias, Stage):-
  ap_debug(Alias, '[Stage:~w] Started.', [Stage]).


ap_stage_end(O1, Alias, Stage, ToDir):-
  % Send a progress bar to the debug chanel.
  ap_stage_done(Alias, Stage),

  % Add an empty file that indicates this stage completed successfully
  (
    option(finished(true), O1, false)
  ->
    absolute_file_name(
      'FINISHED',
      ToFile,
      [access(write),file_errors(fail),relative_to(ToDir)]
    ),
    atom_to_file('', ToFile)
  ;
    true
  ),

  ap_debug(Alias, '[Stage:~w] Ended successfully.', [Stage]).


%! ap_stage_from_arg(
%!   +Options:list(nvpair),
%!   +Stage:nonneg,
%!   +FromDir:atom,
%!   -FromArg:atom
%! ) is det.

% Read the from file located in the previous stage directory.
ap_stage_from_arg(O1, _, FromDir, FromArg):-
  option(from(_,FromFileName,FromFileType), O1),
  nonvar(FromFileName),
  nonvar(FromFileType),
  absolute_file_name(
    FromFileName,
    FromArg,
    [
      access(read),
      file_errors(fail),
      file_type(FromFileType),
      relative_to(FromDir)
    ]
  ).
% For the input stage we allow the file to be absent.
% The user may be asked to provide a file location.
ap_stage_from_arg(O1, 0, FromDir, FromDir):-
  option(from(_FromDir,FromFileName,FromFileType), O1),
  nonvar(FromFileName),
  nonvar(FromFileType), !,
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
  safe_copy_file(AbsoluteFile, FromArg).
% Read from the previous stage directory.
ap_stage_from_arg(_, _, FromDir, FromDir):-
  access_file(FromDir, read).


%! ap_stage_from_directory(
%!   +Options:list(nvpair),
%!   +Alias:atom,
%!   +Stage:nonneg,
%!   -FromDirectory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Use the directory that is specified as an option, if it exists.
ap_stage_from_directory(O1, Alias, _, FromDir):-
  option(from(FromDirName,_,_), O1),
  nonvar(FromDirName), !,
  ap_dir(Alias, write, FromDirName, FromDir).
% Before the first stage directory we start in `0` aka `Input`.
ap_stage_from_directory(_, Alias, 0, InputDir):- !,
  ap_dir(Alias, write, input, InputDir).
% For stages `N >= 1` we use stuff from directories called `stage_N`.
ap_stage_from_directory(_, Alias, Stage, StageDir):-
  ap_stage_name(Stage, StageName),
  ap_dir(Alias, write, StageName, StageDir).


%! ap_stage_to_arg(
%!   +Options:list(nvpair),
%!   +ToDirectory:atom,
%!   -ToArgument
%! ) is det.

ap_stage_to_arg(O1, ToDir, ToArg):-
  option(to(_,ToFileName,ToFileType), O1),
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
ap_stage_to_arg(_, ToDir, ToDir):-
  access_file(ToDir, write).


%! ap_stage_to_directory(
%!   +Options:list(nvpair),
%!   +Alias:atom,
%!   +FromStage:nonneg,
%!   -ToStage:nonneg,
%!   -Directory:atom
%! ) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% Specific directory specified as option.
ap_stage_to_directory(O1, _, Stage, Stage, Dir):-
  option(to(FromDir1,_,_), O1),
  nonvar(FromDir1), !,
  FromDir2 =.. [FromDir1,'.'],
  absolute_file_name(
    FromDir2,
    Dir,
    [access(write),file_errors(fail),file_type(directory)]
  ).
% Stage directories.
ap_stage_to_directory(_, Alias, Stage1, Stage2, StageDir):-
  Stage2 is Stage1 + 1,
  ap_stage_name(Stage2, StageName),
  ap_dir(Alias, write, StageName, StageDir).


%! ap_stages(+Alias:atom, +Stage:nonneg, +Stages:list(compound)) is det.

:- meta_predicate(ap_stages(+,:,-)).
ap_stages(_, [], []).
ap_stages(Alias, Mod:[ap_stage(O1,Goal)|T], Msgs2):-
  ap_dir(Alias, write, input, ToDir),
  catch(
    (
      ap_stage_init(O1, Alias, ToDir, Mod:Goal, Msg),
      ap_stage_end(O1, Alias, 0, ToDir),
      ap_stages(Alias, 0, Mod:T, Msgs1),
      Msgs2 = [Msg|Msgs1]
    ),
    E,
    ap_catcher(E, T, Msgs2)
  ).

:- meta_predicate(ap_stages(+,+,:,-)).
ap_stages(_, _, _Mod:[], []):- !.
ap_stages(Alias, Stage1, Mod:[ap_stage(O1,H)|T], Msgs2):-
  catch(
    (
      ap_stage(O1, Alias, Stage1, Stage2, Mod:H, ToDir, Msg),
      ap_stage_end(O1, Alias, Stage2, ToDir),
      ap_stages(Alias, Stage2, Mod:T, Msgs1),
      Msgs2 = [Msg|Msgs1]
    ),
    E,
    ap_catcher(E, T, Msgs2)
  ).

ap_catcher(E, L, [E|Msgs]):-
  length(L, Length),
  repeating_list(ap(status(skip),'never reached'), Length, Msgs).

execute_goal(Goal, Args):-
  get_time(Begin),
  apply(Goal, Args),
  get_time(End),
  Delta is End - Begin,
  debug(ap, 'Duration:~w ; Goal:~w ; Args:~w', [Delta,Goal,Args]).

