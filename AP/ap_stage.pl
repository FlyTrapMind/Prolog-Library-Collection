:- module(
  ap_stage,
  [
    ap_stages/3 % +Alias:atom
                % +AP:iri
                % :AP_Stages:list(compound)
  ]
).

/** <module> Auto-process stages

Runs stages in an automated process.

# Options for AP stages

The following options can be added to AP stages:
  * =|args(+PostfixedArguments:list)|=
  * =|between(+Low:integer, +High:integer)|=
  * =|finished(+Finished:boolean)|=
  * =|from(+Directory:atom,+Base:atom,+FileType:atom)|=
  * =|name(+Name:atom)|=
    The name of the stage.
    This is e.g. used as the column label in tabular overviews of APs.
  * =|to(+Base:atom,+FileType:atom)|=

@author Wouter Beek
@version 2013/10-2014/02
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stat)).
:- use_module(ap(ap_table)).
:- use_module(generics(list_ext)).
:- use_module(generics(user_input)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(io_ext)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').



%! ap_stage(
%!   +Options:list(nvpair),
%!   +Alias:atom,
%!   +AP:iri,
%!   +FromStage:nonneg,
%!   +ToStage:nonneg,
%!   :Goal,
%!   -ToDirectory:atom
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
% `ToDirectory` is the atomic name of the directory where
%  the results are stored.

:- meta_predicate(ap_stage(+,+,+,+,+,:,-)).
ap_stage(O1, Alias, AP, Stage1, Stage2, Goal, ToDir):-
  ap_stage_from_directory(O1, Alias, Stage1, FromDir),
  ap_stage_to_directory(O1, Alias, Stage1, Stage2, ToDir),
  ap_stage2(O1, Alias, AP, Stage1, FromDir, ToDir, Goal).


:- meta_predicate(ap_stage2(+,+,+,+,+,+,:)).
% This stage was alreay completed previously. Skip this stage.
ap_stage2(_, _, _, _, _, ToDir, _):-
  absolute_file_name(
    'FINISHED',
    _,
    [access(read),file_errors(fail),relative_to(ToDir)]
  ), !,
  debug(ap, 'Skipped. Finished file found.', []).
% This stage has not been perfomed yet.
ap_stage2(O1, Alias, AP, StageNum, FromDir, ToDir, Goal):-
  % From directory or file.
  ap_stage_from_arg(O1, StageNum, FromDir, FromArg),

  % To directory or file.
  ap_stage_to_arg(O1, ToDir, ToArg),

  % Beginning of a script stage.
  ap_stage_begin(Alias, AP, StageNum, AP_Stage),

  % Make sure the arguments option is present.
  option(args(Args), O1, []),

  (
    option(between(Low,High), O1)
  ->
    Potential is  High - Low + 1,
    ap_stage_init(Potential),
    forall(
      between(Low, High, N),
      execute_goal(Goal, [FromArg,ToArg,AP_Stage,N|Args])
    )
  ;
    execute_goal(Goal, [FromArg,ToArg,AP_Stage|Args])
  ), !.
ap_stage2(_, _, _, _, _, _, _).


ap_stage_begin(Alias, AP, Stage, AP_Stage):-
  Graph = ap,
  atomic_list_concat(['AP-Stage',Alias], '/', LocalName),
  rdf_global_id(ap:LocalName, AP_Stage),
  rdf_assert_individual(AP_Stage, ap:'AP-Stage', Graph),
  rdf_assert_collection_member(AP, AP_Stage, Graph),
  rdfs_assert_label(AP_Stage, Alias, Graph),
  rdf_assert_datatype(AP_Stage, ap:stage, xsd:integer, Stage, Graph).


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

  debug(ap, '[Stage:~w] Ended successfully.', [Stage]).


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


%! ap_stages(+Alias:atom, +AP:iri, :AP_Stages:list(compound)) is det.

:- meta_predicate(ap_stages(+,+,:)).
ap_stages(_, [], []).
ap_stages(Alias, AP, Mod:[ap_stage(O1,Goal)|T]):-
  ap_dir(Alias, write, input, ToDir),
  catch(
    (
      ap_stage2(O1, Alias, AP, input, input, ToDir, Mod:Goal),
      ap_stage_end(O1, Alias, 0, ToDir),
      ap_stages(Alias, AP, 0, Mod:T)
    ),
    Error,
    debug(ap, '~w', [Error])
  ).

%! ap_stages(
%!   +Alias:atom,
%!   +AP:iri,
%!   +AP_Stage:nonneg,
%!   :AP_Stages:list(compound)
%! ) is det.

:- meta_predicate(ap_stages(+,+,+,:)).
ap_stages(_, _, _, _Mod:[]):- !.
ap_stages(Alias, AP, StageNum1, Mod:[ap_stage(O1,H)|T]):-
  catch(
    (
      ap_stage(O1, Alias, AP, StageNum1, StageNum2, Mod:H, ToDir),
      ap_stage_end(O1, Alias, StageNum2, ToDir),
      ap_stages(Alias, StageNum2, Mod:T)
    ),
    Error,
    debug(ap, '~w', [Error])
  ).

:- meta_predicate(execute_goal(:,+)).
execute_goal(Goal, Args):-
  setup_call_cleanup(
    get_time(Begin),
    apply(Goal, Args),
    (
      get_time(End),
      Delta is End - Begin,
      debug(ap, 'Duration:~w ; Goal:~w ; Args:~w', [Delta,Goal,Args])
    )
  ).

