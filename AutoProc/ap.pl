:- module(
  ap,
  [
    ap/2, % +Options:list(nvpair)
          % +Stages:list(compound)
  ]
).

/** <module> Auto-process

Support for running automated processing.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11
*/

:- use_module(library(option)).



%! ap(+Options:list(nvpair), +Stages:list(compound)) is det.
% The following options are supported:
%   * =|actual(+ActualNumberOfApplications:nonneg)|=
%     This is used to display progress bars.
%   * =|potential(+PotentialNumberOfApplications:nonneg)|=
%     This is used to display progress bars.
%   * =|process(+Process:atom)|=
%     The atomic name of the process.
%     This is used for constructing the process directory structure.
%     Default: =process=.
%   * =|project(+Project:atom)|=
%     The atomic name of the root of the project's file search path.
%     Default: =project=.
%   * =|to(?ToFile:atom,?ToFileType:atom)|=
%     Identifies the output from a script stage.
%     The directory is not included since this is fixed to
%     the process' output directory.
%
% @param Options A list of name-value pairs.
% @param Stages A list of compound terms identifying script stages.
%
% @tbd If the `to/3` option is not set, then it is not clear whether a
%      stage has been processed or not.

ap(O1, Stages):-
  ap_begin(O1),
  ap_run(O1, Stages),
  ap_end(O1).

%! ap_begin(+Options:list(nvpair)) is det.
% The following options are supported:
%   * =|ap(+Process:atom)|=
%     The atomic name of the process.
%     This is used for constructing the process directory structure.
%     Default: =process=.
%   * =|project(+Project:atom)|=
%     The atomic name of the root of the project's file search path.
%     Default: =project=.

ap_begin(O1):-
  ap_debug(O1, 'Started.'),

  option(project(Project), O1, project),
  option(process(Process), O1, process),
  atomic_list_concat([Project,Process], '_', ProcessAlias),
  
  % Make sure the process directory is there.
  (
    file_search_path(ProcessAlias, _ProcessDir), !
  ;
    ProcessSpec =.. [Project,Process],
    create_nested_directory(ProcessSpec, ProcessDir),
    db_add_novel(user:file_search_path(ProcessAlias, ProcessDir))
  ),

  % Make sure the input directory is there.
  atomic_list_concat([ProcessAlias,input], '_', InputAlias),
  (
    file_search_path(InputAlias, _InputDir), !
  ;
    InputSpec =.. [ProcessAlias,'Input'],
    create_nested_directory(InputSpec, InputDir),
    db_add_novel(user:file_search_path(InputAlias, InputDir))
  ),

  % Make sure the output directory is there.
  atomic_list_concat([ProcessAlias,output], '_', OutputAlias),
  (
    file_search_path(OutputAlias, _OutputDir), !
  ;
    OutputSpec =.. [ProcessAlias,'Output'],
    create_nested_directory(OutputSpec, OutputDir),
    db_add_novel(user:file_search_path(OutputAlias, OutputDir))
  ).

%! ap_end(+Options:list(nvpair)) is det.
% End the script, saving the results to the output directory.
%
% The following options are supported:
%   * =|ap(+Process:atom)|=
%     The atomic name of the process.
%     This is used for constructing the process directory structure.
%     Default: =process=.
%   * =|project(+Project:atom)|=
%     The atomic name of the root of the project's file search path.
%     Default: =project=.

ap_end(O1):-
  % Retrieve the last two stage directories, if present.
  ap_find_last_stage_directories(O1, LastDirs),
  (
    LastDirs = [], !
  ;
    LastDirs = [CopyDir], !
  ;
    LastDirs = [RemDir,CopyDir]
  ),

  % The next-to-last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    var(CopyDir), !
  ;
    ap_output_directory(O1, ToDir),
    safe_copy_directory(CopyDir, ToDir)
  ),

  % The last stage directory is superfluous; remove it.
  (
    var(RemDir), !
  ;
    safe_delete_directory(RemDir)
  ),

  ap_debug(O1, 'Ended successfully.').

% Option `to/2` is used to skip the entire script in case its output
% is already available.
ap_run(O1, Stages):-
  option(to(ToFileName,ToFileType), O1),
  ap_output_directory(O1, OutputDir),
  absolute_file_name(
    ToFileName,
    _ToFile,
    [
      access(read),
      file_errors(fail),
      file_type(ToFileType),
      relative_to(OutputDir)
    ]
  ), !,
  ap_debug(
    O1,
    'Process skipped. Results are already in output directory.',
  ).
ap_run(O1, Stages):-
  ap_stages(O1, 0, Stages).

