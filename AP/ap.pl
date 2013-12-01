:- module(
  ap,
  [
    ap/2 % +Options:list(nvpair)
         % +Stages:list(compound)
  ]
).

/** <module> Auto-process

Support for running automated processing.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(ap(ap_stat)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).

:- meta_predicate(ap(+,:)).
:- meta_predicate(ap_run(+,:)).



%! ap(+Options:list(nvpair), +Stages:list(compound)) is det.
% The following options are supported:
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
%   * =|process(+Process:atom)|=
%     The atomic name of the process.
%     This is used for constructing the process directory structure.
%     Default: =process=.
%   * =|project(+Project:atom)|=
%     The atomic name of the root of the project's file search path.
%     Default: =project=.

ap_begin(O1):-
  ap_debug(O1, 'Started.', []),

  % Make sure the process directory is there.
  ap_dir(O1, '.', _ProcessDir),

  % Make sure the input directory is there.
  ap_dir(O1, input, _InputDir),

  % Make sure the output directory is there.
  ap_dir(O1, output, _OutputDir).

%! ap_end(+Options:list(nvpair)) is det.
% End the script, saving the results to the output directory.
%
% The following options are supported:
%   * =|process(+Process:atom)|=
%     The atomic name of the process.
%     This is used for constructing the process directory structure.
%     Default: =process=.
%   * =|project(+Project:atom)|=
%     The atomic name of the root of the project's file search path.
%     Default: =project=.

ap_end(O1):-
  % The last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    ap_last_stage_directory(O1, LastStageDir)
  ->
    ap_dir(O1, output, OutputDir),
    safe_copy_directory(LastStageDir, OutputDir)
  ;
    true
  ),

  ap_debug(O1, 'Ended successfully.', []).

% Output is already available.
ap_run(O1, _Stages):-
  ap_dir(O1, output, OutputDir),
  absolute_file_name(
    'FINISHED',
    _ToFile,
    [access(read),file_errors(fail),relative_to(OutputDir)]
  ), !,
  ap_debug(
    O1,
    'Process skipped. Results are already in output directory.',
    []
  ).
ap_run(O1, Stages):-
  ap_stages(O1, 0, Stages).

