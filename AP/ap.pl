:- module(
  ap,
  [
    ap/3 % +Alias:atom
         % +Stages:list(compound)
         % -Row:list:atom
  ]
).

/** <module> Auto-process

Support for running automated processing.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11, 2014/01
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(ap(ap_stat)).
:- use_module(generics(list_ext)).
:- use_module(os(dir_ext)).

:- meta_predicate(ap(+,:,-)).
:- meta_predicate(ap_run(+,:,-)).



%! ap(+Alias:atom, +Stages:list(compound), -Row:list(atom)) is det.
% The following options are supported:
%   * =|to(?ToFile:atom,?ToFileType:atom)|=
%     Identifies the output from a script stage.
%     The directory is not included since this is fixed to
%     the process' output directory.
%
% @arg Alias An atomic alias, denoting the encompassing AP directory,
%      which must exist prior to calling this predicate.
% @arg Stages A list of compound terms identifying script stages.
% @arg Row A list of atoms that describe how each stage went.

ap(Alias, Stages, Row):-
  ap_begin(Alias),
  ap_run(Alias, Stages, Row),
  ap_end(Alias).


%! ap_begin(+Alias:atom) is det.

ap_begin(Alias):-
  ap_debug(Alias, 'Started.', []),

  % Make sure the input directory is there.
  Spec1 =.. [Alias,input],
  create_nested_directory(Spec1),

  % Make sure the output directory is there.
  Spec2 =.. [Alias,output],
  create_nested_directory(Spec2).


%! ap_end(+Alias:atom) is det.
% End the script, saving the results to the output directory.

ap_end(Alias):-
  % The last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    ap_last_stage_directory(Alias, LastStageDir)
  ->
    ap_dir(Alias, write, output, OutputDir),
    safe_copy_directory(LastStageDir, OutputDir)
  ;
    true
  ),

  ap_debug(Alias, 'Ended successfully.', []).


%! ap_run(+Alias:atom, +Stages:list(compound), -Row:list(atom)) is det.

% The output is already available.
ap_run(Alias, Stages, Row):-
  ap_dir(Alias, read, output, OutputDir),
  absolute_file_name(
    'FINISHED',
    _ToFile,
    [access(read),file_errors(fail),relative_to(OutputDir)]
  ), !,
  length(Stages, Length),
  repeating_list(skip, Length, Row),
  ap_debug(
    Alias,
    'Process skipped. Results are already in output directory.',
    []
  ).
ap_run(Alias, Stages, Row):-
  ap_stages(Alias, Stages, Row).

