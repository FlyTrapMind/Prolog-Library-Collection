:- module(
  ap,
  [
    ap/4 % +Options:list(nvpair)
         % +Alias:atom
         % +Stages:list(compound)
         % -Row:list:atom
  ]
).

/** <module> Auto-process

Support for running automated processing.

@tbd Implement to/2 option:
  * =|to(?ToFile:atom,?ToFileType:atom)|=
    Identifies the output from a script stage.
    The directory is not included since this is fixed to
    the process' output directory.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11, 2014/01-2014/02
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(ap(ap_stat)).
:- use_module(generics(list_ext)).
:- use_module(os(dir_ext)).



%! ap(
%!   +Options:list(nvpair),
%!   +Alias:atom,
%!   +Stages:list(compound),
%!   -Row:list(atom)
%! ) is det.
% The following options are supported:
%   * =|graph(+Graph:atom)|=
%   * =|reset(+Reset:boolean)|=
%     When true, removes the results of any prior executions of stages.
%     Default: `false`.
%
% @arg Options
% @arg Alias An atomic alias, denoting the encompassing AP directory,
%      which must exist prior to calling this predicate.
% @arg Stages A list of compound terms identifying script stages.
% @arg Row A list of compound term that describe how each stage went.
%      These compound terms have the form =|ap(status(Status),Message)|=,
%      where `Status` is `oneof([fail,skip,succeed])` and
%      `Message` is generated using pl_term//1.

:- meta_predicate(ap(+,+,:,-)).
ap(O1, Alias, Stages, Row):-
  ap_begin(O1, Alias),
  ap_run(Alias, Stages, Row),
  ap_end(Alias).


%! ap_begin(+Options:list(nvpair), +Alias:atom) is det.

ap_begin(O1, Alias):-
  ap_debug(Alias, 'Started.', []),
  
  % Process the reset option.
  option(reset(Reset), O1, false),
  (
    Reset == false, !
  ;
    absolute_file_name(Alias, Dir, [access(read),file_type(directory)]),
    delete_directory([include_self(false),safe(false)], Dir)
  ),
  
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
    copy_directory([safe(true)], LastStageDir, OutputDir)
  ;
    true
  ),

  ap_debug(Alias, 'Ended successfully.', []).


%! ap_run(
%!   +Alias:atom,
%!   :Stages:list(compound),
%!   -Row:list(atom)
%! ) is det.

:- meta_predicate(ap_run(+,:,-)).
% The output is already available.
ap_run(Alias, _:Stages, Row):-
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

