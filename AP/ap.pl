:- module(
  ap,
  [
    ap/3 % +Options:list(nvpair)
         % ?AP:iri
         % +AP_Stages:list(compound)
  ]
).

/** <module> Auto-process

Support for running automated processing.

@tbd Implement to/2 option:
  * =|to(?ToFile:atom,?ToFileType:atom)|=
    Identifies the output from a script stage.
    The directory is not included since this is fixed to
    the process' output directory.
@tbd Add support for option =|finished(+Finished:boolean)|=,
     allowing previously finished processes to be skipped.

@author Wouter Beek
@version 2013/06, 2013/10-2013/11, 2014/01-2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(library(apply)).
:- use_module(library(process)).
:- use_module(os(dir_ext)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_serial)).
:- use_module(xsd(xsd_duration)).



%! ap(+Options:list(nvpair), +AP:iri, +AP_Stages:list(compound)) is det.
% `Alias` is an existing path alias with prolog_file_path/2.
%
% The following options are supported:
%   * =|leave_trail(LeaveTrail:boolean)|=
%     Default `true`.
%   * =|reset(+Reset:boolean)|=
%     When true, removes the results of any prior executions of stages.
%     Default: `false`.

ap(O1, AP, AP_Stages):-
  var(AP), !,
  create_ap(_, AP),
  ap(O1, AP, AP_Stages).
ap(O1, AP, AP_Stages):-
  ap_begin(O1, AP),
  ap_stages(AP, AP_Stages),
  ap_end(O1, AP).


%! ap_begin(+Options:list(nvpair), +AP:iri) is det.

ap_begin(O1, AP):-
  rdf_datatype(AP, ap:alias, xsd:string, Alias, ap),

  % Process the reset option.
  % If `true` then any previous results are removed.
  option(reset(Reset), O1, false),
  (
    Reset == true,
    ap_directory(AP, Dir)
  ->
    process_create(path(rm), ['-r',file(Dir)], [])
    %delete_directory([include_self(true),safe(false)], Dir)
  ;
    true
  ),

  % Make sure the input directory is there.
  Spec1 =.. [Alias,input],
  create_nested_directory(Spec1),

  % Make sure the output directory is there.
  Spec2 =.. [Alias,output],
  create_nested_directory(Spec2).


%! ap_end(+Options:list(nvpair), +AP:iri) is det.
% End the script, saving the results to the output directory.

ap_end(O1, AP):-
  % The last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    ap_last_stage_directory(AP, LastStageDir)
  ->
    ap_directory(AP, write, output, OutputDir),
    link_directory_contents(LastStageDir, OutputDir)
  ;
    true
  ),

  (
    option(leave_trail(true), O1, true)
  ->
    true
  ;
    ap_clean(AP)
  ),

  rdf_collection(AP, AP_Stages, ap),
  maplist(ap_stage_duration, AP_Stages, Monthss, Secondss),
  maplist(sum_list, [Monthss,Secondss], [Months,Seconds]),
  rdf_assert_datatype(
    AP,
    ap:duration,
    xsd:duration,
    duration(Months,Seconds),
    ap
  ),
  
  % Save the ap assertions until now.
  absolute_file_name(data(ap), File, [access(write),extensions([tmp])]),
  rdf_save([format(turtle)], ap, File).

ap_stage_duration(AP_Stage, Months, Seconds):-
  rdf_datatype(AP_Stage, ap:duration, xsd:duration, Duration, ap), !,
  Duration = duration(Months,Seconds).
ap_stage_duration(_, 0, 0.0).

