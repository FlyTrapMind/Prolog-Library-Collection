:- module(
  ap,
  [
    ap/3 % +Options:list(nvpair)
         % +Alias:atom
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

@author Wouter Beek
@version 2013/06, 2013/10-2013/11, 2014/01-2014/02
*/

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(ap(ap_stat)).
:- use_module(generics(list_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_container)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

:- initialization(assert_ap_schema(ap)).



assert_ap_schema(Graph):-
  rdfs_assert_subclass(ap:'AP-Collection', rdf:'Bag', Graph),
  rdfs_assert_label(
    ap:'AP-Collection',
    'Collection of automated processes',
    Graph
  ),

  rdfs_assert_subclass(ap:'AP', rdf:'Seq', Graph),
  rdfs_assert_label(ap:'AP', 'Automated process', Graph),

  rdfs_assert_class(ap:'AP-Stage', Graph),
  rdfs_assert_label(ap:'AP-Stage', 'Automated process stage', Graph).


%! ap(+Options:list(nvpair), +Alias:atom, +AP_Stages:list(compound)) is det.
% `Alias` is an existing path alias with prolog_file_path/2.
%
% The following options are supported:
%   * =|reset(+Reset:boolean)|=
%     When true, removes the results of any prior executions of stages.
%     Default: `false`.

:- meta_predicate(ap(+,+,:)).
ap(O1, Alias, AP_Stages):-
  Graph = ap,

  rdf_global_id(ap:Alias, AP),
  rdf_assert_individual(AP, ap:'AP', Graph),

  atomic_list_concat(['AP_Collection',Alias], '/', LocalName),
  rdf_global_id(ap:LocalName, AP_Collection),
  rdf_assert_individual(AP_Collection, ap:'AP-Collection', Graph),
  rdf_assert_collection_member(AP_Collection, AP, Graph),

  ap_begin(O1, Alias),
  ap_run(Alias, AP, AP_Stages),
  ap_end(Alias).


%! ap_begin(+Options:list(nvpair), +Alias:atom) is det.

ap_begin(O1, Alias):-
  % Process the reset option.
  option(reset(Reset), O1, false),
  (
    Reset == true,
    file_search_path(Alias, Spec),
    absolute_file_name(
      Spec,
      Dir,
      [access(read),file_errors(fail),file_type(directory)]
    )
  ->
    delete_directory([include_self(false),safe(false)], Dir)
  ;
    true
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
  ).


%! ap_run(+Alias:atom, +AP:iri, :AP_Stages:list(compound)) is det.

:- meta_predicate(ap_run(+,+,:)).
% The output is already available.
ap_run(Alias, _, _):-
  ap_dir(Alias, read, output, OutputDir),
  absolute_file_name(
    'FINISHED',
    _ToFile,
    [access(read),file_errors(fail),relative_to(OutputDir)]
  ), !.
ap_run(Alias, AP, AP_Stages):-
  ap_stages(Alias, AP, AP_Stages).

