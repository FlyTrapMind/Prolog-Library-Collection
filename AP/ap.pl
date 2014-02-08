:- module(
  ap,
  [
    ap/3, % +Options:list(nvpair)
          % +AP:iri
          % +AP_Stages:list(compound)
    ap_resource/2 % ?AP_Collection:iri
                  % -AP:iri
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

:- use_module(ap(ap_dir)).
:- use_module(ap(ap_stage)).
:- use_module(ap(ap_stat)).
:- use_module(generics(list_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
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


ap_resource(AP_Collection1, AP):-
  var(AP_Collection1), !,
  Graph = ap,
  flag(ap_collection, Id2, Id2 + 1),
  atomic_list_concat(['AP_Collection',Id2], '/', LocalName2),
  rdf_global_id(ap:LocalName2, AP_Collection2),
  rdf_assert_individual(AP_Collection2, ap:'AP-Collection', Graph),
  ap_resource(AP_Collection2, AP).
ap_resource(AP_Collection, AP):-
  Graph = ap,
  flag(ap, Id1, Id1 + 1),
  atomic_list_concat(['AP',Id1], LocalName1),
  rdf_global_id(ap:LocalName1, AP),
  rdf_assert_individual(AP, ap:'AP', Graph),
  rdf_assert_collection_member(AP_Collection, AP, Graph).



%! ap(+Options:list(nvpair), +AP:iri, +AP_Stages:list(compound)) is det.
% `Alias` is an existing path alias with prolog_file_path/2.
%
% The following options are supported:
%   * =|reset(+Reset:boolean)|=
%     When true, removes the results of any prior executions of stages.
%     Default: `false`.

:- meta_predicate(ap(+,+,:)).
ap(O1, AP, AP_Stages):-
  ap_begin(O1, AP),
  ap_stages(AP, AP_Stages),
  ap_end(AP).


%! ap_begin(+Options:list(nvpair), +AP:iri) is det.

ap_begin(O1, AP):-
  rdf_datatype(AP, ap:alias, xsd:string, Alias, ap),

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


%! ap_end(+AP:iri) is det.
% End the script, saving the results to the output directory.

ap_end(AP):-
  % The last stage directory contains the output of the script.
  % Copy these contents to the output directory.
  (
    ap_last_stage_dir(AP, LastStageDir)
  ->
    ap_dir(AP, write, output, OutputDir),
    copy_directory([safe(true)], LastStageDir, OutputDir)
  ;
    true
  ).

