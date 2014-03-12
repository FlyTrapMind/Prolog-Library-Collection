:- module(
  ap_db,
  [
% ADD
    add_done/1, % +AP:iri
    add_operation_on_file/4, % +AP_Stage:iri
                             % +File:atom
                             % +Operation:atom
                             % +Modifiers:list
    add_properties_of_file/3, % +AP_Stage:iri
                              % +File:atom
                              % +NVPairs:list(pair)
    add_table/2, % +AP_Stage:iri
                 % +Table:iri
    add_skip/1, % +AP_Stage:iri
    add_succeed/1, % +AP_Stage:iri
% READ
    ap_resource/3, % +AP:iri
                   % ?Resource:iri
                   % ?Graph:atom
    ap_stage_name/2, % ?AP_Stage:iri
                     % ?Name:atom
    ap_stage_resource/3, % +AP_Stage:iri
                         % ?Resource:iri
                         % ?Graph:atom
% CREATE
    create_ap/2, % +AP_Collection:iri
                 % -AP:iri
    create_ap_collection/1, % -AP_Collection:iri
    create_initial_stage/2, % +AP:iri
                            % -Intitial_AP_Stage:iri
    create_next_stage/2, % +AP_Stage1:iri
                         % -AP_Stage2:iri
    create_resource/2 % +ClassName:atom
                      % -Resource:iri
  ]
).

/** <module> Automated Processes databases

@author Wouter Beek
@version 2014/02
*/

:- use_module(generics(error_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(ap_resource(r,r,?)).
:- rdf_meta(ap_stage_resource(r,r,?)).

:- xml_register_namespace(ap, 'http://www.wouterbeek.com/ap.owl#').

:- initialization(assert_schema).

assert_schema:-
  rdfs_assert_subclass(ap:'AP-Collection', rdf:'Bag', ap),
  rdfs_assert_label(
    ap:'AP-Collection',
    'Collection of automated processes',
    ap
  ),

  rdfs_assert_subclass(ap:'AP', rdf:'Seq', ap),
  rdfs_assert_label(ap:'AP', 'Automated process', ap),

  rdfs_assert_class(ap:'AP-Stage', ap),
  rdfs_assert_label(ap:'AP-Stage', 'Automated process stage', ap).


add_nvpair(Name-Value1, BNode):-
  rdf_bnode(BNode),
  rdf_assert_datatype(BNode, ap:name, xsd:string, Name, ap),
  with_output_to(atom(Value2), write_canonical_catch(Value1)),
  rdf_assert_datatype(BNode, ap:value, xsd:string, Value2, ap).


%! add_done(+AP:iri) is det.
% States that the given automated process was run in its entirety and
%  all its results (including failures and exceptions)
%  were stored succesfully.

add_done(AP):-
  rdf_assert_datatype(AP, ap:done, xsd:boolean, true, ap).


add_operation_on_file(AP_Stage, File, Operation, Modifiers):-
  rdf_assert_individual(AP_Stage, ap:'FileOperation', ap),
  rdf_assert_datatype(AP_Stage, ap:file, xsd:string, File, ap),
  rdf_assert_datatype(AP_Stage, ap:operation, xsd:string, Operation, ap),
  forall(
    member(Modifier, Modifiers),
    rdf_assert_datatype(AP_Stage, ap:has_modifier, xsd:string, Modifier, ap)
  ).


add_properties_of_file(AP_Stage, File, NVPairs):-
  rdf_assert_individual(AP_Stage, ap:'FileProperties', ap),
  rdf_assert_datatype(AP_Stage, ap:file, xsd:string, File, ap),
  maplist(add_nvpair, NVPairs, BNodes),
  forall(
    member(BNode, BNodes),
    rdf_assert(AP_Stage, ap:has_property, BNode, ap)
  ).


add_table(AP_Stage, Table):-
  rdf_assert_individual(AP_Stage, ap:'Tables', ap),
  rdf_assert(AP_Stage, ap:table, Table, ap).


%! add_skip(+AP_Stage:iri) is det.

add_skip(AP_Stage):-
  rdf_assert_individual(AP_Stage, ap:'Skip', ap),
  rdf_assert_datatype(AP_Stage, ap:status, xsd:string, skip, ap).


%! add_succeed(+AP_Stage:iri) is det.
% States that the given automated process stage was completed succesfully,
%  i.e. without failing or throwing an exception.

% Skipped AP stages are not asserted as succeeding.
add_succeed(AP_Stage):-
  rdfs_individual_of(AP_Stage, ap:'Skip'), !.
add_succeed(AP_Stage):-
  rdf_assert_datatype(AP_Stage, ap:status, xsd:string, succeed, ap).


ap_resource(AP, Resource, Graph):-
  rdf(AP, ap:resource, Resource, ap),
  rdf_datatype(AP, ap:graph, xsd:string, Graph, ap).


ap_stage_resource(AP_Stage, Resource, Graph):-
  nonvar(AP_Stage), !,
  rdf_collection_member(AP_Stage, AP, ap),
  ap_resource(AP, Resource, Graph).
ap_stage_resource(AP_Stage, Resource, Graph):-
  ap_resource(AP, Resource, Graph),
  rdf_collection_member(AP_Stage, AP, ap).


ap_stage_name(AP_Stage, Name):-
  rdf_datatype(AP_Stage, ap:name, xsd:string, Name, ap).


create_ap(AP_Collection, AP):-
  create_resource('AP', AP),
  rdf_assert_collection_member(AP_Collection, AP, ap).


create_ap_collection(AP_Collection):-
  create_resource('AP-Collection', AP_Collection).


create_initial_stage(AP, AP_Stage):-
  create_stage(AP, -1, AP_Stage).


create_stage(AP, StageNum, AP_Stage):-
  create_resource('AP-Stage', AP_Stage),
  rdf_assert_collection_member(AP, AP_Stage, ap),
  format(atom(Label), 'Automated stage ~d', [StageNum]),
  rdfs_assert_label(AP_Stage, Label, ap),
  rdf_assert_datatype(AP_Stage, ap:stage, xsd:integer, StageNum, ap).


create_next_stage(AP_Stage1, AP_Stage2):-
  rdf_datatype(AP_Stage1, ap:stage, xsd:integer, StageNum1, ap),
  once(rdf_collection_member(AP_Stage1, AP, ap)),
  StageNum2 is StageNum1 + 1,
  create_stage(AP, StageNum2, AP_Stage2).


create_resource(BaseName, Resource):-
  rdf_create_next_resource(ap, BaseName, Resource, ap).

