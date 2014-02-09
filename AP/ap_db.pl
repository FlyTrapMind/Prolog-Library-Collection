:- module(
  ap_db,
  [
    add_operation_on_file/4, % +AP_Stage:iri
                             % +File:atom
                             % +Operation:atom
                             % +NVPairs:list(pair)
    add_properties_of_file/3, % +AP_Stage:iri
                              % +File:atom
                              % +NVPairs:list(pair)
    add_succeed/1, % +AP_Stage:iri
    ap_resource/3, % +AP_Stage:iri
                   % ?Resource:iri
                   % ?Graph:atom
    create_ap/2, % ?AP_Collection:iri
                 % -AP:iri
    create_initial_stage/2, % +AP:iri
                            % -Intitial_AP_Stage:iri
    create_next_stage/2, % +AP_Stage1:iri
                         % -AP_Stage2:iri
    create_resource/2 % +ClassName:atom
                      % -Resource:iri
  ]
).

/** <module> AP DB

@author Wouter Beek
@version 2014/02
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_list)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(xml(xml_namespace)).

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

add_nvpair(Name-Value, BNode):-
  rdf_bnode(BNode),
  rdf_assert_datatype(BNode, ap:name, xsd:string, Name, ap),
  rdf_assert_datatype(BNode, ap:value, xsd:string, Value, ap).

add_operation_on_file(AP_Stage, File, Operation, NVPairs):-
  rdf_assert_individual(AP_Stage, ap:'FileOperation', ap),
  rdf_assert_datatype(AP_Stage, ap:file, xsd:string, File, ap),
  rdf_assert_datatype(AP_Stage, ap:operation, xsd:string, Operation, ap),
  maplist(add_nvpair, NVPairs, BNodes),
  rdf_assert_list(BNodes, RDF_List, ap),
  rdf_assert(AP_Stage, ap:operations, RDF_List, ap).

add_properties_of_file(AP_Stage, File, NVPairs):-
  rdf_assert_individual(AP_Stage, ap:'FileProperties', ap),
  rdf_assert_datatype(AP_Stage, ap:file, xsd:string, File, ap),
  maplist(add_nvpair, NVPairs, BNodes),
  forall(
    member(BNode, BNodes),
    rdf_assert(AP_Stage, ap:has_property, BNode, ap)
  ).

add_succeed(AP_Stage):-
  rdf_assert_datatype(AP_Stage, ap:status, xsd:string, succeed, ap).

ap_resource(AP_Stage, Resource, Graph):-
  rdf_collection_member(AP_Stage, AP, ap),
  rdf(AP, ap:resource, Resource, ap),
  rdf_datatype(AP, ap:graph, xsd:string, Graph, ap).

create_ap(AP_Collection, AP):-
  var(AP_Collection), !,
  create_resource('AP-Collection', AP_Collection),
  create_ap(AP_Collection, AP).
create_ap(AP_Collection, AP):-
  create_resource('AP', AP),
  rdf_assert_collection_member(AP_Collection, AP, ap).

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
  rdf_collection_member(AP_Stage1, AP, ap),
  StageNum2 is StageNum1 + 1,
  create_stage(AP, StageNum2, AP_Stage2).

create_resource(BaseName, Resource):-
  rdf_create_next_resource(ap, BaseName, Resource, ap).

