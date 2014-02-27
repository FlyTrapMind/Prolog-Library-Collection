:- module(
  ap_void_stat,
  [
    void_statistics/3 % +FromDirectory:atom
                      % +ToDirectory:atom
                      % +AP_Status:compound
  ]
).

/** <module> AP VoID statistics

VoID statistics process for the AP architecture.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(http_parameters(rfc2616_media_type)). % DCG-meta.
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_stat)).
:- use_module(void(void_stat)). % If only for the namespace.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc2616, 'http://tools.ietf.org/html/rfc2616#').



void_statistics(FromDir, ToDir, AP_Stage):-
  directory_files([], FromDir, FromFiles),
  findall(
    File,
    (
      member(File, FromFiles),
      rdf_setup_call_cleanup(
        [],
        File,
        void_statistics_on_graph(AP_Stage, NVPairs)
      ),
      add_properties_of_file(AP_Stage, File, NVPairs)
    ),
    Files
  ),
  (Files == [] -> existence_error('LOD', 'No LOD here') ; true),
  link_directory_contents(FromDir, ToDir).


void_statistics_on_graph(AP_Stage, NVPairs, ReadGraph):-
  NVPairs = [
    classes-NC,
    subjects-NS,
    properties-NP,
    objects-NO,
    triples-NT
  ],

  count_classes(ReadGraph, NC),
  count_objects(_, _, ReadGraph, NO),
  count_subjects(_, _, ReadGraph, NS),
  count_properties(_, _, ReadGraph, NP),
  rdf_statistics(triples_by_graph(ReadGraph, NT)),

  (
    ap_stage_resource(AP_Stage, Resource, WriteGraph)
  ->
    rdf_assert_datatype(Resource, void:classes, xsd:integer, NC, WriteGraph),
    rdf_assert_datatype(Resource, void:distinctObjects, xsd:integer, NO, WriteGraph),
    rdf_assert_datatype(Resource, void:distinctSubject, xsd:integer, NS, WriteGraph),
    rdf_assert_datatype(Resource, void:properties, xsd:integer, NP, WriteGraph),
    rdf_assert_datatype(Resource, void:triples, xsd:integer, NT, WriteGraph)
  ;
    true
  ).

