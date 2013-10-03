:- module(
  void_stats,
  [
    void_assert_modified/2, % +VoID_Graph:atom
                            % +DatasetName:atom
    void_assert_statistics/2 % +VoID_Graph:atom
                             % +DatasetName:atom
  ]
).

/** <module> VoID statistics

Asserts statistics for VoID descriptions.

@author Wouter Beek
@version 2013/03-2013/05, 2013/09-2013/10
*/

:- use_module(os(datetime_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').



%! void_assert_modified(+VoID_Graph:atom, +DatasetName:atom) is det.
% Overwrites the modification date for the given dataset.

void_assert_modified(G, DName):-
  once(rdfs_label(D, DName, G)),
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_overwrite_datatype(D, dc:modified, dateTime, XSD_DT, G).

void_assert_statistics(G, DName):-
  once(rdfs_label(D, DName, G)),
  
  % void:classes
  count_classes(DatasetGraph, NumberOfClasses),
  rdf_overwrite_datatype(
    Dataset,
    void:classes,
    integer,
    NumberOfClasses,
    VoID_Graph
  ),

  % void:distinctObjects
  count_objects(_, _, DatasetGraph, NumberOfObjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctObjects,
    integer,
    NumberOfObjects,
    VoID_Graph
  ),

  % void:distinctSubjects
  count_subjects(_, _, DatasetGraph, NumberOfSubjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctSubjects,
    integer,
    NumberOfSubjects,
    VoID_Graph
  ),

  % void:properties
  count_properties(_, _, DatasetGraph, NumberOfProperties),
  rdf_overwrite_datatype(
    Dataset,
    void:properties,
    integer,
    NumberOfProperties,
    VoID_Graph
  ),

  % void:triples
  rdf_statistics(triples_by_graph(DatasetGraph, NumberOfTriples)),
  rdf_overwrite_datatype(
    Dataset,
    void:triples,
    integer,
    NumberOfTriples,
    VoID_Graph
  ).

