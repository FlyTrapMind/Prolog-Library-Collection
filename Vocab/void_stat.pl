:- module(
  void_stats,
  [
    void_assert_modified/2, % +VoID_Graph:atom
                            % +Dataset:iri
    void_assert_statistics/3 % +VoID_Graph:atom
                             % +Dataset:iri
                             % +DatasetGraph:atom
  ]
).

/** <module> VoID statistics

Asserts statistics for VoID descriptions.

@author Wouter Beek
@version 2013/03-2013/05, 2013/09-2013/10
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_statistics)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(void_assert_modified(+,r)).
:- rdf_meta(void_assert_statistics(+,r,+)).

:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').



%! void_assert_modified(+VoID_Graph:atom, +Dataset:iri) is det.
% Overwrites the modification date for the given dataset.

void_assert_modified(G, D):-
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_overwrite_datatype(D, dc:modified, dateTime, XSD_DT, G).

%! void_assert_statistics(
%!   +VoID_Graph:atom,
%!   +Dataset:iri,
%!   +DatasetGraph:atom
%! ) is det.

void_assert_statistics(G, D, DG):-
  % void:classes
  count_classes(DG, NC),
  rdf_overwrite_datatype(D, void:classes, integer, NC, G),

  % void:distinctObjects
  count_objects(_, _, DG, NO),
  rdf_overwrite_datatype(D, void:distinctObjects, integer, NO, G),

  % void:distinctSubjects
  count_subjects(_, _, DG, NS),
  rdf_overwrite_datatype(D, void:distinctSubjects, integer, NS, G),

  % void:properties
  count_properties(_, _, DG, NP),
  rdf_overwrite_datatype(D, void:properties, integer, NP, G),

  % void:triples
  rdf_statistics(triples_by_graph(DG, NT)),
  rdf_overwrite_datatype(D, void:triples, integer, NT, G).

