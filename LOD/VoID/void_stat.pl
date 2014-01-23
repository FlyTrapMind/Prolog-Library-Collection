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
@version 2013/03-2013/05, 2013/09-2013/12
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(regex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_stat)).
:- use_module(void(void_db)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_dateTime)).

:- rdf_meta(void_assert_modified(+,r)).
:- rdf_meta(void_assert_statistics(+,r,+)).

:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').
:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').



%! void_assert_modified(+VoID_Graph:atom, +Dataset:iri) is det.
% Overwrites the modification date for the given dataset.

void_assert_modified(G, D):-
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_assert_datatype(D, dc:modified, xsd:date, XSD_DT, G).

void_assert_statistics(DD_G):-
  forall_thread(
    (
      % This includes VoID linksets, according to the VoID vocabulary.
      rdfs_individual_of(DS, void:'Dataset'),
      void_dataset(DD_G, DS, _DS_F, DS_G),
      format(atom(Msg), 'Asserting statistics for ~w.', [DS])
    ),

    void_assert_statistics(DD_G, DS, DS_G),
    void_stat,
    Msg
  ).

%! void_assert_statistics(
%!   +VoID_Graph:atom,
%!   +Dataset:iri,
%!   +DatasetGraphOrFile:atom
%! ) is det.

% The dataset is stored in a file; load it into memory.
void_assert_statistics(DD_G, DS, DS_F):-
  is_absolute_file_name(DS_F), !,
  % Load the dataset file in and out of memory.
  setup_call_cleanup(
    (
      % Make sure the graph name is not in use yet.
      file_to_graph_name(DS_F, DS_G),
      rdf_load2(DS_F, [format(turtle),graph(DS_G)])
    ),
    void_assert_statistics(DD_G, DS, DS_G),
    rdf_unload_graph(DS_G)
  ).
% The dataset is loaded in memory.
void_assert_statistics(DD_G, DS, DS_G):-
  rdf_graph(DS_G), !,

  % void:classes
  count_classes(DS_G, NC),
  rdf_overwrite_datatype(DS, void:classes, xsd:integer, NC, DD_G),

  % void:distinctObjects
  count_objects(_, _, DS_G, NO),
  rdf_overwrite_datatype(DS, void:distinctObjects, xsd:integer, NO, DD_G),

  % void:distinctSubjects
  count_subjects(_, _, DS_G, NS),
  rdf_overwrite_datatype(DS, void:distinctSubjects, xsd:integer, NS, DD_G),

  % void:entities
  (
    rdf_literal(DS, void:uriRegexPattern, RE, DD_G)
  ->
    setoff(
      S,
      (
        rdf(S, _, _, DS_G),
        S=~RE
      ),
      Ss
    ),
    length(Ss, NE),
    rdf_overwrite_datatype(DS, void:entities, xsd:integer, NE, DD_G)
  ;
    true
  ),

  % void:properties
  count_properties(_, _, DS_G, NP),
  rdf_overwrite_datatype(DS, void:properties, xsd:integer, NP, DD_G),

  % void:triples
  rdf_statistics(triples_by_graph(DS_G, NT)),
  rdf_overwrite_datatype(DS, void:triples, xsd:integer, NT, DD_G).

