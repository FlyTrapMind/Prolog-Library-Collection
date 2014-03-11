:- module(
  void_stat,
  [
    void_update/1 % +VoidGraph:atom
  ]
).

/** <module> VoID statistics

Asserts statistics for VoID descriptions.

@author Wouter Beek
@version 2013/03-2013/05, 2013/09-2014/03
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
:- use_module(void(void_db)). % XML namespace.
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd_dateTime_ext)).

:- xml_register_namespace(dc, 'http://purl.org/dc/elements/1.1/').



%! void_update(+VoidGraph:atom) is det.

void_update(VoidGraph):-
  % NO THREADS
  forall(
    void_dataset(VoidGraph, VoidDataset),
    void_update_dataset(VoidGraph, VoidDataset)
  ).
/*
  % THREADS
  forall_thread(
    (
      void_dataset(VoidGraph, VoidDataset),
      format(atom(Msg), 'Saving VoID dataset ~w.', [VoidDataset])
    ),
    void_update_dataset(VoidGraph, VoidDataset),
    void_file,
    Msg
  ).
*/


%! void_update_dataset(+VoidGraph:atom, +VoidDataset:iri) is det.

void_update_dataset(_, VoidDataset):-
  rdf_graph_property(VoidDataset, modified(false)), !.
void_update_dataset(VoidGraph, VoidDataset):-
  % dc:modified.
  get_time(PosixDatetime),
  posix_timestamp_to_xsd_dateTime(PosixDatetime, XsdDatetime),
  rdf_overwrite_datatype(
    VoidDataset,
    dc:modified,
    xsd:date,
    XsdDatetime,
    VoidGraph
  ),
  
  % void:classes
  count_classes(VoidDataset, NumberOfClasses),
  rdf_overwrite_datatype(
    VoidDataset,
    void:classes,
    xsd:integer,
    NumberOfClasses,
    VoidGraph
  ),

  % void:distinctObjects
  count_objects(_, _, VoidDataset, NumberOfObjects),
  rdf_overwrite_datatype(
    VoidDataset,
    void:distinctObjects,
    xsd:integer,
    NumberOfObjects,
    VoidGraph
  ),

  % void:distinctSubjects
  count_subjects(_, _, VoidDataset, NumberOfSubjects),
  rdf_overwrite_datatype(
    VoidDataset,
    void:distinctSubjects,
    xsd:integer,
    NumberOfSubjects,
    VoidGraph
  ),

  % void:entities
  (
    rdf_datatype(
      VoidDataset,
      void:uriRegexPattern,
      xsd:string,
      RegularExpression,
      VoidGraph
    )
  ->
    setoff(
      Entity,
      (
        rdf(Entity, _, _, VoidDataset),
        Entity=~RegularExpression
      ),
      Entities
    ),
    length(Entities, NumberOfEntities),
    rdf_overwrite_datatype(
      VoidDataset,
      void:entities,
      xsd:integer,
      NumberOfEntities,
      VoidGraph
    )
  ;
    true
  ),

  % void:properties
  count_properties(_, _, VoidDataset, NumberOfProperties),
  rdf_overwrite_datatype(
    VoidDataset,
    void:properties,
    xsd:integer,
    NumberOfProperties,
    VoidGraph
  ),

  % void:triples
  rdf_statistics(triples_by_graph(VoidDataset, NumberOfTriples)),
  rdf_overwrite_datatype(
    VoidDataset,
    void:triples,
    xsd:integer,
    NumberOfTriples,
    VoidGraph
  ).

