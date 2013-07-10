:- module(
  rdf_script,
  [
    assert_visum/0
  ]
).

/** <module> RDF script

Scripts for asserting RDF graphs that can be used for debugging.

[[rdfs.png]]

@author Wouter Beek
@version 2012/12-2013/02, 2013/07
*/

:- use_module(owl(owl_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ch,  'http://www.wouterbeek.com/ch.owl#' ).
:- xml_register_namespace(dbp, 'http://www.wouterbeek.com/dbp.owl#').
:- xml_register_namespace(nl,  'http://www.wouterbeek.com/nl.owl#' ).

assert_visum:-
  Graph = visum,
  
  % Chinese namespace
  rdfs_assert_subclass(  ch:cityWithAirport, rdfs:'Resource',    Graph),
  rdfs_assert_subclass(  ch:capital,         ch:cityWithAirport, Graph),
  rdfs_assert_individual(ch:'Amsterdam',     ch:capital,         Graph),
  rdfs_assert_subclass(  ch:visumNeeded,     rdfs:'Resource',    Graph),
  rdfs_assert_subclass(  ch:europeanCity,    ch:visumNeeded,     Graph),
  rdfs_assert_individual(ch:'Amsterdam',     ch:europeanCity,    Graph),
  
  % Dutch namespace
  rdfs_assert_subclass(  nl:europeanCity, rdfs:'Resources', Graph),
  rdfs_assert_subclass(  nl:visumFree,    nl:europeanCity,  Graph),
  rdfs_assert_individual(nl:'Amsterdam',  nl:europeanCity,  Graph),
  rdfs_assert_subclass(  nl:capital,      rdfs:'Resource',  Graph),
  rdfs_assert_individual(nl:'Amsterdam',  nl:capital,       Graph),
  
  % Interrelations
  owl_assert_class_equivalence(ch:capital,      nl:capital,     Graph),
  owl_assert_resource_identity(dbp:'Amsterdam', ch:'Amsterdam', Graph),
  owl_assert_resource_identity(dbp:'Amsterdam', nl:'Amsterdam', Graph).
