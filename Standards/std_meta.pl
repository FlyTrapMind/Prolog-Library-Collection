:- module(
  stc_meta,
  [
    assert_standard/4, % +Graph:atom
                       % +Type:oneof([rfc,iso,w3c])
                       % +StandardName:atom
                       % -Standard:uri
    standards_graph/1 % ?Graph:atom
  ]
).

/** <module> STD_META

Meta-data on standards.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).



assert_standard(G, Type, Name, URI):-
  rdf_global_id(Type:Name, URI),
  rdf_global_id(Type:'Standard', Standard),
  rdfs_assert_class(Standard, G),
  rdf_assert_individual(URI, Standard, G).

standards_graph(std).

