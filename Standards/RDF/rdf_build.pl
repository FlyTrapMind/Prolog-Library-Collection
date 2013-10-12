:- module(
  rdf_build,
  [
    rdf_assert_individual/3, % +Individual:iri
                             % +Class:iri
                             % +Graph:atom
    rdf_assert_property/2, % +Property:iri
                           % +Graph:atom
    rdf_remove_property/2 % +Graph:atom
                          % +Property:iri
  ]
).

/** <module> RDF build

Simple asserion and retraction predicates for RDF.
Triples with literals are treated in dedicated modules.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_assert_individual(r,r,+)).
:- rdf_meta(rdf_assert_property(r,+)).
:- rdf_meta(rdf_remove_property(+,r)).



%! rdf_assert_individual(+Individual:uri, +Class:uri, +Graph:graph) is det.
% Asserts an individual/class relationship.
%
% @param Individual An instance resource.
% @param Class A class resource.
% @param Graph The atomic name of an RDF graph.

rdf_assert_individual(I, C, G):-
  rdf_assert(I, rdf:type, C, G).

rdf_assert_property(Property, G):-
  rdf_assert_individual(Property, rdf:'Property', G).

rdf_remove_property(G, P):-
  rdf_property(G, P), !,
  rdf_retractall(P, _, _, G),
  rdf_retractall(_, P, _, G),
  rdf_retractall(_, _, P, G).

