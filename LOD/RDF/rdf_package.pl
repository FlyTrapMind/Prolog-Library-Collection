:- module(
  rdf_package,
  [
    rdf_package_build/1 % +Dataset:compound
  ]
).

/** <module> RDF Package

Packaging of RDF datasets

@author Wouter Beek
@version 2014/03
*/

:- use_module(rdf(rdf_dataset)).



rdf_package_build(Dataset):-
  rdf_dataset(DefaultGraph, NamedGraphs, Dataset),
  

