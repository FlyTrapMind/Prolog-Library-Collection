:- module(
  sparql_queries,
  [
    sparql_classes/2, % +Resource:or([bnode,iri,literal])
                      % -Classes:ordset(iri)
    sparql_domain/2, % +Resource:or([bnode,iri,literal])
                     % -Classes:ordset(iri)
    sparql_range/2 % +Resource:or([bnode,iri,literal])
                   % -Classes:ordset(iri)
  ]
).

/** <module> SPARQL queries

@author Wouter Beek
@version 2014/01
*/

:- use_module(sparql(sparql_build)).



sparql_classes(Individual, Classes):-
  'SPARQL_formulate'(
    _,
    _,
    [rdfs],
    select,
    true,
    [class],
    ['rdf(iri(Individual), p
