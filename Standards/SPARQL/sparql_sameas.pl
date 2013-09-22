:- module(
  sparql_sameas,
  [
    describe_sameas/3, % +Remote:atom
                       % +Resource:iri
                       % -Rows:list(compound)
    query_sameas/3 % +Remote:atom
                   % +Resource:iri
                   % -IdenticalResources:list(iri)
  ]
).

/** <module> SPARQL resource identity closure

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').

:- rdf_meta(query_sameas(+,r,-)).
:- rdf_meta(describe_sameas(+,r,-)).

:- register_sparql_prefix(owl).

:- debug(sparql_sameas).



describe_sameas(Remote, Resource, Rows):-
  query_sameas(Remote, Resource, IdenticalResources),
  setoff(
    Rows0,
    (
      member(row(IdenticalResource), [Resource|IdenticalResources]),
      describe_resource(Remote, IdenticalResource, Rows0)
    ),
    Rowss
  ),
  ord_union(Rowss, Rows).

query_sameas(Remote, Resource, IdenticalResources):-
  format(atom(Where), '  <~w> owl:sameAs* ?x .', [Resource]),
  formulate_sparql(
    [owl],
    'SELECT DISTINCT ?x',
    [Where],
    0,
    Query
  ),
  enqueue_sparql(Remote, Query, _VarNames, IdenticalResources).
