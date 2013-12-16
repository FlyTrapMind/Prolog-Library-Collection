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
@tbd Integrate with [sparql_ext]; identoty closure as option.
@version 2013/09, 2013/12
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(pair_ext)).
:- use_module(generics(row_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- sparql_add_prefix(owl).

:- rdf_meta(query_sameas(+,r,-)).
:- rdf_meta(describe_sameas(+,r,-)).

:- debug(sparql_sameas).



%! describe_sameas(+Remote:atom, +Resource:uri, -Rows:list(compound)) is det.

describe_sameas(Remote, Resource, Rows):-
  query_sameas(Remote, Resource, IdenticalResources),
  maplist(describe_resource(Remote), IdenticalResources, Rows),
  setoff(
    Rows0,
    (
      member(row(IdenticalResource), [row(Resource)|IdenticalResources]),
      describe_resource(Remote, IdenticalResource, Rows0)
    ),
    Rowss
  ),
  ord_union(Rowss, Rows).

%! query_sameas(
%!   +Remote:atom,
%!   +Resource:uri,
%!   -IdenticalResources:ordset
%! ) is det.
% 
%
% @param Remote The atomic name of a registered SPARQL remote.
% @param Resource The URI of a resource.
% @param IdenticalResources An ordered set of identical resources.

query_sameas(Remote, Resource, IResources2):-
  format(atom(Where), '  <~w> owl:sameAs ?x .', [Resource]),
  formulate_sparql(
    _Graph,
    [owl],
    select([distinct(true)],[x]),
    [Where],
    _Extra,
    Query
  ),
  enqueue_sparql(Remote, Query, _VarNames, IResources1),
  rows_to_ord_set(IResources1, IResources2).

