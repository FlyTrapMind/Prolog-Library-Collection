:- module(
  sparql_cache,
  [
    sparql_cache/2, % +Resource:or([bnode,iri,literal])
                    % +Graph:atom
    sparql_cache/3 % +Resource:or([bnode,iri,literal])
                   % -Resources:ordset(or([bnode,iri,literal]))
                   % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> SPARQL Cache

Locally caches triples that are relevant for a given resource.

@author Wouter Beek
@version 2014/01-2014/02, 2014/06
*/

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plSparql(sparql_api)).
:- use_module(plSparql(sparql_db)).



%! sparql_cache(+Resource:or([bnode,iri,literal]), +Graph:atom) is det.
% Wrapper around sparql_cache/3, where the set of resources does not matter.

sparql_cache(Resource, Graph):-
  sparql_cache(Resource, _, Propositions),
  forall(
    member([S,P,O], Propositions),
    rdf_assert(S, P, O, Graph)
  ).


%! sparql_cache(
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% Returns the depth-one resources and propositions for the given resource.
%
% This only gives non-trivial results if the resource term is a URL
% which denotes a registered SPARQL endpoint.

% Blank node.
sparql_cache(Resource, [], []):-
  rdf_is_bnode(Resource), !.
% Literal.
sparql_cache(Resource, [], []):-
  rdf_is_literal(Resource), !.
% IRI with a registered SPARQL endpoint.
sparql_cache(Resource, Resources, Propositions):-
  sparql_endpoint_by_resource(Resource, Endpoint),
  sparql_select(Endpoint, [], [p,o], [
      rdf(iri(Resource),var(p),var(o))], Propositions, [distinct(true)]),
  ord_union(Propositions, Resources).

