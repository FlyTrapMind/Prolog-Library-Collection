:- module(
  cache_it,
  [
    cache_it/3, % :Predicate
                % +Resource:iri
                % +Graph:atom
    cache_it/4 % :Predicate
               % +Resource:iri
               % -Resources:ordset(iri)
               % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> Cache it

Generic predicate for caching RDF results.

Possible instantiations for `Predicate` are SPARQL_cache/3 and LOD_cache/3.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(os(file_ext)).
:- use_module(rdf_web(rdf_table)).
:- use_module('SPARQL'('SPARQL_db')).

:- meta_predicate(cache_it(3,+,+)).
:- meta_predicate(cache_it(3,+,-,-)).
:- meta_predicate(cache_it(3,+,+,-,+,-)).

:- debug(cache_it).



%! cache_it(
%!   :Predicate,
%!   +Resource:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is det.

cache_it(Pred, Resource, Graph):-
  cache_it(Pred, Resource, _, Propositions),
  maplist(assert_proposition(Graph), Propositions).

assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).


%! cache_it(
%!   :Predicate,
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list)
%! ) is det.

cache_it(_, [], [], []):- !.
cache_it(Pred, [H|T], Resources, Propositions):- !,
  cache_it(Pred, [H|T], [H], Resources, [], Propositions),
  % DEB
  findall(
    [S,P,O,none],
    member([S,P,O], Propositions),
    Quadruples
  ),
  rdf_store_table(Quadruples).
cache_it(Pred, Resource, Resources, Propositions):-
  cache_it(Pred, [Resource], Resources, Propositions).


% Base case.
cache_it(_, [], VSol, VSol, PropsSol, PropsSol):- !.
% Recursive case.
cache_it(Pred, [H1|T1], Vs1, VSol, Props1, PropsSol):-
  message('Resource ~w', [H1]),
  call(Pred, H1, Neighbors, NeighborProps), !,

  % Filter on propositions that are included in results.
  exclude(old_proposition, NeighborProps, NewProps),
  length(NewProps, NumberOfNewProps),
  message('~d propositions added', [NumberOfNewProps]),

  % Filter on resources that have to be visited.
  exclude(old_neighbor(Vs1, NewProps), Neighbors, NewNeighbors),
  length(NewNeighbors, NumberOfNewNeighbors),
  message('~d resources added', [NumberOfNewNeighbors]),

  % Update resources that have to be visiterd.
  append(T1, NewNeighbors, T2),

  % Update results.
  ord_union(Vs1, NewNeighbors, Vs2),
  ord_union(Props1, NewProps, Props2),

  % Recurse.
  cache_it(Pred, T2, Vs2, VSol, Props2, PropsSol).
% The show must go on!
cache_it(Pred, [H|T], Vs, VSol, Props, PropsSol):-
  message('~w failed', [H]),
  cache_it(Pred, T, Vs, VSol, Props, PropsSol).

message(Format, Args):-
  debug(cache_it, Format, Args),
  format(user_output, Format, Args),
  nl(user_output),
  flush_output(user_output).

old_neighbor(Vs1, _, Element):-
  memberchk(Element, Vs1), !.
old_neighbor(_, NewProps, Element):-
  member(
    [_,'http://dbpedia.org/ontology/wikiPageExternalLink',Element],
    NewProps
  ), !.

old_proposition([S,P,O]):-
  rdf(S, P, O), !.
old_proposition([S,P,O]):-
  rdf_predicate_property(P, symmetric(true)),
  rdf(O, P, S), !.

