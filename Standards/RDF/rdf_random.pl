:- module(
  rdf_random,
  [
    rdf_random_neighbor/3, % +Graph:atom
                           % +Vertex:or([bnode,literal,iri])
                           % -RandomNeighbor:or([bnode,literal,iri])
    rdf_random_term/2, % +Graph:atom
                       % -Term:or([bnode,literal,iri])
    rdf_random_term/3, % +Graph:atom
                       % :Requirement
                       % -Term:or([bnode,literal,iri])
    rdf_random_triple/4 % +Graph:atom
                        % -RandomSubject:or([bnode,iri])
                        % -RandomPredicate:iri
                        % -RandomObject:or([bnode,iri,literal])
  ]
).

/** <module> RDF_RANDOM

@author Wouter Beek
@version 2013/09
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_term)).
:- use_module(rdf_graph(rdf_graph_theory)).

:- meta_predicate(rdf_random_term(+,1,-)).

:- rdf_meta(rdf_index(r,r,r,?,?)).
:- rdf_meta(rdf_random_neighbor(+,r,r)).
:- rdf_meta(rdf_random_term(+,r)).
:- rdf_meta(rdf_random_term(+,:,r)).
:- rdf_meta(rdf_random_triple(+,r,r,r)).



%! rdf_index(
%!   ?Graph:graph,
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Index:integer
%! ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.
%
% @arg Graph The atomic name of a graph.
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.
% @arg Index A compound term of the form =Graph:Line= with =Graph= the
%        atomic name of an RDF graph and =Line= an integer.

rdf_index(G, S, P, O, I):-
  rdf_graph:rdf_graph_to_triples(G, Triples),
  nth0(I, Triples, rdf(S, P, O)).

rdf_random_neighbor(G, V, RndN):-
  rdf_neighbors(G, V, Ns),
  length(Ns, L),
  random_betwixt(1, L, I),
  nth1(I, Ns, RndN).

rdf_random_term(G, T):-
  rdf_random_term(G, rdf_term(G), T).

rdf_random_term(G, Requirement, T2):-
  rdf_random_triple(S, P, O, G),
  random_betwixt(2, J),
  nth0(J, [S,P,O], T1),
  (
    call(Requirement, T1)
  ->
    T2 = T1
  ;
    rdf_random_term(G, Requirement, T2)
  ).

%! rdf_random_triple(
%!   ?Graph:graph,
%!   -Subject:oneof([bnode,iri]),
%!   -Predicate:iri,
%!   -Object:or([bnode,literal,iri])
%! ) is det.
% Returns a random triple from the given graph.
%
% @arg Graph The atomic name of a graph.
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Object A resource.

rdf_random_triple(G, S, P, O):-
  rdf_graph_property(G, triples(NumberOfTriples)),
  succ(UpperIndex, NumberOfTriples),
  random_betwixt(UpperIndex, RndI),
  rdf_index(G, S, P, O, RndI).

