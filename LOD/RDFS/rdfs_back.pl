:- module(
  rdfs_back,
  [
    rdfs/4 % ?Subject:or([bnode,iri])
           % ?Predicate:iri
           % ?Object:or([bnode,iri,literal])
           % ?Graph:atom
  ]
).

/** <module> RDFS Class

Deductive closure over RDFS classes.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdfs(r,r,o,?)).
:- rdf_meta(rdfs_(r,r,o,?)).

:- initialization(load_rdfs_voc(test)).

load_rdfs_voc(Graph):-
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_load(File, [graph(Graph)]).



rdfs(X, Q, Y, G):-
  nonvar(X),
  nonvar(Q),
  nonvar(Y),
  nonvar(G), !,
  rdfs_(X, Q, Y, G), !.
rdfs(X, Q, Y, G):-
  rdfs_(X, Q, Y, G).

% SE
rdfs_(X, Q, Y, G):-
  rdf(X, Q, Y, G).

% RDF-1
rdfs_(X, rdf:type, rdf:'Property', G):-
gtrace,
  rdfs_(_, X, _, G),
  \+ rdf(X, rdf:type, rdf:'Property', G).

% RDFS-2
rdfs_(X, rdf:type, C, G):-
  var(X), var(C),
  rdfs_(Q, rdfs:domain, C, G),
  rdfs_(X, Q, _, G).
rdfs_(X, rdf:type, C, G):-
  nonvar(C),
  rdfs_(Q, rdfs:domain, C, G),
  rdfs_(X, Q, _, G).
rdfs_(X, rdf:type, C, G):-
  nonvar(X), var(C),
  rdfs_(X, Q, _, G),
  rdfs_(Q, rdfs:domain, C, G).

% RDFS-3
rdfs_(X, rdf:type, C, G):-
  var(X), var(C),
  rdfs_(Q, rdfs:range, C, G),
  rdfs_(_, Q, X, G).
rdfs_(X, rdf:type, C, G):-
  nonvar(C),
  rdfs_(Q, rdfs:range, C, G),
  rdfs_(_, Q, X, G).
rdfs_(X, rdf:type, C, G):-
  nonvar(X), var(C),
  rdfs_(_, Q, X, G),
  rdfs_(Q, rdfs:range, C, G).

% RDFS-4a
rdfs_(X, rdf:type, rdfs:'Resource', G):-
  rdfs_(X, _, _, G).

% RDFS-4a
rdfs_(X, rdf:type, rdfs:'Resource', G):-
  rdfs_(_, _, X, G).

% RDFS-5
rdfs_(Q, rdfs:subPropertyOf, P, G):-
  nonvar(Q),
  rdfs_(Q, rdfs:subPropertyOf, R, G),
  rdfs_(R, rdfs:subPropertyOf, P, G).
rdfs_(Q, rdfs:subPropertyOf, P, G):-
  var(Q),
  rdfs_(R, rdfs:subPropertyOf, P, G),
  rdfs_(Q, rdfs:subPropertyOf, R, G).

% RDFS-6
rdfs_(Q, rdfs:subPropertyOf, Q, G):-
  rdfs_(Q, rdf:type, rdf:'Property', G).

% RDFS-7
rdfs_(X, P, Y, G):-
  nonvar(P),
  rdfs_(Q, rdfs:subPropertyOf, P, G),
  rdfs_(X, Q, Y, G).
rdfs_(X, P, Y, G):-
  var(P),
  rdfs_(X, Q, Y, G),
  rdfs_(Q, rdfs:subPropertyOf, P, G).

% RDFS-8
rdfs_(C, rdfs:subClassOf, rdfs:'Resource', G):-
  rdfs_(C, rdf:type, rdfs:'Class', G).

% RDFS-9
rdfs_(X, rdf:type, C, G):-
  nonvar(C),
  rdfs_(D, rdfs:subClassOf, C, G),
  rdfs_(X, rdf:type, D, G).
rdfs_(X, rdf:type, C, G):-
  var(C),
  rdfs_(X, rdf:type, D, G),
  rdfs_(D, rdfs:subClassOf, C, G).

% RDFS-10
rdfs_(C, rdfs:subClassOf, C, G):-
  rdfs_(C, rdf:type, rdfs:'Class', G).

% RDFS-11
rdfs_(D, rdfs:subClassOf, C, G):-
  nonvar(D),
  rdfs_(D, rdfs:subClassOf, E, G),
  rdfs_(E, rdfs:subClassOf, C, G).
rdfs_(D, rdfs:subClassOf, C, G):-
  var(D),
  rdfs_(E, rdfs:subClassOf, C, G),
  rdfs_(D, rdfs:subClassOf, E, G).

% RDFS-12
rdfs_(Q, rdfs:subPropertyOf, rdfs:member, G):-
  rdfs_(Q, rdf:type, rdfs:'ContainerMembershipProperty', G).

% RDFS-13
rdfs_(C, rdfs:subClassOf, rdfs:'Literal', G):-
  rdfs_(C, rdf:type, rdfs:'Datatype', G).

