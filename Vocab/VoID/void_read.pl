:- module(
  void_read,
  [
    rdf_link/5, % ?Subject:oneof([bnode,uri])
                % ?Predicate:uri
                % ?Object:oneof([bnode,literal,uri])
                % ?FromGraph:atom
                % ?ToGraph:atom
    rdf_linkset/3 % +Triples:list(compound)
                  % ?FromGraph:atom
                  % ?ToGraph:atom
  ]
).

/** <module> VoID read

Read information for VoID descriptions.

Also contains predicates for the definition of RDF links and RDF linksets
as it appears in the VoID specification.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_link(r,r,o,?,?)).
:- rdf_meta(rdf_linkset(t,?,?)).



%! rdf_link(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?FromGraph:atom,
%!   ?ToGraph:atom
%! ) is nondet.
% An RDF link is an RDF triple whose subject and object are described in
% different datasets.

rdf_link(S, P, O, FromG, ToG):-
  rdf(S, P1, O1, FromG),
  \+ ((P1 == P, O1 == O)),
  rdf(O, P2, O2, ToG),
  \+ ((P2 == P, O2 == O)),
  FromG \== ToG.

%! rdf_linkset(+Triples:list(compound), ?FromGraph:atom, ?ToGraph:atom) is semidet.
%! rdf_linkset(-Triples:list(compound), +FromGraph:atom, +ToGraph:atom) is det
% An RDF linkset is a collection of RDF links between the same two datasets.

rdf_linkset(Ts, FromG, ToG):-
  nonvar(Ts), !,
  forall(
    member(row(S,P,O), Ts),
    rdf_link(S, P, O, FromG, ToG)
  ).
rdf_linkset(Ts, FromG, ToG):-
  findall(
    row(S, P, O),
    rdf_link(S, P, O, FromG, ToG),
    Ts
  ).

