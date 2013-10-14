:- module(
  rdf_read,
  [
    rdf2/4, % ?Subject:or([bnode,iri])
            % ?Predicate:iri
            % ?Object:or([bnode,iri,label])
            % ?Graph:graph
    rdf_find/4, % +Subject:or([bnode,iri]),
                % +Predicate:iri,
                % +Object:or([bnode,iri,literal]),
                % +Graph:atom
    rdf_member/2, % ?Member:uri
                  % ?Members:list(uri)
    rdf_memberchk/2, % ?Member:uri
                     % ?Members:list(uri)
    rdf_property/2 % +Graph:atom
                   % ?Property:iri
  ]
).

/** <module> RDF read

Predicates for reading from RDF, customized for specific datatypes and
literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04, 2013/07-2013/09
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_term)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf2(r,r,o,?)).
:- rdf_meta(rdf_member(r,+)).
:- rdf_meta(rdf_memberchk(r,+)).
:- rdf_meta(rdf_property(+,r)).



rdf2(S, P, O, G):-
  var(G), !,
  rdf(S, P, O).
rdf2(S, P, O, G):-
  rdf(S, P, O, G).

%! rdf_bnode_to_var(
%!   +RDF_Term:or([bnode,iri,literal]),
%!   ?Out:or([iri,literal])
%! ) is det.
% Replaced blank nodes with uninstantiated variables.

rdf_bnode_to_var(X, _):-
  rdf_is_bnode(X), !.
rdf_bnode_to_var(X, X).

%! rdf_both_bnode(
%!   +RDF_Term1:or([bnode,iri,literal]),
%!   +RDF_Term2:or([bnode,iri,literal])
%! ) is semidet.
% Fails if only either of the RDF terms is a blank node.

rdf_both_bnode(X, Y):-
  rdf_is_bnode(X), !,
  rdf_is_bnode(Y).
rdf_both_bnode(_, _).

%! rdf_find(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is semidet.
% Finds an RDF triple according to an RDF triple.
% This is different from rdf/[3,4], which are not RDF triples
% (since their parameters may hold argments that are variables).
%
% Since we cannot match blank nodes directly, we replace them with variables.
% This is valid under graph equivalence.

rdf_find(S, P, O, G):-
  maplist(rdf_bnode_to_var, [S,P,O], [SS,PP,OO]),
  rdf(SS, PP, OO, G),
  maplist(rdf_both_bnode, [S,P,O], [SS,PP,OO]).

rdf_member(Member, List):-
  member(Member0, List),
  rdf_global_id(Member0, Member).

rdf_memberchk(Member, List):-
  once(rdf_member(Member, List)).

rdf_property(G, P):-
  rdfs_individual(m(f,f,f), P, rdf:'Property', G).

