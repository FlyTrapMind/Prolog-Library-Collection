:- module(
  rdf_statistics,
  [
    count_classes/2, % +Graph:atom
                     % -Count:nonneg
    count_objects/4, % +Subject:or([bnode,iri])
                     % +Predicate:iri
                     % +Graph:atom
                     % -Count:nonneg
    count_predicates/4, % +Subject:or([bnode,iri])
                        % +Object:or([bnode,iri,literal])
                        % +Graph:atom
                        % -Count:nonneg
    count_properties/2, % +Graph:atom
                        % -Count:nonneg
    count_subjects/4 % +Predicate:iri
                     % +Object:or([literal,iri])
                     % +Graph:atom
                     % -Count:nonneg
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_term)).

:- rdf_meta(count_classes(+,-)).
:- rdf_meta(count_objects(r,r,+,-)).
:- rdf_meta(count_predicates(r,r,+,-)).
:- rdf_meta(count_properties(+,-)).
:- rdf_meta(count_subjects(r,r,+,-)).



%! count_classes(+Graph:atom, -Count:nonneg) is det.

count_classes(G, Count):-
  setoff(C, (rdf_term(G, C), rdfs_individual_of(C, rdfs:'Class')), Cs),
  length(Cs, Count).

%! count_objects(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.

count_objects(S, P, G, Count):-
  setoff(O, rdf(S, P, O, G), Os),
  length(Os, Count).

%! count_predicates(
%!   +Subject:or([bnode,iri]),
%!   +Object:or([bnode,literal,iri]),
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.

count_predicates(S, O, G, Count):-
  setoff(P, rdf(S, P, O, G), Ps),
  length(Ps, Count).

%! count_properties(+Graph:atom, -Count:nonneg) is det.
% Returns the number of RDF properties in the given graph.

count_properties(G, Count):-
  setoff(P, (rdf_term(G, P), rdfs_individual_of(P, rdf:'Property')), Ps),
  length(Ps, Count).

%! count_subjects(
%!   +Predicate:iri,
%!   +Object:or([bnode,literal,iri]),
%!   +Graph:atom
%!   -Count:nonneg
%! ) is det.
% Returns the number of unique subject terms that occur in triples
% with the given predicate-object pair and in the given graph.

count_subjects(P, O, G, Count):-
  setoff(S, rdf(S, P, O, G), Ss),
  length(Ss, Count).

