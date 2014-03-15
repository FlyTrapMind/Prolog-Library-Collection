:- module(
  rdf_stat,
  [
    count_classes/2, % +Graph:atom
                     % -Count:nonneg
    count_entities/2, % +Graph:atom
                      % -Count:nonneg
    count_individuals/3, % +Class:iri
                         % +Graph:atom
                         % -NumberOfIndividuals:nonneg
    count_objects/4, % ?Subject:or([bnode,iri])
                     % ?Predicate:iri
                     % +Graph:atom
                     % -Count:nonneg
    count_properties/4, % ?Subject:or([bnode,iri])
                        % ?Object:or([bnode,iri,literal])
                        % +Graph:atom
                        % -Count:nonneg
    count_subjects/4, % ?Predicate:iri
                      % ?Object:or([bnode,iri,literal])
                      % +Graph:atom
                      % -Count:nonneg
    rdf_property_table/3 % +Property:iri
                         % +Graph:atom
                         % -Table:list(list)
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@see Based on the definitions in section 4.6 of the VoID W3C specification,
     http://www.w3.org/TR/2011/NOTE-void-20110303/
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdfs(rdfs_read)).

:- rdf_meta(count_individuals(r,+,-)).
:- rdf_meta(count_objects(r,r,+,-)).
:- rdf_meta(count_properties(r,r,+,-)).
:- rdf_meta(count_subjects(r,r,+,-)).
:- rdf_meta(rdf_property_table(r,+,-)).



%! count_classes(+Graph:atom, -Count:nonneg) is det.
% Returns the number of distinct classes in the given graph.
%
% The total number of distinct classes in the graph.
% In other words, the number of distinct class URIs
% occuring as objects of =|rdf:type|= triples in the graph.
%
% This definition is actually incorrect, since a class can be specified
% using either of the following triple schemes, without the class having
% to occur in the object position of any triple.
% ~~~
% <C,  rdf:type,        rdfs:Class>
% <C, rdfs:subClassOf,  C'        >
% ~~~
%
% @see Based on the definition of =|void:classes|=.

count_classes(G, Count):-
  setoff(O, rdf(_, rdf:type, O, G), Os),
  length(Os, Count).

%! count_entities(+Graph:atom, -Count:nonneg) is det.
% Returns the numver of unique entities in the given graph.
%
%	The total number of entities that are described in the graph.
% To be an entity in a graph, a resource must have a URI.
%
% @see Based on the definition of =|void:entities|=.

count_entities(G, Count):-
  setoff(E, rdf_iri(G, E), Es),
  length(Es, Count).

%! count_individuals(
%!   +Class:iri,
%!   +Graph:atom,
%!   -NumberOfIndividuals:nonneg
%! ) is det.

count_individuals(C, G, N):-
  setoff(
    I,
    rdfs_individual(m(t,f,f), I, C, G),
    Is
  ),
  length(Is, N).

%! count_objects(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.
% Returns the number of distinct objects
%
% The total number of distinct objects in the graph.
% In other words, the number of distinct URIs, blank nodes, or literals
% that occur in the object position of triples in the graph.
%
% @see Based on the definition of =|void:distinctObjects|=.

count_objects(S, P, G, Count):-
  setoff(O, rdf(S, P, O, G), Os),
  length(Os, Count).

%! count_properties(
%!   ?Subject:or([bnode,iri]),
%!   ?Object:or([bnode,iri,literal]),
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.
% Returns the number of distinct properties in the given graph.
%
% The total number of distinct properties in the graph.
% In other words, the number of distinct property URIs
% that occur in the predicate position of triples in the graph.
%
% This definition is actually incorrect, since a property can be specified
% using either of the following triple schemes, without the property having
% to occur in the predicate position of any triple.
% ~~~
% <P,  rdf:type,           rdf:Property>
% <P1, rdfs:subPropertyOf, P2          >
% ~~~
%
% @see Based on the definition of =|void:properties|=.

count_properties(S, O, G, Count):-
  setoff(P, rdf(S, P, O, G), Ps),
  length(Ps, Count).

%! count_subjects(
%!   +Predicate:iri,
%!   +Object:or([bnode,literal,iri]),
%!   +Graph:atom
%!   -Count:nonneg
%! ) is det.
% Returns the number of unique subject terms that occur in triples
% with the given predicate-object pair and in the given graph.
%
% The total number of distinct subjects in the graph.
% In other words, the number of distinct URIs or blank nodes
% that occur in the subject position of triples in the graph.
%
% @see Based on the definition of =|void:distinctSubjects|=.

count_subjects(P, O, G, Count):-
  setoff(S, rdf(S, P, O, G), Ss),
  length(Ss, Count).

rdf_property_table(P, G, T):-
  setoff(
    O,
    rdf([graph_mode(no_index)], _, P, O, G),
    Os
  ),
  findall(
    [O,NumberOfO],
    (
      member(O, Os),
      aggregate_all(
        count,
        rdf([graph_mode(no_index)], _, P, O, G),
        NumberOfO
      )
    ),
    T
  ).

