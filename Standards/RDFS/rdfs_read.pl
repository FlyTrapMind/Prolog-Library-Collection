:- module(
  rdfs_read,
  [
% CLASS
    rdfs_class/2, % +Graph:atom
                  % ?Class:iri
    rdfs_is_individual/3, % +Individual:iri
                          % +Class:iri
                          % ?Graph:atom
    rdfs_is_subclass/3, % +Class1:iri
                        % +Class2:iri
                        % ?Graph:atom

% DOMAIN & RANGE
    rdfs_domain/3, % ?Property:uri
                   % ?Domain:uri
                   % ?Graph:atom
    rdfs_range/3, % ?Property:uri
                  % ?Range:uri
                  % ?Graph:atom

% RDF-HAS
    rdfs/4 % ?Subject:oneof([bnode,uri])
           % ?Predicate:uri
           % ?Object:uri
           % ?Graph:atom
  ]
).

/** <module> RDFS read

Predicates for reading/writing RDF lists.

An RDF list is taken to be a resource that occurs in the subject position
on the =|rdf:first|= and of the =|rdf:rest|= predicates.

# rdfs:subClassOf

~~~{.pl}
rdfs_subclass(X, Y, G):-
  rdfs_subclass0(X, Y, G),
  % We add the restriction that both resources are classes.
  once(rdfs_individual(X, rdfs:'Class', G)),
  once(rdfs_individual(Y, rdfs:'Class', G)).
rdfs_subclass0(X, X, _G).
rdfs_subclass0(X, Y, G):-
  rdf(X, rdfs:subClassOf, Z, _SameOrOtherG),
  rdfs_subclass0(Z, Y, G).
~~~

# rdfs:subPropertyOf

~~~{.pl}
rdfs_subproperty(X, Y, G):-
  rdfs_subproperty0(X, Y, G),
  % We add the restriction that both resources are properties.
  rdfs_individual(X, rdf:'Property', G),
  rdfs_individual(Y, rdf:'Property', G).
rdfs_subproperty0(X, X, _G).
rdfs_subproperty0(X, Y, G):-
  rdf(X, rdfs:subPropertyOf, Z, G),
  rdfs_subproperty0(Z, Y, G).
~~~

# rdf:type

~~~{.pl}
% We make the memberhip of RDFS class to itself explicit because otherwise
% the class checks in method rdfs_subclass_of/2 cause rdfs_individual/3 to go
% into a loop.
rdfs_individual(rdfs:'Class', rdfs:'Class', _G).
rdfs_individual(X, Y, G):-
  nonvar(X), !,
  rdf(X, rdf:type, Z, G),
  rdfs_subclass_of(Z, Y),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
rdfs_individual(X, Y, G):-
  rdfs_subclass_of(Z, Y),
  rdf(X, rdf:type, Z, G),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
~~~

@author Wouter Beek
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

% CLASS
:- rdf_meta(rdfs_class(+,r)).
:- rdf_meta(rdfs_is_individual(r,r,+)).
:- rdf_meta(rdfs_is_subclass(r,r,+)).
% DOMAIN & RANGE
:- rdf_meta(rdfs_domain(r,r,?)).
:- rdf_meta(rdfs_range(r,r,?)).
% RDF-HAS
:- rdf_meta(rdfs(r,r,r,?)).



% CLASS %

rdfs_class(G, C):-
  rdf_term(G, C),
  rdfs_individual_of(C, rdfs:'Class').

rdfs_is_individual(I, C, G):-
  rdf2(I, rdf:type, C0, G),
  rdfs_is_subclass(C0, C, G).

rdfs_is_subclass(C, C, _G):- !.
rdfs_is_subclass(C1, C2, G):-
  rdf2(C1, rdfs:subClassOf, C3, G),
  C1 \== C3,
  rdfs_is_subclass(C3, C2, G).



% DOMAIN & RANGE

rdfs_domain(Property1, Domain, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:domain, Domain, Graph).

rdfs_range(Property1, Range, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:range, Range, Graph).



% RDF-HAS %

rdfs(Subject, Predicate, Object, Graph):-
  rdf(Subject, Predicate0, Object, Graph),
  rdfs_subproperty_of(Predicate0, Predicate).

