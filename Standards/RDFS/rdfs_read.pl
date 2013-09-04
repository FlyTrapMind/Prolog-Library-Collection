:- module(
  rdfs_read,
  [
% CLASS
    rdfs_class/2, % +Graph:atom
                  % ?Class:iri

% DOMAIN & RANGE
    rdfs_domain/3, % ?Property:uri
                   % ?Domain:uri
                   % ?Graph:atom
    rdfs_range/3, % ?Property:uri
                  % ?Range:uri
                  % ?Graph:atom

% LABELS
    rdfs_preferred_label/4, % ?RDF_Term:oneof([bnode,uri])
                            % +LanguageTag:atom
                            % -PreferredLanguageTag:atom
                            % ?PreferredLiteral:atom
    rdfs_list_label/3, % +List:uri
                       % +Label:atom
                       % -Element:uri

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
%! rdfs_individual(?Individual:uri, ?Class:uri, ?Graph:atom) is nondet.
% Individual and class pairs.
%
% @param Individual An instance resource.
% @param Class A class resource.
% @param Graph The atomic name of a graph.

% We make the memberhip of RDFS class to itself explicit because otherwise
% the class checks in method rdfs_subclass_of/2 cause rdfs_individual/3 to go
% into a loop.

rdfs_individual(rdfs:'Class', rdfs:'Class', _G).
rdfs_individual(X, Y, G):-
  nonvar(X),
  !,
  rdf(X, rdf:type, Z, G),
  rdfs_subclass_of(Z, Y),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
rdfs_individual(X, Y, G):-
  rdfs_subclass_of(Z, Y),
  rdf(X, rdf:type, Z, G),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
~~~

@author Wouter Beek
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

% CLASS
:- rdf_meta(rdfs_class(+,r)).
% DOMAIN & RANGE
:- rdf_meta(rdfs_domain(r,r,?)).
:- rdf_meta(rdfs_range(r,r,?)).
% LABELS
:- rdf_meta(rdfs_preferred_label(r,+,-,?)).
:- rdf_meta(rdfs_list_label(r,+,-)).
% RDF-HAS
:- rdf_meta(rdfs(r,r,r,?)).



% CLASS %

rdfs_class(G, C):-
  rdf_term(G, C),
  rdfs_individual_of(C, rdfs:'Class').



% DOMAIN & RANGE

rdfs_domain(Property1, Domain, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:domain, Domain, Graph).

rdfs_range(Property1, Range, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:range, Range, Graph).



% LITERALS %

%! rdfs_preferred_label(
%!   ?RDF_Term:or([bnode,iri]),
%!   +LanguageTag:atom,
%!   -PreferredLangTag:atom,
%!   ?Label:atom
%! ) is nondet.
% Multiple labels are returned (nondet) in a descending preference order.

rdfs_preferred_label(RDF_Term, LangTags, PreferredLangTag, PreferredLabel):-
  rdfs_label(RDF_Term, Label1),
  rdf_preferred_literal(
    RDF_Term,
    rdfs:label,
    LangTags,
    PreferredLangTag,
    PreferredLabel
  ),
  Label1 == PreferredLabel, !.

%! rdfs_list_label(+RDF_List:uri, +Label:atom, -Element:uri) is nondet.
% Returns RDF list elements that have the given label.

rdfs_list_label(RDF_List, Label, Element):-
  rdf_list_first(RDF_List, First),
  rdfs_list_label0(First, Label, Element).

rdfs_list_label0(Element, Label, Element):-
  rdfs_label(Element, Label), !.
rdfs_list_label0(Element, Label, Element0):-
  rdf_list_next(Element, NextElement),
  rdfs_list_label0(NextElement, Label, Element0).



% RDF-HAS %

rdfs(Subject, Predicate, Object, Graph):-
  rdf(Subject, Predicate0, Object, Graph),
  rdfs_subproperty_of(Predicate0, Predicate).

