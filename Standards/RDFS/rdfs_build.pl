:- module(
  rdfs_build,
  [
% CLASS HIERARCHY
    rdfs_assert_class/2, % +Class:uri
                         % +Graph:graph
    rdfs_assert_individual/2, % +Individual:uri
                              % +Graph:graph
    rdfs_assert_property_class/2, % +PropertyClass:uri
                                  % +Graph:graph
    rdfs_assert_subclass/3, % +Class:uri
                            % +SuperClass:uri
                            % +Graph:graph

% DOMAIN & RANGE
    rdfs_assert_domain/3, % +Property:uri
                          % +Class:uri
                          % +Graph:atom
    rdfs_assert_domain_range/3, % +Property:uri
                                % +Class:uri
                                % +Graph:atom
    rdfs_assert_range/3, % +Property:uri
                         % +Class:uri
                         % +Graph:atom

% LABELS
    rdfs_assert_label/3, % +Subject:oneof([bnode,uri])
                         % +Label:atom
                         % +Graph:graph
    rdfs_assert_label/4, % +Subject:oneof([bnode,uri])
                         % +Language:atom
                         % +Label:atom
                         % +Graph:graph
    rdfs_retractall_label/3, % +Subject:oneof([bnode,uri])
                             % +Label:atom
                             % +Graph:graph
    rdfs_retractall_label/4, % +Subject:oneof([bnode,uri])
                             % +Language:atom
                             % +Label:atom
                             % +Graph:graph

% PROPERTY HIERARCHY
    rdfs_assert_subproperty/3 % +Property:uri
                              % +SuperProperty:uri
                              % +Graph:graph
  ]
).

/** <module> RDFS_BUILD

Predicates for asseritng RDFS statements in an easy way.

Higher-level predicates for building RDF graphs.
These predicates use the lower-level predicates from RDF_WRITE.

A class C is asserted by (1) a triple of the form <C, rdf:type, C'>, such that
rdfs_subClassOf(C', rdfs:Class), and by (2) a triple of the form
<C, rdfs:subClassOf, C'> such that rdfs_subClassOf(C', rdfs:Resource).

For example =|rdf:XMLLiteral|=:

~~~
< rdf:XMLLiteral, rdf:type,        rdfs:Datatype >
< rdf:XMLLiteral, rdfs:subClassOf, rdfs:Literal  >
~~~

A property P is asserted by a triple of the form <P, rdf:type, P'>, such that
rdfs_subPropertyOf(P', rdf:Property).

A property hierarchy with parent _P0_ and children _P1_, ..., _Pn_ is asserted
using the following triples:
    * =|< P0,      rdf:type,           P       >|=
      where =P= is a subclass of =|rdf:Property|=.
    * =|< P0Class, rdfs:subClassOf,    P       >|=
      for the same =P=.
    * =|< P0Class, rdf:type,           C       >|=
      where =C= is a subclass of =|rdfs:Class|=.
    * =|< Pi,      rdf:type,           P0Class >|=
      for every _i_ between _1_ and _n_ (inclusive).
    * =|< Pi,      rdfs:subPropertyOf, P       >|=
      for every _i_ between _1_ and _n_ (inclusive).

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/02, 2013/05-2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

% CLASS HIERARCHY
:- rdf_meta(rdfs_assert_class(r,+)).
:- rdf_meta(rdfs_assert_individual(r,+)).
:- rdf_meta(rdfs_assert_property_class(r,+)).
:- rdf_meta(rdfs_assert_subclass(r,r,+)).
% DOMAIN & RANGE
:- rdf_meta(rdfs_assert_domain(r,r,+)).
:- rdf_meta(rdfs_assert_domain_range(r,r,+)).
:- rdf_meta(rdfs_assert_range(r,r,+)).
% LABELS
:- rdf_meta(rdfs_assert_label(r,+,+)).
:- rdf_meta(rdfs_assert_label(r,+,+,+)).
:- rdf_meta(rdfs_retractall_label(r,+,+)).
:- rdf_meta(rdfs_retractall_label(r,+,+,+)).
% PROPERTY HIERARCHY
:- rdf_meta(rdfs_assert_subproperty(r,r,+)).

:- initialization(load_rdfs_schema).



% CLASS HIERARCHY %

rdfs_assert_class(Class, G):-
  % Materialization would figure this one out as well.
  rdf_assert_individual(Class, rdfs:'Class',    G),
  rdfs_assert_subclass( Class, rdfs:'Resource', G).

rdfs_assert_individual(Individual, G):-
  rdf_assert_individual(Individual, rdfs:'Resource', G).

rdfs_assert_property_class(PropertyClass, G):-
  % Materialization would figure this one out as well.
  rdf_assert_individual(PropertyClass, rdfs:'Class',   G),
  rdfs_assert_subclass( PropertyClass, rdf:'Property', G).

rdfs_assert_subclass(Class, SuperClass, G):-
  rdf_assert(Class, rdfs:subClassOf, SuperClass, G).



% DOMAIN & RANGE %

%! rdfs_assert_domain(+Property:uri, +Class:uri, +Graph:atom) is det.

rdfs_assert_domain(P, C, G):-
  rdf_assert(P, rdfs:domain, C, G).

%! rdfs_assert_domain_range(+Property:uri, +Class:uri, +Graph:atom) is det.
% RDFS properties whose domain and range are the same RDFS class.

rdfs_assert_domain_range(P, C, G):-
  rdf_assert(P, rdfs:domain, C, G),
  rdf_assert(P, rdfs:range, C, G).

%! rdfs_assert_range(+Property:uri, Class:uri, Graph:atom) is det.

rdfs_assert_range(P, C, G):-
  rdf_assert(P, rdfs:range, C, G).



% LABELS %

%! rdfs_assert_label(
%!   +Subject:oneof([bnode,uri]),
%!   +Label:atom,
%!   +Grap:atom
%! ) is det.
% Assert the subject's label description.
%
% @param Subject A resource.
% @param Label An atomic description of a resource.
% @param Graph The atomic name of an RDF graph.
% @see rdfs_assert_label/4 also specifies the label.

rdfs_assert_label(Subject, Label, G):-
  % @ tbd Why is this necessary?
  rdf_global_id(rdfs:label, P),
  rdf_assert_literal(Subject, P, Label, G).

%! rdfs_assert_label(
%!   +Subject:oneof([bnode,uri]),
%!   +Language:atom,
%!   +Label:atom,
%!   +Grap:atom
%! ) is det.
% Assert the subject's label description in the given label.
%
% @param Subject A resource.
% @param Language The atomic name of a language.
% @param Label An atomic description of a resource.
% @param Graph The atomic name of an RDF graph.

rdfs_assert_label(Subject, Language, Label, G):-
  % @ tbd Why is this necessary?
  rdf_global_id(rdfs:label, P),
  rdf_assert_literal(Subject, P, Language, Label, G).

rdfs_retractall_label(Subject, Label, G):-
  rdf_global_id(rdfs:label, P),
  rdf_retractall_literal(Subject, P, Label, G).

rdfs_retractall_label(Subject, Language, Label, G):-
  rdf_global_id(rdfs:label, P),
  rdf_retractall_literal(Subject, P, Language, Label, G).



% PROPERTY HIERARCHY %

%! rdfs_assert_subproperty(
%!   +Property:property,
%!   +SuperProperty:property,
%!   +Graph:graph
%! ) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% @param Property An RDF property.
% @param SuperProperty An RDF property.
% @param Graph The atomic name of an RDF graph.

rdfs_assert_subproperty(Property, SuperProperty, G):-
  % Materialization
  %rdf_assert(Property, rdf:type, rdf:'Property', G),
  rdf_assert(Property, rdfs:subPropertyOf, SuperProperty, G).

