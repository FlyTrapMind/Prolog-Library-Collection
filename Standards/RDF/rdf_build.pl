:- module(
  rdf_build,
  [
% INDIVIDUALS
    rdf_assert_individual/3, % +Individual:uri
                             % +Class:uri
                             % +Graph:atom

% LITERAL ASSERTIONS
    rdf_assert_datatype/5, % +Subject:oneof([bnode,uri])
                           % +Predicate:uri
                           % +Datatype:atom
                           % +Value
                           % +Graph:atom
    rdf_assert_literal/4, % +Subject:oneof([bnode,uri])
                          % +Predicate:uri
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_literal/5, % +Subject:oneof([bnode,uri])
                          % +Predicate:uri
                          % +LanguageTag:atom
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_xml_literal/4, % +Subject:oneof([bnode,uri])
                              % +Predicate:uri
                              % +XMLLiteral:xml
                              % +Graph:atom

% LITERAL UPDATES
    rdf_increment/3, % +Link:uri
                     % +Relation:uri
                     % +Graph:atom
    rdf_overwrite_datatype/5, % +Subject:oneof([bnode,uri])
                              % +Predicate:uri
                              % +DatatypeName:atom
                              % +NewValue
                              % +Graph:atom

% LITERAL RETRACTIONS
    rdf_retractall_datatype/4, % ?Subject:oneof([bnode,uri])
                               % ?Predicate:uri
                               % ?DatatypeName:atom
                               % ?Graph:atom
    rdf_retractall_literal/4, % ?Subject:oneof([bnode,uri])
                              % ?Predicate:uri
                              % ?Literal:atom
                              % ?Graph:atom
    rdf_retractall_literal/5, % ?Subject:oneof([bnode,uri])
                              % ?Predicate:uri
                              % ?Language:atom
                              % ?Literal:atom
                              % ?Graph:atom

% PROPERTIES
    rdf_assert_property/2, % +Property:uri
                           % +Graph:atom
    rdf_remove_property/2 % +Graph:atom
                          % +Property:iri
  ]
).

/** <module> RDF build

Simple asserion and retraction predicates for RDF, customized for specific
datatypes and literals.

The supported datatypes:
    * boolean
    * ateTime
    * double
    * float
    * gDay
    * gMonth
    * gYear
    * image
    * integer

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05, 2013/07-2013/08
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% INDIVIDUALS %
:- rdf_meta(rdf_assert_individual(r,r,+)).
% LITERAL ASSERTIONS
:- rdf_meta(rdf_assert_datatype(r,r,+,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+,+)).
:- rdf_meta(rdf_assert_xml_literal(r,r,+,+)).
% LITERAL UPDATES
:- rdf_meta(rdf_increment(r,r,+)).
:- rdf_meta(rdf_overwrite_datatype(r,r,+,+,+)).
% LITERAL RETRACTIONS
:- rdf_meta(rdf_retractall_datatype(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?,?)).
% PROPERTIES %
:- rdf_meta(rdf_assert_property(r,+)).
:- rdf_meta(rdf_remove_property(+,r)).

:- debug(rdf_build).



% INDIVIDUALS %

%! rdf_assert_individual(+Individual:uri, +Class:uri, +Graph:graph) is det.
% Asserts an individual/class relationship.
%
% @param Individual An instance resource.
% @param Class A class resource.
% @param Graph The atomic name of an RDF graph.

rdf_assert_individual(I, C, G):-
  rdf_assert(I, rdf:type, C, G).



% LITERAL ASSERTIONS %

%! rdf_assert_datatype(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +DatatypeName:atom,
%!   +Value,
%!   +Graph:atom
%! ) is det.
% Asserts a datatyped value for a blank node or IRI reference.
%
% We choose to use the XML Schema 2 Datatypes (2nd Edition)
% for this. The asserted values are the atomic equivalent of the
% *|canonical lexical representations|* as defined by that standard.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_assert_datatype(S, P, DatatypeName, Value, G):-
  xsd_datatype(DatatypeName, Datatype),
  % We only emit canonical representations for XSD values.
  xsd_canonicalMap(Datatype, Value, LEX1),
  atom_codes(LEX2, LEX1),
  rdf_assert(S, P, literal(type(Datatype,LEX2)), G).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a literal value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_assert_literal/5 also specifies the language.

rdf_assert_literal(S, P, Lit, G):-
  % Make sure that the literal is atomic.
  rdf_assert(S, P, literal(Lit), G).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +LanguageTag:atom,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a language-tagged literal value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param LanguageTag An atomic language tag.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_assert_literal(S, P, LangTag, Lit, G):-
  nonvar(LangTag), !,
  rdf_assert(S, P, literal(lang(LangTag, Lit)), G).
rdf_assert_literal(S, P, _LangTag, Lit, G):-
  rdf_assert_literal(S, P, Lit, G).

%! rdf_assert_xml_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +XMLLiteral:atom,
%!   +Graph:atom
%! ) is det.

rdf_assert_xml_literal(S, P, XMLLiteral, G):-
  rdf_assert_datatype(S, P, 'XMLLiteral', XMLLiteral, G).



% LITERAL RETRACTIONS %

%! rdf_retractall_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?DatatypeName:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a datatypes value.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Graph The atomic name of an RDF graph.

rdf_retractall_datatype(S, P, DatatypeName, G):-
  xsd_datatype(DatatypeName, Datatype),
  rdf_retractall(S, P, literal(type(Datatype,_LEX)), G).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_retractall_literal/5 only retracts triples with literals of
%      a specific name.

rdf_retractall_literal(S, P, Literal, G):-
  rdf_retractall(S, P, literal(Literal), G).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Language:atom,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property
% in a specific language.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_retractall_literal(S, P, Language, Literal, G):-
  rdf_retractall(S, P, literal(lang(Language, Literal)), G).



% LITERAL UPDATES %

%! rdf_increment(+Link:uri, +Relation:uri, +Graph:atom) is det.
% Inrements an integer stored in RDF.

rdf_increment(S, P, G):-
  once(rdf_datatype(S, P, integer, OldValue, G)),
  NewValue is OldValue + 1,
  rdf_retractall_datatype(Link, Relation, integer, G),
  rdf_assert_datatype(Link, Relation, integer, NewValue, G).

%! rdf_overwrite_datatype(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +DatatypeName:atom,
%!   +NewValue,
%!   +Graph:atom
%! ) is det.
% The single new value is going to overwrite all old values, unless the new
% value is already asserted. In that case none of the other values gets
% retracted.

rdf_overwrite_datatype(S, P, DatatypeName, Value, G):-
  \+ rdf_datatype(S, P, DatatypeName, _, G), !,
  rdf_assert_datatype(S, P, DatatypeName, Value, G).
rdf_overwrite_datatype(S, P, DatatypeName, NewValue, G):-
  rdf_retractall_datatype(S, P, DatatypeName, G),
  rdf_assert_datatype(S, P, DatatypeName, NewValue, G),
  debug(
    rdf_build,
    'Updated value <~w, ~w, ~w^^~w, ~w>',
    [S,P,NewValue,DatatypeName,G]
  ).



% PROPERTIES %

rdf_assert_property(Property, G):-
  rdf_assert_individual(Property, rdf:'Property', G).

rdf_remove_property(G, P):-
  rdf_property(G, P), !,
  rdf_retractall(P, _, _, G),
  rdf_retractall(_, P, _, G),
  rdf_retractall(_, _, P, G).

