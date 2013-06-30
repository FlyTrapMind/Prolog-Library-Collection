:- module(
  xml_to_rdf,
  [
    attribute_value_pairs/4 % +Namespace:atom
                            % +Graph:atom
                            % +Subject:oneof([bnode,uri])
                            % +DOM
  ]
).

/** <module> XML_TO_RDF

Converts XML DOMs to RDF graphs.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).

:- rdf_meta(attribute_value_pairs(+,+,r,+)).



%! attribute_value_pairs(
%!   +Namespace:atom,
%!   +Graph:atom,
%!   +Subject:oneof([bnode,uri]),
%!   +DOM
%! ) is det.
% Assumes that the DOM consist of a flat enumeration of attribute/value-pairs
% that apply to the given subject term.
%
% The namespace is assumed to be registered as an XML namespace
% prior to calling this predicate.

attribute_value_pairs(Namespace, Graph, Subject, Elements):-
  maplist(attribute_value_pair(Namespace, Graph, Subject), Elements).

attribute_value_pair(
  _Namespace,
  _Graph,
  _Subject,
  element(_Element, _Attributes, [])
):- !.
attribute_value_pair(
  Namespace,
  Graph,
  Subject,
  element(Element, _Attributes, [Content])
):-
  rdf_global_id(Namespace:Element, Predicate),
  rdf_assert_literal(Subject, Predicate, Content, Graph).

