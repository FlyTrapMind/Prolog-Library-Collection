:- module(
  rdf_build,
  [
% INDIVIDUALS
    rdf_assert_individual/3, % +Individual:uri
                             % +Class:uri
                             % +Graph:atom

% LISTS
    rdf_assert_list/3, % +List:list
                       % -RDF_List:uri
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
                          % +Language:atom
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
    rdf_retractall_datatype/5, % ?Subject:oneof([bnode,uri])
                               % ?Predicate:uri
                               % ?DatatypeName:atom
                               % ?Value
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
    rdf_assert_property/2 % +Property:uri
                          % +Graph:atom
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
:- use_module(rdf(rdf_typecheck)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% INDIVIDUALS %
:- rdf_meta(rdf_assert_individual(r,r,+)).
% LISTS
:- rdf_meta(rdf_assert_list(+,r,+)).
% LITERAL ASSERTIONS
:- rdf_meta(rdf_assert_datatype(r,r,+,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+,+)).
:- rdf_meta(rdf_assert_xml_literal(r,r,+,+)).
% LITERAL UPDATES
:- rdf_meta(rdf_increment(r,r,+)).
:- rdf_meta(rdf_overwrite_datatype(r,r,+,+,+)).
% LITERAL RETRACTIONS
:- rdf_meta(rdf_retractall_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?,?)).
% PROPERTIES %
:- rdf_meta(rdf_assert_property(r,+)).

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



% LISTS %

%! rdf_assert_list(+List:list, -RDF_List:uri, +Graph:atom) is det.
% Asserts the given, possibly nested list into RDF.
%
% @param List The, possibly nested, Prolog list.
% @param RDF_List The URI of the node at which the RDF list starts.
% @param Graph The atomic name of a graph or unbound.
%
% @author Wouter Beek, elaborating on Sanders original, allowing the graph
%         to be optional and returning the root of the asserted list.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.

rdf_assert_list(List, RDF_List, G):-
  add_blank_list_individual(RDF_List, G),
  rdf_assert_list0(List, RDF_List, G).

rdf_assert_list0([], rdf:nil, _Graph).
rdf_assert_list0([H|T], RDF_List, G):-
  (
    is_list(H)
  ->
    rdf_assert_list0(H, H1, G)
  ;
    H1 = H
  ),
  rdf_assert(RDF_List, rdf:first, H1, G),
  (
    T == []
  ->
    rdf_global_id(rdf:nil, TList)
  ;
    add_blank_list_individual(TList, G),
    rdf_assert_list0(T, TList, G)
  ),
  rdf_assert(RDF_List, rdf:rest, TList, G).

add_blank_list_individual(Blank, G):-
  rdf_bnode(Blank),
  rdf_assert_individual(Blank, rdf:'List', G).



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
% The value conversion from external to canonical lexical is treated by
% module [xml_schema_datatypes.pl].
%
% At least the following datatypes are supported:
%   * `boolean`
%   * `date`
%   * `dateTime`
%   * `decimal`
%   * `double`
%   * `float`
%   * `gDay`
%   * `gMonth`
%   * `gYear`
%   * `int`
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_assert_datatype(S, P, DatatypeName, Value, G):-
  datatype(DatatypeName, Datatype),
  % We only emit canonical representations for XSD values.
  canonicalMap(Datatype, Value, LEX),
  rdf_assert(S, P, literal(type(Datatype, LEX)), G).

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

rdf_assert_literal(S, P, Literal, G):-
  % Make sure that the literal is atomic.
  rdf_assert(S, P, literal(Literal), G).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Language:atom,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a integer value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_assert_literal(S, P, Language, Literal, G):-
  % Make sure that the literal is atomic.
  rdf_assert(S, P, literal(lang(Language, Literal)), G).

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
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +DatatypeName:atom,
%!   +Value,
%!   +Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a datatypes value.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_retractall_datatype(S, P, DatatypeName, Value, G):-
  datatype(DatatypeName, Datatype),
  forall(
    % The given value may have several possible literals representing it.
    lexicalMap(Datatype, LEX, Value),
    rdf_retractall(S, P, literal(type(Datatype, LEX)), G)
  ).

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
  once(rdf_datatype(S, P, int, OldValue, G)),
  NewValue is OldValue + 1,
  rdf_retractall_datatype(Link, Relation, int, OldValue, G),
  rdf_assert_datatype(Link, Relation, int, NewValue, G).

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

rdf_overwrite_datatype(S, P, DatatypeName, NewValue, G):-
  % Type checking.
  % We need a completely qualified RDF triple.
  rdf_is_subject(S),
  rdf_is_predicate(P),
  rdf_graph(G), !,
  findall(
    OldValue,
    rdf_datatype(S, P, DatatypeName, OldValue, G),
    OldValues
  ),
  (
    member(NewValue, OldValues)
  ;
    rdf_retractall_datatype(S, P, DatatypeName, OldValue, G),
    rdf_assert_datatype(S, P, DatatypeName, NewValue, G),
    debug(
      rdf_build,
      'Updated value <~w, ~w, ~w^^~w, ~w> --> <~w, ~w, ~w^^~w, ~w>\n',
      [S, P, OldValues, DatatypeName, G, S, P, NewValue, DatatypeName, G]
    )
  ), !.



% PROPERTIES %

rdf_assert_property(Property, G):-
  rdf_assert_individual(Property, rdf:'Property', G).

