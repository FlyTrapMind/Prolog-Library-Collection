:- module(
  rdf_datatype,
  [
    rdf_assert_datatype/5, % +Subject:oneof([bnode,iri])
                           % +Predicate:iri
                           % +Datatype:iri
                           % +Value
                           % +Graph:atom
    rdf_overwrite_datatype/5, % +Subject:oneof([bnode,iri])
                              % +Predicate:iri
                              % +Datatype:iri
                              % +NewValue
                              % +Graph:atom
    rdf_retractall_datatype/4 % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?Datatype:iri
                              % ?Graph:atom
  ]
).

/** <module> RDF datatype

Support for RDF typed literals.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_assert_datatype(r,r,r,+,+)).
:- rdf_meta(rdf_overwrite_datatype(r,r,r,+,+)).
:- rdf_meta(rdf_retractall_datatype(r,r,r,?)).

:- debug(rdf_datatype).



%! rdf_assert_datatype(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   +Value,
%!   +Graph:atom
%! ) is det.
% Asserts a datatyped value for a blank node or IRI reference.
%
% We choose to use the XML Schema 2 Datatypes (2nd Edition)
% for this. The asserted values are the atomic equivalent of the
% *|canonical lexical representations|* as defined by that standard.
%
% @param Subject An RDF subject term.
% @param Predicate An RDF predicate term.
% @param Datatype An IRI identifying a datatype.
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_assert_datatype(S, P, D, Val, G):-
  % We only emit canonical representations for XSD values.
  xsd_canonicalMap(D, Val, LEX1),
  atom_codes(LEX2, LEX1),
  rdf_assert(S, P, literal(type(D,LEX2)), G).

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

rdf_overwrite_datatype(S, P, D, NewVal, G):-
  % Make sure there is exactly one value that would be overwritten.
  findall(
    [S,P,D,OldVal,G],
    rdf_datatype(S, P, D, OldVal, G),
    Tuples
  ),
  Tuples = [[S,P,D,OldVal,G]], !,
  
  % Remove the old value and assert the new value.
  rdf_retractall_datatype(S, P, D, G),
  rdf_assert_datatype(S, P, D, NewVal, G),
  
  % DEB
  rdf_typed_literal(OldO, D, OldVal),
  with_output_to(atom(T1), rdf_triple_name(S,P,OldO,G)),
  rdf_typed_literal(NewO, D, NewVal),
  with_output_to(atom(T2), rdf_triple_name(S,P,NewO,G)),
  debug(rdf_datatype, 'Updated triple: ~w --> ~w', [T1,T2]).

%! rdf_retractall_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a datatypes value.
%
% @param Subject An RDF subject term.
% @param Predicate An RDF predicate term.
% @param Datatype An IRI identifying a datatype.
% @param Graph The atomic name of an RDF graph.

rdf_retractall_datatype(S, P, D, G):-
  rdf_retractall(S, P, literal(type(D,_LEX)), G).
