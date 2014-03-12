:- module(
  rdf_datatype,
  [
    rdf_assert_datatype/5, % +Subject:oneof([bnode,iri])
                           % +Predicate:iri
                           % +Datatype:iri
                           % +Value
                           % +Graph:atom
    rdf_datatype/2, % ?Graph:atom
                    % ?Datatype:iri
    rdf_datatype/5, % ?Subject:oneof([bnode,iri])
                    % ?Predicate:iri
                    % ?Datatype:iri
                    % ?Value
                    % ?Graph:atom
    rdf_lexical_map/3, % +Datatype:iri
                       % +Lexical:or([atom,list(code)]
                       % -Value
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
@version 2013/10, 2014/01-2014/03
*/

:- rdf_meta(rdf_assert_datatype(r,r,r,+,+)).
:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype(r,r,r,?,?)).
:- rdf_meta(rdf_lexical_map(r,+,-)).
:- rdf_meta(rdf_overwrite_datatype(r,r,r,+,+)).
:- rdf_meta(rdf_retractall_datatype(r,r,r,?)).

:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_name)). % Meta-DCG.
:- use_module(xsd(xsd)).



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
% We only emit canonical representations for XSD values.
%
% @arg Subject An RDF subject term.
% @arg Predicate An RDF predicate term.
% @arg Datatype An IRI identifying a datatype.
% @arg Value
% @arg Graph The atomic name of an RDF graph.

rdf_assert_datatype(S, P, D, Value, G):-
  xsd_canonical_map(D, Value, LexicalCodes),
  atom_codes(LexicalAtom, LexicalCodes),
  rdf_assert(S, P, literal(type(D,LexicalAtom)), G).


%! rdf_datatype(?Graph:atom, ?Datatype:iri) is nondet.

rdf_datatype(G, D):-
  rdf(_S, _P, literal(type(D, _)), G).


%! rdf_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:or([atom,iri]),
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.
% @tbd Ideally, we would like to close lexical expressions
%      under identity and equivalence in search.

rdf_datatype(S, P, D, Value, G):-
  nonvar(D),
  ground(Value), !,
  % @tbd Ideally, we would like to close `LexicalCodes`
  %      under identity or equivalence.
  xsd_canonical_map(D, Value, LexicalCodes),
  atom_codes(LexicalAtom, LexicalCodes),
  rdf(S, P, literal(type(D,LexicalAtom)), G).
rdf_datatype(S, P, D, Value, G):-
  rdf(S, P, literal(type(D,Lit)), G),
  rdf_lexical_map(D, Lit, Value).


%! rdf_lexical_map(+Datatype:iri, +Literal:atom, -Value) is det.
% Converts atomic typed literals to their corresponsing value,
%  according to the given datatype.

rdf_lexical_map(D, Lexical, Value):-
  xsd_lexical_map(D, Lexical, Value).


%! rdf_overwrite_datatype(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
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
  dcg_with_output_to(atom(T1), rdf_triple_name(S, P, OldO, G)),
  rdf_typed_literal(NewO, D, NewVal),
  dcg_with_output_to(atom(T2), rdf_triple_name(S, P, NewO, G)),
  debug(rdf_datatype, 'Updated triple: ~w --> ~w', [T1,T2]).

%! rdf_retractall_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a datatypes value.
%
% @arg Subject An RDF subject term.
% @arg Predicate An RDF predicate term.
% @arg Datatype An IRI identifying a datatype.
% @arg Graph The atomic name of an RDF graph.

rdf_retractall_datatype(S, P, D, G):-
  rdf_retractall(S, P, literal(type(D,_LEX)), G).

