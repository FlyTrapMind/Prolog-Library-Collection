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
    rdf_has_datatype/4, % ?Subject:oneof([bnode,iri])
                        % ?Predicate:iri
                        % ?Datatype:iri
                        % ?Value
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
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_assert_datatype(r,r,r,+,+)).
:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype(r,r,r,?,?)).
:- rdf_meta(rdf_has_datatype(r,r,r,?)).
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
% @arg Subject An RDF subject term.
% @arg Predicate An RDF predicate term.
% @arg Datatype An IRI identifying a datatype.
% @arg Value
% @arg Graph The atomic name of an RDF graph.

rdf_assert_datatype(S, P, D, Val, G):-
  % We only emit canonical representations for XSD values.
  xsd_canonicalMap(D, Val, LEX1),
  atom_codes(LEX2, LEX1),
  rdf_assert(S, P, literal(type(D,LEX2)), G).

%! rdf_datatype(?Graph:atom, ?Datatype:iri) is nondet.

rdf_datatype(G, D):-
  rdf(_S, _P, literal(type(D, _LEX)), G).

%! rdf_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:or([atom,iri]),
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.
% @tbd Implement the inverse lexical map to fascilitate search (besides read and write).

rdf_datatype(S, P, D, Value, G):-
  once(xsd_datatype(_, D)),
  % Ideally, we would like to interpret all literals, not just the canonical ones.
  % Unfortunately the instantiation pattern for xsd_lexicalMap/3 does not allow this.
  % Interpreting literals could be useful for search, i.e. does a specific value
  % from the value space of the given datatype occur in the currently loaded RDF graph?
  % For this one needs the inverse of the lexical map.
  % In the absence of this inverse lexical map, we have to look for a lexical map
  % of a datatype literal that matches value (this is not so bad as it seems,
  % if subject, predicate, datatype, and graph are specified).
  rdf(S, P, literal(type(D, LEX)), G),
  % This may be nondet!
  xsd_lexicalMap(D, LEX, Value).
% Provide minimal support for non-IRI datatype names.
rdf_datatype(S, P, D1, Value, G):-
  once(xsd_datatype(D1, D2)),
  rdf_datatype(S, P, D2, Value, G).

%! rdf_has_datatype(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.

rdf_has_datatype(S, P, D, Value):-
  (
    nonvar(Value)
  ->
    % Interpret all literals, not just the canonical ones.
    xsd_lexicalMap(D, LEX, Value),
    rdf_has(S, P, literal(type(D, LEX)))
  ;
    rdf_has(S, P, literal(type(D, LEX))),
    % This may be nondet!
    xsd_lexicalMap(D, LEX, Value)
  ).

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
% @arg Subject An RDF subject term.
% @arg Predicate An RDF predicate term.
% @arg Datatype An IRI identifying a datatype.
% @arg Graph The atomic name of an RDF graph.

rdf_retractall_datatype(S, P, D, G):-
  rdf_retractall(S, P, literal(type(D,_LEX)), G).

