:- module(
  rdf_typed_literal,
  [
    rdf_is_typed_literal/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_typed_literal/1, % ?TypedLiteral:compound
    rdf_typed_literal/2, % ?RdfGraph:atom
                         % ?TypedLiteral:compound
    rdf_typed_literal_datatype_iri/2, % ?TypedLiteral:compound
                                      % ?Datatype:iri
    rdf_typed_literal_lexical_form/2 % ?TypedLiteral:compound
                                     % ?LexicalForm:atom
  ]
).

/** <module> RDF typed literal

Support for RDF 1.0 typed literals.
Typed literals are obsolete in RDF 1.1.

A typed literal used to be defined as the cartesian product of
the Unicode strings in Normal Form C with the set of datatype URIs.

@author Wouter Beek
@version 2013/09-2013/11, 2014/01, 2014/03
*/

:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_is_typed_literal(o)).
:- rdf_meta(rdf_typed_literal(o)).
:- rdf_meta(rdf_typed_literal(?,o)).
:- rdf_meta(rdf_typed_literal_datatype_iri(o,r)).
:- rdf_meta(rdf_typed_literal_lexical_form(o,?)).



%! rdf_is_typed_literal(+RdfTerm:or([bnode,iri,literal])) is semidet.

rdf_is_typed_literal(literal(type(IRI,LexicalForm))):-
  is_of_type(iri, IRI),
  atom(LexicalForm).


%! rdf_typed_literal(+TypedLiteral:compound) is semidet.
%! rdf_typed_literal(-TypedLiteral:compound) is nondet.
% Typed literal compound terms, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal(TypedLiteral):-
  % Enumerate all literals.
  rdf_current_literal(TypedLiteral),
  % Make sure the literal is typed.
  rdf_is_typed_literal(TypedLiteral).


%! rdf_typed_literal(+RdfGraph:atom, +TypedLiteral:compound) is semidet.
%! rdf_typed_literal(+RdfGraph:atom, -TypedLiteral:compound) is nondet.
%! rdf_typed_literal(-RdfGraph:atom, +TypedLiteral:compound) is nondet.
%! rdf_typed_literal(-RdfGraph:atom, -TypedLiteral:compound) is nondet.
% Pairs of RDF graphs to typed literals.
% Enumeration is assured to not deliver any duplicates.

rdf_typed_literal(RdfGraph, TypedLiteral):-
  rdf_typed_literal(TypedLiteral),
  rdf_object(RdfGraph, TypedLiteral).


%! rdf_typed_literal_datatype_iri(+TypedLiteral:compound, +DatatypeIri:iri) is semidet.
%! rdf_typed_literal_datatype_iri(+TypedLiteral:compound, -DatatypeIri:iri) is det.
%! rdf_typed_literal_datatype_iri(-TypedLiteral:compound, +DatatypeIri:iri) is det.
%! rdf_typed_literal_datatype_iri(-TypedLiteral:compound, -DatatypeIri:iri) is nondet.

rdf_typed_literal_datatype_iri(TypedLiteral, DatatypeIri):-
  % Enumerate all typed literals.
  rdf_typed_literal(TypedLiteral),
  TypedLiteral = literal(type(DatatypeIri,_)).


%! rdf_typed_literal_lexical_form(+TypedLiteral:compound, +LexicalForm:iri) is semidet.
%! rdf_typed_literal_lexical_form(+TypedLiteral:compound, -LexicalForm:iri) is det.
%! rdf_typed_literal_lexical_form(-TypedLiteral:compound, +LexicalForm:iri) is det.
%! rdf_typed_literal_lexical_form(-TypedLiteral:compound, -LexicalForm:iri) is nondet.

rdf_typed_literal_lexical_form(TypedLiteral, LexicalForm):-
  % Enumerate all typed literals.
  rdf_typed_literal(TypedLiteral),
  TypedLiteral = literal(type(_,LexicalForm)).

