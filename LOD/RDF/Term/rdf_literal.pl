:- module(
  rdf_literal,
  [
    rdf_assert_literal/5, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +Graph:atom
    rdf_assert_literal/6, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +LanguageTag:atom
                          % +Graph:atom
    rdf_literal/1, % ?Literal:compound
    rdf_literal/2, % ?RdfGraph:atom
                   % ?Literal:compound
    rdf_literal/4, % ?Literal:compound
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
    rdf_literal/6, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
                   % ?Graph:atom
    rdf_literal_equality/2, % +Literal1:compound
                            % +Literal2:compound
    rdf_retractall_literal/5, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?LexicalForm:atom
                              % ?DatatypeIri:iri
                              % ?Graph:atom
    rdf_retractall_literal/6 % ?Subject:oneof([bnode,iri])
                             % ?Predicate:iri
                             % ?LexicalForm:atom
                             % ?DatatypeIri:iri
                             % ?LanguageTag:atom
                             % ?Graph:atom
  ]
).

/** <module> RDF literal

Support for asserting/retracting/reading triples
that contain literal object terms.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_term)).
:- use_module(xsd(xsd_clean)).

:- rdf_meta(rdf_assert_literal(r,r,+,r,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,r,+,+)).
:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_literal(?,o)).
:- rdf_meta(rdf_literal(o,?,r,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?,?)).



%! rdf_assert_literal(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   +Graph:atom
%! ) is det.
% Asserts a triple with a literal object term.
%
% If the datatype IRI is omitted, the XSD string datatype is used.
%
% This cannot be used to assert triples with a literal object term
% that is of type =|rdf:langString|=

rdf_assert_literal(Subject, Predicate, LexicalForm, DatatypeIri, Graph):-
  rdf_assert_literal(Subject, Predicate, LexicalForm, DatatypeIri, _, Graph).


%! rdf_assert_literal(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   +LanguageTag:atom,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a triple with a literal object term.

% Language-tagged strings.
rdf_assert_literal(Subject, Predicate, LexicalForm, DatatypeIri, LanguageTag, Graph):- !,
  nonvar(LanguageTag), !,
  % The datatype IRI is =|rdf:langString|= iff the language tag is set.
  rdf_equal(rdf:langString, DatatypeIri),
  rdf_assert(Subject, Predicate, literal(lang(LanguageTag,LexicalForm)), Graph).
% Simple literals.
rdf_assert_literal(Subject, Predicate, LexicalForm, DatatypeIri, _, Graph):-
  var(DatatypeIri), !,
  rdf_assert_literal(Subject, Predicate, LexicalForm, xsd:string, _, Graph).
% Others.
rdf_assert_literal(Subject, Predicate, LexicalForm, DatatypeIri, _, Graph):-
  rdf_assert(Subject, Predicate, literal(type(DatatypeIri,LexicalForm)), Graph).


%! rdf_literal(+Literal) is semidet.
%! rdf_literal(-Literal) is nondet.

rdf_literal(Literal):-
  % Enumerates all literals.
  rdf_current_literal(Literal).


%! rdf_literal(+RdfGraph, +Literal) is semidet.
%! rdf_literal(-RdfGraph, +Literal) is nondet.
%! rdf_literal(+RdfGraph, -Literal) is nondet.
%! rdf_literal(-RdfGraph, -Literal) is nondet.

rdf_literal(RdfGraph, Literal):-
  % Enumerated all literals.
  rdf_literal(Literal),
  % Relates to an RDF graph.
  rdf_object(RdfGraph, Literal).


%! rdf_literal(+Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is semidet.
%! rdf_literal(+Literal:compound, -LexicalForm:atom, -DatatypeIri:iri, ?LanguageTag:atom) is det.
%! rdf_literal(-Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is det.
% Construct/disassemble an RDF literal compound term in the Semweb format.

rdf_literal(literal(lang(LanguageTag,LexicalForm)), LexicalForm, rdf:langString, LanguageTag).
rdf_literal(literal(type(DatatypeIri,LexicalForm)), LexicalForm, DatatypeIri, _).
rdf_literal(literal(LexicalForm), LexicalForm, xsd:string, _):-
  atom(LexicalForm).


%! rdf_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?RdfGraph:graph
%! ) is nondet.

% Language-tagged strings.
rdf_literal(Subject, Predicate, LexicalForm, DatatypeIri, LanguageTag, Graph):-
  rdf_equal(rdf:langString, DatatypeIri),
  rdf(Subject, Predicate, literal(lang(LanguageTag,LexicalForm)), Graph).
% Typed literals.
rdf_literal(Subject, Predicate, LexicalForm, DatatypeIri, LanguageTag, Graph):-
  var(LanguageTag),
  rdf(Subject, Predicate, literal(type(DatatypeIri,LexicalForm)), Graph).
% Simple literals.
rdf_literal(Subject, Predicate, LexicalForm, DatatypeIri, LanguageTag, Graph):-
  var(LanguageTag),
  rdf_equal(xsd:string, DatatypeIri),
  rdf(Subject, Predicate, literal(LexicalForm), Graph),
  atom(LexicalForm).


%! rdf_literal_equality(+Literal1:literal, +Literal2:literal) is semidet.
% Succeeds if the given literals are equivalent.
%
% Two literals are equivalent if:
%   1. The strings of the two lexical forms compare equal,
%      character by character.
%   2. Either both or neither have language tags.
%   3. The language tags, if any, compare equal.
%   4. Either both or neither have datatype URIs.
%   5. The two datatype URIs, if any, compare equal, character by character.
%
% @see Resource Description Framework (RDF): Concepts and Abstract Syntax
%      http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/

% Plain literals with the same language tag and value string.
rdf_literal_equality(
  literal(lang(LangTag1,LexicalForm1)),
  literal(lang(LangTag2,LexicalForm2))
):- !,
  LangTag1 == LangTag2,
  LexicalForm1 == LexicalForm2.
% Typed literals with equivalent values in the datatype's value space.
rdf_literal_equality(
  literal(type(DatatypeIri1,LexicalForm1)),
  literal(type(DatatypeIri2,LexicalForm2))
):- !,
  DatatypeIri1 == DatatypeIri2,
  xsd_lexical_canonical_map(DatatypeIri1, LexicalForm1, CanonicalLexicalForm1),
  xsd_lexical_canonical_map(DatatypeIri2, LexicalForm2, CanonicalLexicalForm2),
  CanonicalLexicalForm1 == CanonicalLexicalForm2.
% Simple literals that are the same.
rdf_literal_equality(literal(LexicalForm1), literal(LexicalForm2)):-
  atom(LexicalForm1),
  atom(LexicalForm2),
  LexicalForm1 == LexicalForm2.


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Does not retract language-tagged strings for specific language tags.

rdf_retractall_literal(Subject, Predicate, LexicalForm, DatatypeIri, Graph):-
  rdf_retractall_literal(Subject, Predicate, LexicalForm, DatatypeIri, _, Graph).


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
% asserted with datatype IRI =|xsd:string|=.
% We do not retract literal compound terms of the form
% =|literal(LexicalForm:atom)|=.

% Retract language-tagged strings.
rdf_retractall_literal(Subject, Predicate, LexicalForm, DatatypeIri, LanguageTag, Graph):-
  (rdf_equal(rdf:langString, DatatypeIri) ; nonvar(LanguageTag)), !,
  rdf_retractall(Subject, Predicate, literal(lang(LanguageTag,LexicalForm)), Graph).
% Retract others.
rdf_retractall_literal(Subject, Predicate, LexicalForm, DatatypeIri, _, Graph):-
  rdf_retractall(Subject, Predicate, literal(type(DatatypeIri,LexicalForm)), Graph).

