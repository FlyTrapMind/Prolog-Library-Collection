:- module(
  rdf_literal,
  [
    rdf_assert_literal/5, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +RdfGraph:atom
    rdf_assert_literal/6, % +Subject:oneof([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % +DatatypeIri:iri
                          % +LanguageTag:atom
                          % +RdfGraph:atom
    rdf_literal/1, % ?Literal:compound
    rdf_literal/2, % ?Literal:compound
                   % ?RdfGraph:atom
    rdf_literal/4, % ?Literal:compound
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
    rdf_literal/6, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?DatatypeIri:iri
                   % ?LanguageTag:atom
                   % ?RdfGraph:atom
    rdf_literal_equality/2, % +Literal1:compound
                            % +Literal2:compound
    rdf_literal_map/4, % ?LexicalForm:atom
                       % ?DatatypeIri:iri
                       % ?LanguageTag:atom
                       % ?Value
    rdf_retractall_literal/5, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?LexicalForm:atom
                              % ?DatatypeIri:iri
                              % ?RdfGraph:atom
    rdf_retractall_literal/6, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?LexicalForm:atom
                              % ?DatatypeIri:iri
                              % ?LanguageTag:atom
                              % ?RdfGraph:atom
    rdf_update_literal/7 % ?Subject:oneof([bnode,iri])
                         % ?Predicate:iri
                         % ?FromLexicalForm:atom
                         % ?FromDatatypeIri:iri
                         % ?FromLanguageTag:atom
                         % ?RdfGraph:atom
                         % +Action:compound
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
:- use_module(xsd(xsd)).
:- use_module(xsd(xsd_clean)).

:- rdf_meta(rdf_assert_literal(r,r,+,r,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,r,+,+)).
:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_literal(o,?)).
:- rdf_meta(rdf_literal(o,?,r,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?,?)).
:- rdf_meta(rdf_literal_map(?,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,r,?,?)).
:- rdf_meta(rdf_update_literal(r,r,?,r,?,?,t)).



%! rdf_assert_literal(
%!   +Subject:oneof([bnode,iri]),
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   +RdfGraph:atom
%! ) is det.
% Asserts a triple with a literal object term.
%
% If the datatype IRI is omitted, the XSD string datatype is used.
%
% This cannot be used to assert triples with a literal object term
% that is of type =|rdf:langString|=

rdf_assert_literal(S, P, LexicalForm, Datatype, Graph):-
  rdf_assert_literal(S, P, LexicalForm, Datatype, _, Graph).


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
rdf_assert_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  nonvar(LangTag), !,
  % The datatype IRI is =|rdf:langString|= iff the language tag is set.
  rdf_equal(rdf:langString, Datatype),
  rdf_assert(S, P, literal(lang(LangTag,LexicalForm)), G).
% Simple literals.
rdf_assert_literal(S, P, LexicalForm, Datatype, _, G):-
  var(Datatype), !,
  rdf_assert_literal(S, P, LexicalForm, xsd:string, _, G).
% Others.
rdf_assert_literal(S, P, LexicalForm, Datatype, _, G):-
  rdf_assert(S, P, literal(type(Datatype,LexicalForm)), G).


%! rdf_convert_literal(
%!   +FromLexicalForm:atom,
%!   +FromDatatypeIri:iri,
%!   +FromLanguageTag:atom,
%!   -ToLexicalForm:atom,
%!   -ToDatatypeIri:iri,
%!   -ToLanguageTag:atom
%! ) .

rdf_convert_literal(
  FromLexicalForm, FromDatatype, FromLanguageTag,
  ToLexicalForm,   ToDatatype,   ToLanguageTag
):-
  rdf_literal_map(FromLexicalForm, FromDatatype, FromLanguageTag, Value),
  rdf_literal_map(ToLexicalForm,   ToDatatype,   ToLanguageTag,   Value).


%! rdf_literal(+Literal:compound) is semidet.
%! rdf_literal(-Literal:compound) is nondet.

rdf_literal(Literal):-
  % Enumerates all literals.
  rdf_current_literal(Literal).


%! rdf_literal(+Literal:compound, +RdfGraph:atom) is semidet.
%! rdf_literal(+Literal:compound, -RdfGraph:atom) is nondet.
%! rdf_literal(-Literal:compound, +RdfGraph:atom) is nondet.
%! rdf_literal(-Literal:compound, -RdfGraph:atom) is nondet.

rdf_literal(Literal, G):-
  % Enumerated all literals.
  rdf_literal(Literal),
  % Relates to an RDF graph.
  rdf_object(G, Literal).


%! rdf_literal(+Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is semidet.
%! rdf_literal(+Literal:compound, -LexicalForm:atom, -DatatypeIri:iri, ?LanguageTag:atom) is det.
%! rdf_literal(-Literal:compound, +LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom) is det.
% Construct/disassemble an RDF literal compound term in the Semweb format.

rdf_literal(literal(lang(LangTag,LexicalForm)), LexicalForm, rdf:langString, LangTag).
rdf_literal(literal(type(Datatype,LexicalForm)), LexicalForm, Datatype, _).
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
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  rdf_equal(rdf:langString, Datatype),
  rdf(S, P, literal(lang(LangTag,LexicalForm)), G).
% Typed literals.
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  var(LangTag),
  rdf(S, P, literal(type(Datatype,LexicalForm)), G).
% Simple literals.
rdf_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  var(LangTag),
  rdf_equal(xsd:string, Datatype),
  rdf(S, P, literal(LexicalForm), G),
  atom(LexicalForm).


%! rdf_literal_equality(+Literal1:compound, +Literal2:compound) is semidet.
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
  literal(type(Datatype1,LexicalForm1)),
  literal(type(Datatype2,LexicalForm2))
):- !,
  Datatype1 == Datatype2,
  xsd_lexical_canonical_map(Datatype1, LexicalForm1, CanonicalLexicalForm1),
  xsd_lexical_canonical_map(Datatype2, LexicalForm2, CanonicalLexicalForm2),
  CanonicalLexicalForm1 == CanonicalLexicalForm2.
% Simple literals that are the same.
rdf_literal_equality(literal(LexicalForm1), literal(LexicalForm2)):-
  atom(LexicalForm1),
  atom(LexicalForm2),
  LexicalForm1 == LexicalForm2.


%! rdf_literal_map(-CanonicalLexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom, +Value) is det.
%! rdf_literal_map(+LexicalForm:atom, +DatatypeIri:iri, ?LanguageTag:atom, -Value) is det.

% Support for =|rdf:langString|=.
rdf_literal_map(LexicalForm, rdf:langString, LangTag, lang(LangTag,LexicalForm)):- !,
  nonvar(LexicalForm),
  nonvar(LangTag).
% Support for the XSD datatypes.
rdf_literal_map(LexicalForm, Datatype, LangTag, Value):-
  xsd_datatype(Datatype),
  var(LangTag),
  (
    nonvar(Value)
  ->
    xsd_canonical_map(Datatype, Value, LexicalForm)
  ;
    nonvar(LexicalForm)
  ->
    xsd_lexical_map(Datatype, LexicalForm, Value)
  ).


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?RdfGraph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Does not retract language-tagged strings for specific language tags.

rdf_retractall_literal(S, P, LexicalForm, Datatype, G):-
  rdf_retractall_literal(S, P, LexicalForm, Datatype, _, G).


%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?DatatypeIri:iri,
%!   ?LanguageTag:atom,
%!   ?RdfGraph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
% asserted with datatype IRI =|xsd:string|=.
% We do not retract literal compound terms of the form
% =|literal(LexicalForm:atom)|=.

% Retract language-tagged strings.
rdf_retractall_literal(S, P, LexicalForm, Datatype, LangTag, G):-
  (rdf_equal(rdf:langString, Datatype) ; nonvar(LangTag)), !,
  rdf_retractall(S, P, literal(lang(LangTag,LexicalForm)), G).
% Retract others.
rdf_retractall_literal(S, P, LexicalForm, Datatype, _, G):-
  rdf_retractall(S, P, literal(type(Datatype,LexicalForm)), G).


%! rdf_update_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?FromLexicalForm:atom,
%!   ?FromDatatype:iri,
%!   +FromLanguageTag:atom,
%!   +RdfGraph:atom,
%!   +Action:compound
%! ) is det.

rdf_update_literal(
  S,
  P,
  FromLexicalForm,
  FromDatatype,
  FromLanguageTag,
  G,
  literal(ToLexicalForm,ToDatatype,ToLanguageTag)
):-
  forall(
    rdf_literal(S, P, FromLexicalForm, FromDatatype, FromLanguageTag, G),
    (
      rdf_convert_literal(
        FromLexicalForm,
        FromDatatype,
        FromLanguageTag,
        ToLexicalForm,
        ToDatatype,
        ToLanguageTag
      ),
      rdf_retractall_literal(
        S,
        P,
        FromLexicalForm,
        FromDatatype,
        FromLanguageTag,
        G
      ),
      rdf_assert_literal(S, P, ToLexicalForm, ToDatatype, ToLanguageTag, G)
    )
  ).

