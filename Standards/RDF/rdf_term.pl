:- module(
  rdf_term,
  [
    rdf_bnode/2, % ?Graph:atom
                 % ?BNode:bnode
    rdf_iri/2, % ?Graph:atom
               % ?IRI:iri
    rdf_is_iri/1,
    rdf_is_object/1,
    rdf_is_predicate/1,
    rdf_is_subject/1,
    rdf_is_plain_literal/1, % ?PlainLiteral:compound
    rdf_is_simple_literal/1, % ?SimpleLiteral:compound
    rdf_literal_equality/2, % +Literal1:literal
                            % +Literal2:literal
    rdf_name/2, % ?Graph:atom
                % +RDF_Name:oneof([literal,uri])
    rdf_node/2, % ?Graph:atom
                % ?Node:or([bnode,uri,literal])
    rdf_object/2, % ?Graph:atom
                  % ?Objects:oneof([bnode,literal,uri])
    rdf_plain_literal/1, % ?PlainLiteral:compound
    rdf_plain_literal/2, % ?Graph:atom
                         % ?PlainLiteral:compound
    rdf_po_pairs/2, % +Resource:uri
                    % -PredicateObjectPairs:list(pair)
    rdf_predicate/2, % ?Graph:atom
                     % ?Predicate:uri
    rdf_predicates/2, % +Graph:atom
                      % -Predicates:ordset(uri)
    rdf_shared_po_pairs/5, % +X_PO_Pairs:ordset(list),
                           % +Y_PO_Pairs:ordset(list),
                           % -Shared_PO_Pairs:ordset(list),
                           % -X_Exclusive_PO_Pairs:ordset(list),
                           % -Y_Exclusive_PO_Pairs:ordset(list)
    rdf_shared_p_triples/5, % +X_P_Pairs1:ordset(list)
                             % +Y_P_Pairs1:ordset(list)
                             % -SharedPredicateTuples:ordset(list)
                             % -X_P_Pairs2:ordset(list)
                             % -Y_P_Pairs2:ordset(list)
    rdf_simple_literal/1, % ?SimpleLiteral:compound
    rdf_simple_literal/2, % ?Graph:atom
                          % ?SimpleLiteral:compound
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:oneof([bnode,uri])
    rdf_term/2, % ?Graph:atom
                % ?RDF_Term:or([bnode,literal,uri])
    rdf_typed_literal/1, % ?TypedLiteral:compound
    rdf_typed_literal/2, % ?Graph:atom
                         % ?TypedLiteral:compound
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ordset(oneof([literal,uri]))
  ]
).

/** <module> RDF_TERM

RDF triples consist of three terms:
  * _Subject_, which is an RDF URI reference or a blank node
  * _Predicate_, which is an RDF URI reference
  * _Object_, which is an RDF URI reference, a literal or a blank node

## Blank node

## Literal

## URI reference

A URI reference within an RDF graph (an RDF URI reference) is
a Unicode string that:
  * does not contain any control characters (#x00-#x1F, #x7F-#x9F)
  * and would produce a valid URI character sequence
    (per RFC-2396, sections 2.1) representing an absolute URI with
    optional fragment identifier when subjected to the encoding
    described below.

The encoding consists of:
  * encoding the Unicode string as UTF-8 [RFC-2279],
    giving a sequence of octet values.
  * %-escaping octets that do not correspond to
    permitted US-ASCII characters.

The disallowed octets that must be %-escaped include
all those that do not correspond to US-ASCII characters,
and the excluded characters listed in Section 2.4 of RFC-2396,
except for the number sign (#), percent sign (%), and
the square bracket characters re-allowed in RFC-2732.

Disallowed octets must be escaped with the URI escaping mechanism
(that is, converted to =|%HH|=, where =HH= is the 2-digit hexadecimal
numeral corresponding to the octet value).

### Equality

Two RDF URI references are equal if and only if they compare as equal,
character by character, as Unicode strings.

### Relation to "XML Schema 2: Datatypes"

RDF URI references are compatible with the =anyURI= datatype
as defined by XML Schema Part 2: Datatypes, constrained to be
an absolute rather than a relative URI reference.

### Relation to "XML Namespaces 1.1"

RDF URI references are compatible with International Resource Identifiers
as defined by XML Namespaces 1.1.

### Relation to an IRI RFC standard

This section anticipates an RFC on Internationalized Resource Identifiers.
Implementations may issue warnings concerning the use of RDF URI References
that do not conform with the IRI draft or its successors.

### Relation to concrete syntaxes

The restriction to absolute URI references is found in this abstract syntax.
When there is a well-defined base URI, concrete syntaxes, such as RDF/XML,
may permit relative URIs as a shorthand for such absolute URI references.

### Warning for seemingly equal RDF URI references

Because of the risk of confusion between RDF URI references that
would be equivalent if derefenced, the use of %-escaped characters
in RDF URI references is strongly discouraged.

See also the URI equivalence issue of the Technical Architecture Group [TAG].

@author Wouter Beek
@see KlyneCarroll2004 http://www.w3.org/TR/rdf-concepts/#dfn-URI-reference
@version 2012/01-2013/05, 2013/07-2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_bnode(?,r)).
:- rdf_meta(rdf_is_object(r)).
:- rdf_meta(rdf_is_predicate(r)).
:- rdf_meta(rdf_is_subject(r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_po_pairs(r,-)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_subject(?,r)).
:- rdf_meta(rdf_typed_literal(r)).
:- rdf_meta(rdf_typed_literal(?,r)).



rdf_bnode(Graph, BNode):-
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).

rdf_iri(G, IRI):-
  rdf_term(G, IRI),
  % Exclude blank nodes and literals.
  rdf_is_iri(IRI).

%! rdf_is_iri(+IRI) is semidet.
% The predicate rdf_is_resource/1 in SWI-Prolog's Semweb library is
% quite misleading. A first mistake one might make is thinking that
% the predicate applies to semantic objects (since a resource is
% a semantic object according to RDFS Semantics).
% However, the predicate applies to syntactic constructs instead.
%
% A second mistake one is likely to make, is to assume that
% rdf_is_resource/1 will succeed for precisely those syntactic constructs
% that have a resource as their meaning.
% But this is not the case either, since typed literals are mapped onto
% resources, but rdf_is_resource/1 fails for them.

rdf_is_iri(IRI):-
  rdf_is_resource(IRI),
  \+ rdf_is_bnode(IRI).

rdf_is_object(Object):-
  rdf_is_subject(Object), !.
rdf_is_object(Object):-
  rdf_is_literal(Object).

rdf_is_plain_literal(literal(lang(Lang,Lit))):-
  atomic(Lang),
  atomic(Lit).

rdf_is_predicate(Predicate):-
  rdf_is_iri(Predicate).

rdf_is_simple_literal(literal(Lit)):-
  atomic(Lit).

rdf_is_subject(Subject):-
  rdf_is_bnode(Subject), !.
rdf_is_subject(Subject):-
  rdf_is_iri(Subject).

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

rdf_literal_equality(literal(lang(Lang1,Lit1)), literal(lang(Lang2,Lit2))):- !,
  Lang1 == Lang2,
  Lit1 == Lit2.
rdf_literal_equality(literal(type(Type1,LEX1)), literal(type(Type2,LEX2))):- !,
  Type1 == Type2,
  xsd_lexicalMap(Type1, LEX1, Value1),
  xsd_canonicalMap(Type1, Value1, CAN1),
  xsd_lexicalMap(Type2, LEX2, Value2),
  xsd_canonicalMap(Type2, Value2, CAN2),
  CAN1 == CAN2.
rdf_literal_equality(literal(Lit1), literal(Lit2)):- !,
  Lit1 == Lit2.

%! rdf_name(?G:atom, ?RDF_Name:oneof([literal,uri])) is nondet.
% Succeeds if the given object is an RDF name.
%
% An RDF name is either an RDF URI reference or an RDF literal.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/
% @tbd Update the definition for IRIs.

rdf_name(G, RDF_Name):-
  nonvar_det(rdf_name_(G, RDF_Name)).
rdf_name_(G, RDF_Name):-
  rdf_subject(G, RDF_Name),
  \+ rdf_is_bnode(RDF_Name).
rdf_name_(G, RDF_Name):-
  rdf_predicate(G, RDF_Name).
rdf_name_(G, RDF_Name):-
  rdf_object(G, Object),
  \+ rdf_is_bnode(Object),
  (
    Object = literal(type(Datatype, _LexicalValue))
  ->
    % Specifically include datatypes that are strictly speaking not RDF terms.
    (RDF_Name = Object ; RDF_Name = Datatype)
  ;
    RDF_Name = Object
  ).

rdf_node(Graph, Node):-
  nonvar_det(rdf_node0(Graph, Node)).
rdf_node0(Graph, Node):-
  rdf_subject(Graph, Node).
rdf_node0(Graph, Node):-
  rdf_object(Graph, Node).

rdf_object(G, O):-
  nonvar_det(rdf_object0(G, O)).
rdf_object0(G, O):-
  rdf(_, _, O, G).

%! rdf_plain_literal(?PlainLiteral:compound) is nondet.
% @see rdf_plain_literal/2

rdf_plain_literal(PlainLit):-
  rdf_plain_literal(_G, PlainLit).

%! rdf_plain_literal(?Graph:atom, ?PlainLiteral:compound) is nondet.

rdf_plain_literal(G, Lit):-
  % rdf/[3,4] throws an exception for numeric input.
  \+ number(Lit),
  rdf(_, _, Lit, G),
  Lit = literal(lang(Lang,_)),
  % It is apparently a feature of rdf/[3,4] to match
  % =|literal(lang(Language,Literal))|= against =|literal(Literal)|=,
  % so we need to check for the language tag being instantiated.
  nonvar(Lang).
rdf_plain_literal(G, Lit):-
  rdf_simple_literal(G, Lit).

rdf_po_pairs(Resource, PO_Pairs):-
  is_uri(Resource), !,
  setoff(
    [P,O],
    rdf(Resource, P, O, _Graph),
    PO_Pairs
  ).

rdf_predicate(G, P):-
  nonvar_det(rdf_predicate0(G, P)).
rdf_predicate0(G, P):-
  rdf(_, P, _, G).

rdf_predicates(Graph, Predicates):-
  setoff(
    Predicate,
    rdf_predicate(Graph, Predicate),
    Predicates
  ).

%! rdf_shared_po_pairs(
%!   +X_PO_Pairs:ordset(pair),
%!   +Y_PO_Pairs:ordset(pair),
%!   -Shared_PO_Pairs:ordset(triple),
%!   -X_Exclusive_PO_Pairs:ordset(pair),
%!   -Y_Exclusive_PO_Pairs:ordset(pair)
%! ) is det.

rdf_shared_po_pairs(
  X_PO_Pairs,
  Y_PO_Pairs,
  Shared_PO_Pairs,
  X_Exclusive_PO_Pairs,
  Y_Exclusive_PO_Pairs
):-
  ord_intersection(
    X_PO_Pairs,
    Y_PO_Pairs,
    Shared_PO_Pairs,
    Y_Exclusive_PO_Pairs
  ),
  ord_subtract(X_PO_Pairs, Shared_PO_Pairs, X_Exclusive_PO_Pairs).

%! rdf_shared_p_triples(
%!   +X_PO_Pairs:ordset(pair),
%!   +Y_PO_Pairs:ordset(pair),
%!   -Shared_P_Tuples:ordset(triple),
%!   -X_Exclusive_PO_Pairs:ordset(pair),
%!   -Y_Exclusive_PO_Pairs:ordset(pair)
%! ) is det.

rdf_shared_p_triples(
  X_PO_Pairs,
  Y_PO_Pairs,
  Shared_P_Triples,
  X_Exclusive_P_Pairs,
  Y_Exclusive_P_Pairs
):-
  setoff(
    P-X_O-Y_O,
    (
      member(P-X_O, X_PO_Pairs),
      member(P-Y_O, Y_PO_Pairs)
    ),
    Shared_P_Triples
  ),
  ord_subtract(X_PO_Pairs, Shared_P_Triples, X_Exclusive_P_Pairs),
  ord_subtract(Y_PO_Pairs, Shared_P_Triples, Y_Exclusive_P_Pairs).

%! rdf_simple_literal(?SimpleLiteral:atom) is nondet.
% @see rdf_simple_literal/2

rdf_simple_literal(Lit):-
  rdf_simple_literal(_, Lit).

%! rdf_simple_literal(?Graph:atom, ?SimpleLiteral:atom) is nondet.

rdf_simple_literal(G, Lit):-
  % rdf/[3,4] throws an exception for numeric input.
  \+ number(Lit),
  rdf(_, _, Lit, G),
  Lit = literal(Lex),
  % Exclude cases in which `Lex` is a compound term,
  % i.e., either `lang(Lang,Lexical)` or `type(Type,Lexical)`.
  atomic(Lex).

rdf_subject(G, S):-
  nonvar_det(rdf_subject_(G, S)).
rdf_subject_(G, S):-
  rdf(S, _P, _O, G).

%! rdf_term(?Graph:graph, ?Term:uri) is nondet.
% Pairs of graphs and terms that occur in that graph.
% A term is either a subject, predicate or object term
% in an RDF triple.
%
% @param Graph The atomic name of a graph.
% @param Term A resource.

rdf_term(Graph, Term):-
  rdf_node(Graph, Term).
rdf_term(Graph, Term):-
  rdf_predicate(Graph, Term).

%! rdf_typed_literal(?TypedLiteral:compound) is nondet.
% @see rdf_typed_literal/2

rdf_typed_literal(Lit):-
  rdf_typed_literal(_, Lit).

%! rdf_typed_literal(?Graph:atom, TypedLiteral:compound) is nondet.

rdf_typed_literal(G, Lit1):-
  rdf_global_object(Lit1, Lit2),
  rdf(_, _, Lit2, G),
  Lit2 = literal(type(_,_)).

%! rdf_vocabulary(+Graph:atom, -Vocabulary:ordset([literal,iri])) is det.
% Returns the vocabulary of the given graph.
%
% The vocabulary of a graph is the set of RDF names that occur
% in the triples of the graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_vocabulary(G, Vocabulary):-
  setoff(RDF_Name, rdf_name(G, RDF_Name), Vocabulary).

