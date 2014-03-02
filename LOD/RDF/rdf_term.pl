:- module(
  rdf_term,
  [
    rdf_object/2, % ?Graph:atom
                  % ?Objects:oneof([bnode,literal,uri])
    rdf_predicate/2, % ?Graph:atom
                     % ?Predicate:uri

    rdf_bnode/2, % ?Graph:atom
                 % ?BNode:bnode
    rdf_iri/2, % ?Graph:atom
               % ?IRI:iri
    rdf_is_iri/1,
    rdf_is_object/1,
    rdf_is_predicate/1,
    rdf_is_subject/1,
    rdf_name/2, % ?Graph:atom
                % ?RDF_Name:oneof([literal,uri])
    rdf_node/2, % ?Graph:atom
                % ?Node:or([bnode,uri,literal])
    rdf_po_pairs/2, % +Resource:uri
                    % -PredicateObjectPairs:list(pair)
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
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:oneof([bnode,uri])
    rdf_term/2, % ?Graph:atom
                % ?RDF_Term:or([bnode,literal,uri])
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ordset(oneof([literal,uri]))
  ]
).

/** <module> RDF Term

## Source

## Vocabulary


## Triple: Pragmatics



--

@author Wouter Beek
@see KlyneCarrollMcbride2014
     RDF 1.1 Concepts and Abstract Syntax
     http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2012/01-2013/05, 2013/07-2013/08, 2014/01-2014/02
*/

:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_subject(?,r)).

:- use_module(library(semweb/rdf_db)).
:- use_module(programming(prolog_mode)).



%! rdf_subject(+Graph:atom, +Subject:or([bnode,iri])) is semidet.
%! rdf_subject(+Graph:atom, -Subject:or([bnode,iri])) is nondet.
%! rdf_subject(-Graph:atom, +Subject:or([bnode,iri])) is nondet.
%! rdf_subject(-Graph:atom, -Subject:or([bnode,iri])) is nondet.
%! rdf_predicate(+Graph:atom, +Predicate:iri) is semidet.
%! rdf_predicate(+Graph:atom, -Predicate:iri) is nondet.
%! rdf_predicate(-Graph:atom, +Predicate:iri) is nondet.
%! rdf_predicate(-Graph:atom, -Predicate:iri) is nondet.
%! rdf_object(+Graph:atom, +Object:or([bnode,iri,literal])) is semidet.
%! rdf_object(+Graph:atom, -Object:or([bnode,iri,literal])) is nondet.
%! rdf_object(-Graph:atom, +Object:or([bnode,iri,literal])) is nondet.
%! rdf_object(-Graph:atom, -Object:or([bnode,iri,literal])) is nondet.
% Triples consist of the following kinds of terms:
%   * _Subject_, which is an RDF URI reference or a blank node
%   * _Predicate_, which is an RDF URI reference
%   * _Object_, which is an RDF URI reference, a literal or a blank node
%
% Terms can therefore be named by
%  the position in which they occur in a triple.
%
% Whether a term is either of these three is _relative_
%  to a given triple.
%
% These predicates check whether a term occurs in a graph;
%  relate terms to graphs and vice versa,
%  and generate the terms in a graphs
%  and the graphs in which a term occurs.

rdf_subject(G, S):-
  enforce_mode(
    rdf(S, P, O, G),
    [S,G],
    [['+','+']-semidet,['+','-']-nondet,['-','+']-nondet,['-','-']-nondet]
  ).

rdf_predicate(G, P):-
  rdf(_, P, _, G).
  enforce_mode(
    rdf(_, P, _, G),
    [P, G]
    [['+','+']-semidet,['+','-']-nondet,['-','+']-nondet,['-','-']-nondet]
  ).

rdf_object(G, O):-
  enforce_mode(
    rdf(_, _, O, G),
    [O,G],
    [['+','+']-semidet,['+','-']-nondet,['-','+']-nondet,['-','-']-nondet]
  ).



/*

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
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- rdf_meta(rdf_bnode(?,r)).
:- rdf_meta(rdf_is_object(r)).
:- rdf_meta(rdf_is_predicate(r)).
:- rdf_meta(rdf_is_subject(r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_po_pairs(r,-)).



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

rdf_is_predicate(Predicate):-
  rdf_is_iri(Predicate).

rdf_is_subject(Subject):-
  rdf_is_bnode(Subject), !.
rdf_is_subject(Subject):-
  rdf_is_iri(Subject).


%! rdf_name(+Graph:atom, +RDF_Name:oneof([literal,iri])) is semidet.
%! rdf_name(+Graph:atom, -RDF_Name:oneof([literal,iri])) is nondet.
%! rdf_name(-Graph:atom, +RDF_Name:oneof([literal,iri])) is nondet.
%! rdf_name(-Graph:atom, -RDF_Name:oneof([literal,iri])) is nondet.
% According to RDF Semantics, IRIs and literals are names.
%
% # Instatiations
%
% | ++ | semidet | Does this RDF graph contain this RDF name?              |
% | +- | nondet  | Enumerate the RDF names in this RDF graph.\c
%                  An RDF graph may have zero or more RDF names.           |
% | -+ | nondet  | Enumerate the RDF graphs in which this RDF name occurs. |
%                  An RDF name may occur nowhere.                          |
% | -- | nondet  | Enumerate pars of RDF graphs and RDF names.             |
%
% @arg Graph The atomic name of an RDF graph.
% @arg RDF_Name Either an IRI or an RDF literal.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_name(G, N):-
  enforce_mode(
    '_rdf_name'(G, N),
    [['+','+']-semidet,['+','-']-nondet,['-','+']-nondet,['-','-']-nondet]
  ).
'_rdf_name'(G, S):-
  rdf_subject(G, S),
  \+ rdf_is_bnode(S).
'_rdf_name'(G, P):-
  rdf_predicate(G, P).
'_rdf_name'(G, O2):-
  rdf_object(G, O1),
  \+ rdf_is_bnode(O1),
  (
    O1 = literal(type(Datatype, _LexicalValue))
  ->
    % Specifically include datatypes that are strictly speaking not RDF terms.
    (O2 = O1 ; O2 = Datatype)
  ;
    O2 = O1
  ).


rdf_node(Graph, Node):-
  rdf_subject(Graph, Node).
rdf_node(Graph, Node):-
  rdf_object(Graph, Node).

rdf_po_pairs(Resource, PO_Pairs):-
  must_be(iri, Resource), !,
  setoff(
    [P,O],
    rdf(Resource, P, O, _Graph),
    PO_Pairs
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

%! rdf_term(?Graph:graph, ?Term:uri) is nondet.
% Pairs of graphs and terms that occur in that graph.
% A term is either a subject, predicate or object term
% in an RDF triple.
%
% @arg Graph The atomic name of a graph.
% @arg Term A resource.

rdf_term(Graph, Term):-
  rdf_node(Graph, Term).
rdf_term(Graph, Term):-
  rdf_predicate(Graph, Term).

%! rdf_vocabulary(+Graph:atom, -Vocabulary:ordset([literal,iri])) is det.
% Returns the vocabulary of the given graph.
%
% The vocabulary of a graph is the set of RDF names that occur
% in the triples of the graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_vocabulary(G, Vocabulary):-
  setoff(RDF_Name, rdf_name(G, RDF_Name), Vocabulary).

