:- module(
  rdf_term,
  [
    rdf_bnode/2, % ?Graph:graph
                 % ?BNode:bnode
    rdf_literal_equality/2, % +Literal1:literal
                            % +Literal2:literal
    rdf_name/2, % ?Graph:atom
                % +RDF_Name:oneof([literal,uri])
    rdf_node/2, % ?Graph:atom
                % ?Node:or([bnode,uri,literal])
    rdf_object/2, % ?Graph:graph
                  % ?Objects:oneof([bnode,literal,uri])
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
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:oneof([bnode,uri])
    rdf_term/2, % ?Graph:atom
                % ?RDF_Term:or([bnode,literal,uri])
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ordset(oneof([literal,uri]))
  ]
).

/** <module> RDF_TERM

@author Wouter Beek
@version 2012/01-2013/05, 2013/07-2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsd(xsd)).

:- rdf_meta(rdf_bnode(?,r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_po_pairs(r,-)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_subject(?,r)).



rdf_bnode(Graph, BNode):-
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).

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

%! rdf_vocabulary(+Graph:atom, -Vocabulary:ordset([literal,iri])) is det.
% Returns the vocabulary of the given graph.
%
% The vocabulary of a graph is the set of RDF names that occur
% in the triples of the graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_vocabulary(G, Vocabulary):-
  setoff(RDF_Name, rdf_name(G, RDF_Name), Vocabulary).

