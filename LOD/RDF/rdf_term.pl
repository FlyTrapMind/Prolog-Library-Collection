:- module(
  rdf_term,
  [
% SYNTACTIC TERMS
    rdf_bnode/2, % ?RdfGraph:atom
                 % ?BlankNode:bnode
    rdf_iri/2, % ?RdfGraph:atom
               % ?Iri:iri
    rdf_is_iri/1, % +Iri:iri
    rdf_name/2, % ?RdfGraph:atom
                % ?RdfName:oneof([iri,literal])
    rdf_term/2, % ?RdfGraph:atom
                % ?RdfTerm:or([bnode,iriliteral])
    rdf_vocabulary/2, % +RdfGraph:atom
                      % -RdfVocabulary:ordset(oneof([iriliteral]))
% POSITIONAL OCUCURRENCE
    rdf_is_subject/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_is_predicate/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_is_object/1, % +RdfTerm:or([bnode,iri,literal])
    rdf_subject/2, % ?RdfGraph:atom
                   % ?RdfSubjectTerm:oneof([bnode,iri])
    rdf_predicate/2, % ?RdfGraph:atom
                     % ?RdfPredicateTerm:iri
    rdf_object/2, % ?RdfGraph:atom
                  % ?RdfObjectTerm:oneof([bnode,literal,iri])
    rdf_node/2 % ?RdfGraph:atom
               % ?RdfNode:or([bnode,iri,literal])
  ]
).

/** <module> RDF Term

Support for RDF 1.1 terms.
Support for RDF literals is found in [rdf_literal].

@author Wouter Beek
@see CyganiakWoodLanthaler2014
     RDF 1.1 Concepts and Abstract Syntax
     http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2012/01-2013/05, 2013/07-2013/08, 2014/01-2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_iri(?,r)).
:- rdf_meta(rdf_is_iri(r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_term(?,r)).
:- rdf_meta(rdf_vocabulary(?,t)).
:- rdf_meta(rdf_is_subject(r)).
:- rdf_meta(rdf_is_predicate(r)).
:- rdf_meta(rdf_is_object(r)).
:- rdf_meta(rdf_subject(?,r)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_node(?,r)).



% SYNTACTIC TERMS %

%! rdf_bnode(+Graph:atom, +BNode:bnode) is semidet.
%! rdf_bnode(+Graph:atom, -BNode:bnode) is nondet.
%! rdf_bnode(-Graph:atom, +BNode:bnode) is nondet.
%! rdf_bnode(-Graph:atom, -BNode:bnode) is nondet.

rdf_bnode(Graph, BNode):-
  % We do not need to consider RDF terms
  % that occur only in the predicate position.
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).


%! rdf_iri(+Graph:atom, +IRI:iri) is semidet.
%! rdf_iri(+Graph:atom, -IRI:iri) is nondet.
%! rdf_iri(-Graph:atom, +IRI:iri) is nondet.
%! rdf_iri(-Graph:atom, -IRI:iri) is nondet.

rdf_iri(Graph, IRI):-
  rdf_term(Graph, IRI),
  rdf_is_iri(IRI).


%! rdf_is_iri(+IRI:iri) is semidet.
% Succeeds for atoms that conform to the syntactic requirement of being
% an IRI RDF term.
%
% This does not imply that the term occurs in an actual triple or graph.
%
% @see The predicate rdf_is_resource/1 in library(semweb/rdf_db)
%      is quite misleading.
%      A first mistake one may make is to think that
%      this predicate is about semantics (resources being objects)
%      while it actually is about syntax
%      (RDF terms that are either IRIs or blank nodes).
%      A second mistake one may make is to assume that
%      rdf_is_resource/1 will succeed for precisely those
%      syntactic constructs that have a resource as their interpretation.
%      But this is not the case either,
%      since typed literals are mapped onto resources as well.

rdf_is_iri(IRI):-
  is_of_type(iri, IRI).


%! rdf_name(+Graph:atom, +RDF_Name:oneof([literal,iri])) is semidet.
%! rdf_name(+Graph:atom, -RDF_Name:oneof([literal,iri])) is nondet.
%! rdf_name(-Graph:atom, +RDF_Name:oneof([literal,iri])) is nondet.
%! rdf_name(-Graph:atom, -RDF_Name:oneof([literal,iri])) is nondet.
% RDF names are IRIs and RDF literals.
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

rdf_name(Graph, Name):-
  rdf_subject(Graph, Name),
  \+ rdf_is_bnode(Name).
rdf_name(Graph, Name):-
  rdf_predicate(Graph, Name).
rdf_name(Graph, Name):-
  rdf_object(Graph, Object),
  \+ rdf_is_bnode(Object),
  % Specifically include datatypes that are strictly speaking not RDF terms.
  (
    Name = Object
  ;
    Object = literal(type(Name,_))
  ).


%! rdf_term(+Graph:graph, +Term:or([bnode,iri,literal])) is semidet.
%! rdf_term(+Graph:graph, -Term:or([bnode,iri,literal])) is nondet.
%! rdf_term(-Graph:graph, +Term:or([bnode,iri,literal])) is nondet.
%! rdf_term(-Graph:graph, -Term:or([bnode,iri,literal])) is nondet.
% Pairs of graphs and terms that occur in that graph.
%
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

rdf_vocabulary(Graph, Vocabulary):-
  setoff(RdfName, rdf_name(Graph, RdfName), Vocabulary).





% POSITIONAL OCCURRENCE %

%! rdf_is_subject(+RdfTerm:or([bnode,iri,literal])) is semidet.
%! rdf_is_predicate(+RdfTerm:or([bnode,iri,literal])) is semidet.
%! rdf_is_object(+RdfTerm:or([bnode,iri,literal])) is semidet.
% Succeeds if the given syntactic term could occur in the indicated
% position of an RDF triple.
%
% This is independent of the term occurring in an actual triple or graph.

rdf_is_subject(Subject):-
  rdf_is_bnode(Subject), !.
rdf_is_subject(Subject):-
  rdf_is_iri(Subject).

rdf_is_predicate(Predicate):-
  rdf_is_iri(Predicate).

rdf_is_object(Object):-
  rdf_is_subject(Object), !.
rdf_is_object(Object):-
  rdf_is_literal(Object).


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
  rdf(S, _, _, G).

rdf_predicate(G, P):-
  rdf(_, P, _, G).

rdf_object(G, O):-
  rdf(_, _, O, G).


%! rdf_node(+Graph:atom, +Node:or([bnode,iri,literal])) is semidet.
%! rdf_node(+Graph:atom, -Node:or([bnode,iri,literal])) is nondet.
%! rdf_node(-Graph:atom, +Node:or([bnode,iri,literal])) is nondet.
%! rdf_node(-Graph:atom, -Node:or([bnode,iri,literal])) is nondet.
% The set of nodes of an RDF graph is
% the set of subjects and objects of triples in the graph.
%
% It is possible for a predicate IRI to also occur as a node
% in the same graph.

rdf_node(Graph, Node):-
  rdf_subject(Graph, Node).
rdf_node(Graph, Node):-
  rdf_object(Graph, Node).

