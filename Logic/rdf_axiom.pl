:- module(
  rdf_axioms,
  [
    axiom/4, % ?Language:oneof([rdf,rdfs])
             % ?Subject:or([bnode,iri])
             % ?Predicate:iri
             % ?Object:or([bnode,literal,iri])
    bnode_literal_map/2, % ?BNode:bnode
                         % ?Literal:compound
    materialize/0,
    query/3 % ?Subject:or([bnode,iri])
            % ?Predicate:iri
            % ?Object:or([bnode,literal,iri])
  ]
).

/** <module> RDF AXIOMS

An axiomatic approach towards RDF(S) materialization.

@author Wouter Beek
@see Hayes2004
@version 2013/05, 2013/08
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).
:- use_module(library(debug)).
:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)). % Meta-option.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- dynamic(bnode_literal_map_/2).

%! axiom(
%!   ?Language:oneof([rdf,rdfs]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(axiom/4).
:- rdf_meta(axiom(?,r,r,r)).

:- rdf_meta(query(r,r,r)).
:- rdf_meta(query(?,r,r,r)).

%! rule(M,
%!   -Rule:atom,
%!   -Premises:list(triple),
%!   ?Mode:oneof([m,q]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(rule/6).
:- rdf_meta(rule(-,-,?,r,r,r)).

%! stmt(
%!   -Tree:compound,
%!   ?Mode:oneof([m,q]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(stmt/5).
:- rdf_meta(stmt(-,?,r,r,r)).

:- debug(rdf_axiom).



%! bnode_literal_map(?BNode:bnode, ?Literal:literal) is nondet.
% The mapping between blank nodes and literals, which is used
% to assert predications of literals, even though literals cannot
% be subject terms according to RDF 1.0 syntax.

bnode_literal_map(BNode, Literal):-
  db_add_novel(bnode_literal_map_(BNode, Literal)).

materialize:-
  setoff(
    T0-Lang-S/P/O,
    (
      rule(T0, m, S, P, O),
      % We only collect new triples.
      \+ rdf(S, P, O)
    ),
    Triples
  ),
  length(Triples, NumberOfTriples),
  debug(rdf_axiom, '~w triples were added.', [NumberOfTriples]),
  forall(
    member(Tree-Lang-S/P/O, Triples),
    (
      print_proof([transformation(rdf_triple_name)], atom(Atom), Tree),
      debug(rdf_axiom, '~w', [Atom]),
      rdf_assert(S, P, O)
    )
  ).

query(S, P, O):-
  query(T0, S, P, O),
  absolute_file_name(project(query), F, [access(write),file_type(pdf)]),
  tree_to_gv_file([], T0, dot, pdf, F),
  open_pdf(F).

% Explicitly asserted.
query(fact([],S-P-O), S, P, O):-
  rdf(S, P, O).
% Derived using rules and axioms.
query(Tree, S, P, O):-
  rule(Tree, q, S, P, O).

rule(Tree, Mode, S, P, O):-
  rule(Rule, Premises, Mode, S, P, O),
  % @tbd What's this?
  rdf_graph:rdf_triple(S, P, O, Conclusion),
  Tree =.. [Rule,Premises,Conclusion].

% All axioms can be deduced as if they are the outcome of a rule.
rule(axiom, [], _Mode, S, P, O):-
  axiom(_Lang, S, P, O).

% Statements occur inside rules (as premises).
% We want to perform materialization in individual steps of depth 1.
% This means that for materialization (mode `m`) we do not use rules.
stmt(fact([],S-P-O), M, S, P, O):-
  memberchk(M, [m,q]),
  rdf(S, P, O).
% Since queries can traverse the search space until arbitrary depth,
% they can use rules a predicates of rules.
stmt(Tree, q, S, P, O):-
  rule(Tree, q, S, P, O).



% RDF RULES & AXIOMS %

/*
% [se1] Simple entailment w.r.t. the object term.
% [lg]  Literal generalization is a special case of [se1],
%       where the object term is a literal.
%       Literal generalization is used whenever something has to be
%       predicated of a literal (since literals cannot occur
%       as subject terms).
rule(se1, [T1], M, S, P, B):-
  stmt(T1, M, S, P, O),
  (
    rdf_is_literal(O)
  ->
    bnode_literal_map(B, O)
  ;
    rdf_bnode(B)
  ).

% [se2] Simple entailment w.r.t. the subject term.
rule(se2, [T1], M, B, P, O):-
  stmt(T1, M, _, P, O),
  rdf_bnode(B).
*/

% [rdf1] Predicate terms are instances of =|rdf:'Property'|=.
rule(rdf1, [T1], M, P, rdf:type, rdf:'Property'):-
  stmt(T1, M, _, P, _).

% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'|=.
rule(rdf2, [T1], M, BNode, rdf:type, rdf:'XMLLiteral'):-
  rdf_global_id(rdf:'XMLLiteral', XMLLiteralType),
  TypedLiteral = literal(type(XMLLiteralType,_)),
  stmt(T1, M, _, _, TypedLiteral),
  bnode_literal_map(BNode, TypedLiteral).

% RDF axiomatic triples.
axiom(rdf, rdf:type,      rdf:type, rdf:'Property').
axiom(rdf, rdf:subject,   rdf:type, rdf:'Property').
axiom(rdf, rdf:predicate, rdf:type, rdf:'Property').
axiom(rdf, rdf:object,    rdf:type, rdf:'Property').
axiom(rdf, rdf:first,     rdf:type, rdf:'Property').
axiom(rdf, rdf:rest,      rdf:type, rdf:'Property').
axiom(rdf, rdf:value,     rdf:type, rdf:'Property').
axiom(rdf, rdf:'_1',      rdf:type, rdf:'Property').
axiom(rdf, rdf:'_2',      rdf:type, rdf:'Property').
axiom(rdf, rdf:'_3',      rdf:type, rdf:'Property').
axiom(rdf, rdf:nil,       rdf:type, rdf:'List'    ).
/*
% There is actually an infinite number of integer enumerator axioms...
axiom(IRI, rdf:type, rdf:'Property'):-
  between(1, inf, I),
  format(atom(Name), '_~w', [I]),
  rdf_global_id(rdf:Name, IRI).
*/



% RDFS AXIOMS %

% [gl] Literal instantiation rule
%      This ensures that every triple that contains a literal and
%      its similar triple that contains the allocated blank node
%      (according to the literal generation rule [lg])
%      are derivable from each other.
rule(gl, [T1], M, S, P, Literal):-
  stmt(T1, M, S, P, O),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(O),
  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_literal_map(O, Literal).

% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.
rule(rdfs1, [T1], M, BNode, rdf:type, rdfs:'Literal'):-
  stmt(T1, M, _, _, O),
  rdf_is_literal(O),
  bnode_literal_map(BNode, O).

% [rdfs2]
rule(rdfs2, [T1,T2], M, S, rdf:type, C):-
  stmt(T1, M, P, rdfs:domain, C),
  stmt(T2, M, S, P, _).

% [rdfs3]
rule(rdfs3, [T1,T2], M, O, rdf:type, C):-
  stmt(T1, M, P, rdfs:range, C),
  stmt(T2, M, _, P, O).

% [rdfs4a]
rule(rdfs4a, [T1], M, S, rdf:type, rdfs:'Resource'):-
  stmt(T1, M, S, _, _).
% [rdfs4b]
rule(rdfs4b, [T1], M, O, rdf:type, rdfs:'Resource'):-
  stmt(T1, M, _, _, O).

% [rdfs5] Transitive closure of the property hierarchy relation.
rule(rdfs5, [T1,T2], M, P1, rdfs:subPropertyOf, P3):-
  stmt(T1, M, P1, rdfs:subPropertyOf, P2),
  stmt(T2, M, P2, rdfs:subPropertyOf, P3).

% [rdfs6] Reflexivity of the property hierarchy relation.
rule(rdfs6, [T1], M, P, rdfs:subPropertyOf, P):-
  stmt(T1, M, P, rdf:type, rdf:'Property').

% [rdfs7] Using the property hierarchy.
rule(rdfs7, [T1,T2], M, S, P2, O):-
  stmt(T1, M, P1, rdfs:subPropertyOf, P2),
  stmt(T2, M, S, P1, O).

% [rdfs8] Classes are instances of =|rdfs:Resource|=.
rule(rdfs8, [T1], M, C, rdfs:subClassOf, rdfs:'Resource'):-
  stmt(T1, M, C, rdf:type, rdfs:'Class').

% [rdfs9] Using the class hierarchy.
rule(rdfs9, [T1,T2], M, S, rdf:type, C2):-
  stmt(T1, M, C1, rdfs:subClassOf, C2),
  stmt(T2, M, S, rdf:type, C1).

% [rdfs10] Reflexivity of the class hierarchy relation.
rule(rdfs10, [T1], M, C, rdfs:subClassOf, C):-
  stmt(T1, M, C, rdf:type, rdfs:'Class').

% [rdfs11] Transitivity of the class hierarchy relation.
rule(rdfs11, [T1,T2], M, C1, rdfs:subClassOf, C3):-
  stmt(T1, M, C1, rdfs:subClassOf, C2),
  stmt(T2, M, C2, rdfs:subClassOf, C3).

% [rdfs12]
rule(rdfs12, [T1], M, S, rdf:subClassOf, rdfs:member):-
  stmt(T1, M, S, rdf:type, rdfs:'ContainerMembershipProperty').

% [rdfs13]
rule(rdfs13, [T1], M, S, rdfs:subClassOf, rdfs:'Literal'):-
  stmt(T1, M, S, rdf:type, rdfs:'Datatype').



% RDFS axiomatic triples: domain.
axiom(rdfs,  rdf:type,          rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:domain,        rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:range,         rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:subPropertyOf, rdfs:domain,  rdf:'Property' ).
axiom(rdfs, rdfs:subClassOf,    rdfs:domain, rdfs:'Class'    ).
axiom(rdfs,  rdf:subject,       rdfs:domain,  rdf:'Statement').
axiom(rdfs,  rdf:predicate,     rdfs:domain,  rdf:'Statement').
axiom(rdfs,  rdf:object,        rdfs:domain,  rdf:'Statement').
axiom(rdfs, rdfs:member,        rdfs:domain, rdfs:'Resource' ).
axiom(rdfs,  rdf:first,         rdfs:domain,  rdf:'List'     ).
axiom(rdfs,  rdf:rest,          rdfs:domain,  rdf:'List'     ).
axiom(rdfs, rdfs:seeAlso,       rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:isDefinedBy,   rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:comment,       rdfs:domain, rdfs:'Resource' ).
axiom(rdfs, rdfs:label,         rdfs:domain, rdfs:'Resource' ).
axiom(rdfs,  rdf:value,         rdfs:domain, rdfs:'Resource' ).

% RDFS axiomatic triples: range.
axiom(rdfs,  rdf:type,          rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:domain,        rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:range,         rdfs:range, rdfs:'Class'   ).
axiom(rdfs, rdfs:subPropertyOf, rdfs:range,  rdf:'Property').
axiom(rdfs, rdfs:subClassOf,    rdfs:range, rdfs:'Class'   ).
axiom(rdfs,  rdf:subject,       rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:predicate,     rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:object,        rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:member,        rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:first,         rdfs:range, rdfs:'Resource').
axiom(rdfs,  rdf:rest,          rdfs:range,  rdf:'List'    ).
axiom(rdfs, rdfs:seeAlso,       rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:isDefinedBy,   rdfs:range, rdfs:'Resource').
axiom(rdfs, rdfs:comment,       rdfs:range, rdfs:'Literal' ).
axiom(rdfs, rdfs:label,         rdfs:range, rdfs:'Literal' ).
axiom(rdfs,  rdf:value,         rdfs:range, rdfs:'Resource').

% RDFS axiomatic triples: subclass hierarchy.
axiom(rdfs,  rdf:'Alt', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs,  rdf:'Bag', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs,  rdf:'Seq', rdfs:subClassOf, rdfs:'Container').
axiom(rdfs, rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').

% RDFS axiomatic triples: subproperty hierarchy.
axiom(rdfs, rdfs:isDefinedBy, rdfs:subPropertyOf, rdfs:seeAlso).

% RDFS axiomatic triples: datatypes.
axiom(rdfs,  rdf:'XMLLiteral',  rdf:type,       rdfs:'Datatype').
axiom(rdfs,  rdf:'XMLLiteral', rdfs:subClassOf, rdfs:'Literal' ).
axiom(rdfs, rdfs:'Datatype',   rdfs:subClassOf, rdfs:'Class'   ).

% RDFS axiomatic triples: container membership properies.
axiom(rdfs, rdf:'_1',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_1', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_1', rdfs:range,  rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_2',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_2', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_2', rdfs:range,  rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_3',  rdf:type,   rdfs:'ContainerMembershipProperty').
axiom(rdfs, rdf:'_3', rdfs:domain, rdfs:'Resource'                   ).
axiom(rdfs, rdf:'_3', rdfs:range,  rdfs:'Resource'                   ).
/*
% There is an infinite number of RDFS axioms for integer enumeration...
axiom(UriRef, rdf:type, rdfs:'ContainerMembershipProperty'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
axiom(UriRef, rdfs:domain, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
axiom(UriRef, rdfs:range, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
*/



/*
% [RDFS-1] If a resource has in instance, then it must be an =|rdfs:'Class'|=.
axiom(Y, rdf:type, rdfs:'Class'):-
  stmt(M, _, rdf:type, Y).
% [RDFS-2] Everything is an =|rdfs:'Resource'|=.
axiom(X, rdf:type, rdfs:'Resource'):-
  stmt(M, S, P, O),
  (X = S ; X = P ; X = O).
% [RDFS-3] Plain literals are individuals of =|rdfs:'Literal'|=.
axiom(Lit, rdf:type, rdfs:'Literal'):-
  stmt(M, _, _, literal(Lit)).
% [RDFS-12] Individuals of =|rdfs:'Datatype'|= are subclasses of
%           =|rdfs:'Literal'|=.
axiom(X, rdfs:subClassOf, rdfs:'Literal'):-
  stmt(M, X, rdf:type, rdfs:'Datatype').
% [RDFS-11] Individuals of =|rdfs:'ContainerMembershipProperty'|= are
%           subproeprties of =|rdf:member|=.
axiom(X, rdfs:subPropertyOf, rdfs:member):-
  stmt(M, X, rdf:type, rdfs:'ContainerMembershipProperty').
% [RDFS-10b] Transitivity of subclass relation.
axiom(C1, rdfs:subClassOf, C3):-
  stmt(M, C1, rdfs:subClassOf, C2),
  C1 \== C2,
  stmt(M, C2, rdfs:subClassOf, C3),
  C2 \== C3.
% [RDFS-10a] Reflexivity of subclass relation.
axiom(C, rdfs:subClassOf, C):-
  stmt(M, C, rdf:type, rdfs:'Class').
% [RDFS-9b] Individuals are closed under transitivity of the
%           subclass hierarchy.
axiom(X, rdf:type, C2):-
  stmt(M, C1, rdfs:subClassOf, C2),
  stmt(M, X, rdf:type, C1).
% [RDFS-9a] Resources that occur in the subclass hierarchy are
%           individuals of =|rdfs:'Class'|=.
axiom(C, rdf:type, rdfs:'Class'):-
  stmt(M, C1, rdfs:subClassOf, C2),
  (C = C1 ; C = C2).
% [RDFS-8] Classes are subclasses of =|rdfs:'Resource'|=.
axiom(C, rdfs:subClassOf, rdfs:'Resource'):-
  stmt(M, C, rdf:type, rdfs:'Class').
% [RDFS-7b] Use the subproperty hierarchy to derive an arbitrary triple.
axiom(S, P2, O):-
  stmt(M, P1, rdfs:subPropertyOf, P2),
  stmt(M, S, P1, O).
% [RDFS-7a] Resources that occur in the subproperty relation are
%           individuals of =|rdf:'Property'|=.
axiom(P, rdf:type, rdf:'Property'):-
  stmt(M, P1, rdfs:subPropertyOf, P2),
  (P = P1 ; P = P2).
% [RDFS-6a] Transitivity of subproperty relation.
axiom(X, rdfs:subPropertyOf, Z):-
  stmt(M, X, rdfs:subPropertyOf, Y),
  X \== Y,
  stmt(M, Y, rdfs:subPropertyOf, Z),
  Y \== Z.
% [RDFS-6a] Reflexivity of subproperty relation.
axiom(X, rdfs:subPropertyOf, X):-
  stmt(M, X, rdf:type, rdfs:'Property').
% [RDFS-5] If <P,rdfs:range,R> and <X,P,Y>, then <Y,rdf:type,R>.
axiom(Y, rdf:type, R):-
  stmt(M, P, rdfs:range, R),
  stmt(M, _, P, Y).
% [RDFS-4] If <P,rdfs:domain,D> and <X,P,Y>, then <X,rdf:type,D>.
axiom(X, rdf:type, D):-
  stmt(M, P, rdfs:domain, D),
  stmt(M, X, P, _).
*/



:- begin_tests(rdf_axiom).

:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(test_triple(?,r,r,r)).

%! test_triple(
%!   ?Language:oneof([rdf,rdfs]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

test_triple(rdfs, rdfs:'Resource', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:'Class', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:'Literal', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'XMLLiteral', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:'Datatype', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Seq', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Bag', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Alt', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:'Container', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'List', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:'ContainerMembershipProperty', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Property', rdf:type, rdfs:'Class').
test_triple(rdfs, rdf:'Statement', rdf:type, rdfs:'Class').
test_triple(rdfs, rdfs:domain, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:range, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:subPropertyOf, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:subClassOf, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:member, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:seeAlso, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:isDefinedBy, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:comment, rdf:type, rdf:'Property').
test_triple(rdfs, rdfs:label, rdf:type, rdf:'Property').

:- end_tests(rdf_axiom).

