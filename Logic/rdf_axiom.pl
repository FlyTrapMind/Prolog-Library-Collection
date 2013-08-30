:- module(
  rdf_axioms,
  [
    axiom/4, % ?Language:oneof([rdf,rdfs])
             % ?Subject:or([bnode,iri])
             % ?Predicate:iri
             % ?Object:or([bnode,literal,iri])
    bnode_literal_map/2, % ?BNode:bnode
                         % ?Literal:compound
    materialize/0
    %query/3 % ?Subject:or([bnode,iri])
    %        % ?Predicate:iri
    %        % ?Object:or([bnode,literal,iri])
  ]
).

/** <module> RDF AXIOMS

An axiomatic approach towards RDF(S) materialization.

@author Wouter Beek
@see Hayes2004
@tbd Use a CPS for calculating the deductive closure and individual queries.
@version 2013/05, 2013/08
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_name)).
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
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?History:list(atom)
%! ) is nondet.

:- discontiguous(rule/6).
:- rdf_meta(rule(-,-,r,r,r,?)).

%! stmt(
%!   -Tree:compound,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?MaxDepth:positive_integer
%! ) is nondet.

:- discontiguous(stmt/5).
:- rdf_meta(stmt(-,r,r,r,?)).

:- debug(rdf_axiom).



%! bnode_literal_map(?BNode:bnode, ?Literal:literal) is nondet.
% The mapping between blank nodes and literals, which is used
% to assert predications of literals, even though literals cannot
% be subject terms according to RDF 1.0 syntax.

bnode_literal_map(BNode, Literal):-
  db_add_novel(bnode_literal_map_(BNode, Literal)).

rdf_bnode_to_var(X, _):-
  rdf_is_bnode(X), !.
rdf_bnode_to_var(X, X).

rdf_both_bnode(X, Y):-
  rdf_is_bnode(X), !,
  rdf_is_bnode(Y).
rdf_both_bnode(_, _).

alread_in_rdf(S, P, O):-
  maplist(rdf_bnode_to_var, [S,P,O], [SS,PP,OO]),
  rdf(SS, PP, OO),
  rdf_both_bnode([S,P,O], [SS,PP,OO]).

materialize:-
  % Notice that the depth setting constrains the deductions
  % to direct rule applications.
  rule(Tree, S, P, O, 10),
  % We only collect new triples, abstracting the blank nodes.
  \+ alread_in_rdf(S, P, O),
  flag(deductions, Id, Id + 1),
  format(user_output, '~w: ', [Id]),
  print_proof(user_output, Tree),
  rdf_assert(S, P, O),
  !,
  materialize.
materialize:-
  flag(deductions, _OldId, 0).

/*
query(S, P, O):-
  query(T0, S, P, O),
  print_proof(user_output, T0).

% Explicitly asserted.
query(fact([],TripleName), S, P, O):-
  rdf(S, P, O),
  rdf_triple_name(S, P, O, TripleName).
% Derived using rules and axioms.
query(Tree, S, P, O):-
  rule(Tree, S, P, O, 0).
*/

%! rule(
%!   -Tree:compound,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?MaxDepth:positive_integer
%! ) is nondet.

rule(Tree, S, P, O, D):-
  rule(Rule, Premises, S, P, O, D),
  rdf_triple_name(S, P, O, Conclusion),
  Tree =.. [Rule,Premises,Conclusion].

% All axioms can be deduced as if they are the outcome of a rule.
rule(axiom, [], S, P, O, _D):-
  axiom(_Lang, S, P, O).

% Statements occur inside rules (as premises).
% We want to perform materialization in individual steps of depth 1.
% This means that for materialization (mode `m`) we do not use rules.
stmt(fact([],TripleName), S, P, O, _D):-
  rdf(S, P, O),
  rdf_triple_name(S, P, O, TripleName).
/*
% Since queries can traverse the search space until arbitrary depth,
% they can use rules a predicates of rules.
stmt(Tree, S, P, O, D1):-
  D1 =< 3, !, D2 is D1 + 1,
  rule(Tree, S, P, O, D2).
*/



% RDF RULES & AXIOMS %

% [se1] Simple entailment w.r.t. the object term.
% [lg]  Literal generalization is a special case of [se1],
%       where the object term is a literal.
%       Literal generalization is used whenever something has to be
%       predicated of a literal (since literals cannot occur
%       as subject terms).
rule(se1, [T1], S, P, B, D):-
  stmt(T1, S, P, O, D),
  % Constraining the standard.
  \+ rdf_is_bnode(O),
  (
    rdf_is_literal(O)
  ->
    bnode_literal_map(B, O)
  ;
    rdf_bnode(B)
  ).

% [se2] Simple entailment w.r.t. the subject term.
rule(se2, [T1], B, P, O, D):-
  stmt(T1, S, P, O, D),
  % Constraining the standard.
  \+ rdf_is_bnode(S),
  T1 =.. [LastRule|_], LastRule \== se2,
  rdf_bnode(B).

% [rdf1] Predicate terms are instances of =|rdf:'Property'|=.
rule(rdf1, [T1], P, rdf:type, rdf:'Property', D):-
  stmt(T1, _, P, _, D).

% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'|=.
rule(rdf2, [T1], BNode, rdf:type, rdf:'XMLLiteral', D):-
  rdf_global_id(rdf:'XMLLiteral', XMLLiteralType),
  TypedLiteral = literal(type(XMLLiteralType,_)),
  stmt(T1, _, _, TypedLiteral, D),
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
rule(gl, [T1], S, P, Literal, D):-
  stmt(T1, S, P, O, D),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(O),
  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_literal_map(O, Literal).

% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.
rule(rdfs1, [T1], BNode, rdf:type, rdfs:'Literal', D):-
  stmt(T1, _, _, O, D),
  rdf_is_literal(O),
  bnode_literal_map(BNode, O).

% [rdfs2]
rule(rdfs2, [T1,T2], S, rdf:type, C, D):-
  stmt(T1, P, rdfs:domain, C, D),
  stmt(T2, S, P, _, D).

% [rdfs3]
rule(rdfs3, [T1,T2], O, rdf:type, C, D):-
  stmt(T1, P, rdfs:range, C, D),
  stmt(T2, _, P, O, D).

% [rdfs4a]
rule(rdfs4a, [T1], S, rdf:type, rdfs:'Resource', D):-
  stmt(T1, S, _, _, D).
% [rdfs4b]
rule(rdfs4b, [T1], O, rdf:type, rdfs:'Resource', D):-
  stmt(T1, _, _, O, D).

% [rdfs5] Transitive closure of the property hierarchy relation.
rule(rdfs5, [T1,T2], P1, rdfs:subPropertyOf, P3, D):-
  stmt(T1, P1, rdfs:subPropertyOf, P2, D),
  P1 \== P2,
  stmt(T2, P2, rdfs:subPropertyOf, P3, D),
  P2 \== P3.

% [rdfs6] Reflexivity of the property hierarchy relation.
rule(rdfs6, [T1], P, rdfs:subPropertyOf, P, D):-
  stmt(T1, P, rdf:type, rdf:'Property', D).

% [rdfs7] Using the property hierarchy.
rule(rdfs7, [T1,T2], S, P2, O, D):-
  stmt(T1, P1, rdfs:subPropertyOf, P2, D),
  P1 \== P2,
  stmt(T2, S, P1, O, D).

% [rdfs8] Classes are instances of =|rdfs:Resource|=.
rule(rdfs8, [T1], C, rdfs:subClassOf, rdfs:'Resource', D):-
  stmt(T1, C, rdf:type, rdfs:'Class', D).

% [rdfs9] Using the class hierarchy.
rule(rdfs9, [T1,T2], S, rdf:type, C2, D):-
  stmt(T1, C1, rdfs:subClassOf, C2, D),
  C1 \== C2,
  stmt(T2, S,  rdf:type, C1, D).

% [rdfs10] Reflexivity of the class hierarchy relation.
rule(rdfs10, [T1], C, rdfs:subClassOf, C, D):-
  stmt(T1, C, rdf:type, rdfs:'Class', D).

% [rdfs11] Transitivity of the class hierarchy relation.
rule(rdfs11, [T1,T2], C1, rdfs:subClassOf, C3, D):-
  stmt(T1, C1, rdfs:subClassOf, C2, D),
  C1 \== C2,
  stmt(T2, C2, rdfs:subClassOf, C3, D),
  C2 \== C3.

% [rdfs12]
rule(rdfs12, [T1], S, rdf:subClassOf, rdfs:member, D):-
  stmt(T1, S, rdf:type, rdfs:'ContainerMembershipProperty', D).

% [rdfs13]
rule(rdfs13, [T1], S, rdfs:subClassOf, rdfs:'Literal', D):-
  stmt(T1, S, rdf:type, rdfs:'Datatype', D).



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

