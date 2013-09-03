:- module(
  rdf_axiom,
  [
    bnode_literal_map/3, % ?Graph:atom
                         % ?BNode:bnode
                         % ?Literal:compound
    materialize/1, % +Graph:atom
    start_materializer/2 % +Graph:atom
                         % +Interval:positive_integer
  ]
).

/** <module> RDF AXIOMS

An axiomatic approach towards RDF(S) materialization.

@author Wouter Beek
@see Hayes2004, Hitzler2008
@tbd Add the check for well-typed XML literals to rule `rdf2`.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_term)).
:- use_module(tms(doyle)).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- dynamic(bnode_literal_map_/3).

%! axiom(
%!   ?Language:oneof([rdf,rdfs]),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

:- discontiguous(axiom/4).
:- rdf_meta(axiom(?,r,r,r)).

%! rule(M,
%!   -Rule:atom,
%!   -Premises:list(triple),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is nondet.

:- discontiguous(rule/6).
:- rdf_meta(rule(-,-,r,r,r,?)).

:- rdf_meta(stmt(-,r,r,r,?)).

:- debug(rdf_axiom).



%! bnode_literal_map(?Graph:atom, ?BNode:bnode, ?Literal:literal) is nondet.
% The mapping between blank nodes and literals, which is used
% to assert predications of literals, even though literals cannot
% be subject terms according to RDF 1.0 syntax.
%
% @param Graph If no graph is given, then `user` is used instead.
% @param BNode
% @param Literal

bnode_literal_map(G1, BNode, Lit):-
  default(G1, user, G2),
  (
    bnode_literal_map_(G1,BNode,Lit)
  ;
    db_add_novel(bnode_literal_map_(G2,BNode,Lit))
  ), !.

init_materialization(G, TMS):-
  atom_concat(tms_, G, TMS),
  (
    is_registered_tms(TMS)
  ;
    register_tms(doyle, TMS),
    doyle_reset(TMS),
    doyle_init(TMS)
  ), !.

%! materialize(?Graph:atom) is det.
% Performs all depth-one deductions for either the given graph or no graph.
%
% If the graph parameter is given, then only triples from that graph
% are considered for materialization.
% The same graph is used for storing the results.
%
% @param Graph The atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).

materialize(G):-
  (nonvar(G) ; G = user),

  % A deduction of depth one.
  rule(R, Premises, S, P, O, G),
  % Only collect new triples, abstracting away from the blank nodes.
  \+ rdf_find(S, P, O, G),

  % Add to TMS.
  init_materialization(G, TMS),
  rdf_triple_name(S, P, O, Conclusion),
  doyle_add_argument(TMS, Premises, R, Conclusion, J),

  % DEB
  flag(deductions, Id, Id + 1),
  format(user_output, '~w: ', [Id]),
  tms_print_justification([indent(0),lang(en)], TMS, J),

  % Store the result.
  rdf_assert(S, P, O, G), !,
  % Look for more results.
  materialize(G).
materialize(_G):-
  flag(deductions, N, 0),
  debug(rdf_axiom, 'Added ~w deductions.', [N]).

% All axioms can be deduced as if they are the outcome of a rule.
rule(axiom, [], S, P, O, _G):-
  axiom(_Lang, S, P, O).

% Statements occur inside rules (as premises).
% We want to perform materialization in individual steps of depth 1.
% This means that for materialization (mode `m`) we do not use rules.
stmt(TripleName, S, P, O, G):-
  rdf(S, P, O, G),
  rdf_triple_name(S, P, O, TripleName).

%! start_materializer(?Graph:atom, +Interval:positive_integer) is det.
% Performs a depth-one materialization step every N seconds.
%
% @param Graph The atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).
% @param Interval The number of seconds between consecutive
%        materialization attempts.
%
% @see Performs materialization steps using materialize/1.

start_materializer(G, I1):-
  default(I1, 30, I2),
  intermittent_thread(materialize(G), I2, _Id, []),
  debug(rdf_axiom, 'A materializer was started on graph ~w.', [G]).



% RDF RULES & AXIOMS %

% [lg]  Literal generalization is a special case of [se1],
%       where the object term is a literal.
%       Literal generalization is used whenever something has to be
%       predicated of a literal (since literals cannot occur
%       as subject terms).
rule(lg, [T1], S, P, B, G):-
  stmt(T1, S, P, O, G),
  rdf_is_literal(O),
  bnode_literal_map(G, B, O).

/*% [se1] Simple entailment w.r.t. the object term.
rule(se1, [T1], S, P, B, G):-
  stmt(T1, S, P, O, G),
  % Constraining the standard.
  rdf_is_iri(O),
  rdf_bnode(B).*/

/*% [se2] Simple entailment w.r.t. the subject term.
rule(se2, [T1], B, P, O, G):-
  stmt(T1, S, P, O, G),
  % Constraining the standard.
  \+ rdf_is_bnode(S),
  T1 =.. [LastRule|_], LastRule \== se2,
  rdf_bnode(B).*/

% [rdf1] Predicate terms are instances of =|rdf:'Property'|=.
rule(rdf1, [T1], P, rdf:type, rdf:'Property', G):-
  stmt(T1, _, P, _, G).

% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'|=.
rule(rdf2, [T1], BNode, rdf:type, rdf:'XMLLiteral', G):-
  stmt(T1, _, _, TypedLiteral, G),
  % This should be a well-typed XML literal...
  rdf_is_literal(TypedLiteral),
  bnode_literal_map(G, BNode, TypedLiteral).

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
rule(gl, [T1], S, P, Literal, G):-
  stmt(T1, S, P, O, G),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(O),
  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_literal_map(G, O, Literal).

% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.
rule(rdfs1, [T1], BNode, rdf:type, rdfs:'Literal', G):-
  stmt(T1, _, _, O, G),
  rdf_is_literal(O),
  bnode_literal_map(G, BNode, O).

% [rdfs2] Class membership through domain restriction.
rule(rdfs2, [T1,T2], S, rdf:type, C, G):-
  stmt(T1, P, rdfs:domain, C, G),
  rdf_is_iri(P),
  stmt(T2, S, P, _, G).

% [rdfs3] Class membership through range restriction.
rule(rdfs3, [T1,T2], O, rdf:type, C, G):-
  stmt(T1, P, rdfs:range, C, G),
  rdf_is_iri(P),
  stmt(T2, _, P, O, G),
  \+ rdf_is_literal(O).

% [rdfs4a] Subject terms are resources.
rule(rdfs4a, [T1], S, rdf:type, rdfs:'Resource', G):-
  stmt(T1, S, _, _, G).
% [rdfs4b] Object terms are resources.
rule(rdfs4b, [T1], O, rdf:type, rdfs:'Resource', G):-
  stmt(T1, _, _, O, G),
  \+ rdf_is_literal(O).

% [rdfs5] Transitive closure of the property hierarchy relation.
rule(rdfs5, [T1,T2], P1, rdfs:subPropertyOf, P3, G):-
  stmt(T1, P1, rdfs:subPropertyOf, P2, G),
  % `P2` is automatically constrained to blank nodes and IRIs,
  % since is must appear as the subject term of some triple.
  stmt(T2, P2, rdfs:subPropertyOf, P3, G).

% [rdfs6] Reflexivity of the property hierarchy relation.
rule(rdfs6, [T1], P, rdfs:subPropertyOf, P, G):-
  stmt(T1, P, rdf:type, rdf:'Property', G).

% [rdfs7] Using the property hierarchy.
rule(rdfs7, [T1,T2], S, P2, O, G):-
  stmt(T1, P1, rdfs:subPropertyOf, P2, G),
  rdf_is_iri(P1),
  rdf_is_iri(P2),
  stmt(T2, S, P1, O, G).

% [rdfs8] Classes are instances of =|rdfs:Resource|=.
rule(rdfs8, [T1], C, rdfs:subClassOf, rdfs:'Resource', G):-
  stmt(T1, C, rdf:type, rdfs:'Class', G).

% [rdfs9] Using the class hierarchy.
rule(rdfs9, [T1,T2], S, rdf:type, C2, G):-
  stmt(T1, C1, rdfs:subClassOf, C2, G),
  % `C1` is automatically constrained to blank nodes and IRIs,
  % since is must have appeared as the subject term of some triple.
  stmt(T2, S,  rdf:type, C1, G).

% [rdfs10] Reflexivity of the class hierarchy relation.
rule(rdfs10, [T1], C, rdfs:subClassOf, C, G):-
  stmt(T1, C, rdf:type, rdfs:'Class', G).

% [rdfs11] Transitivity of the class hierarchy relation.
rule(rdfs11, [T1,T2], C1, rdfs:subClassOf, C3, G):-
  stmt(T1, C1, rdfs:subClassOf, C2, G),
  \+ rdf_is_literal(C2),
  stmt(T2, C2, rdfs:subClassOf, C3, G).

% [rdfs12]
rule(rdfs12, [T1], S, rdfs:subPropertyOf, rdfs:member, G):-
  stmt(T1, S, rdf:type, rdfs:'ContainerMembershipProperty', G).

% [rdfs13]
rule(rdfs13, [T1], S, rdfs:subClassOf, rdfs:'Literal', G):-
  stmt(T1, S, rdf:type, rdfs:'Datatype', G).



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

