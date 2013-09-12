:- module(
  rdf_axiom,
  [
    explain_web/4, % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:or([bnode,iri,literal])
                   % -SVG:list
    materialize/1, % +Graph:atom
    start_materializer/2 % +Graph:atom
                         % +Interval:positive_integer
  ]
).

/** <module> RDF AXIOMS

An axiomatic approach towards RDF(S) materialization.

Examples of commands to the Web console:
~~~{.pl}
explain(wikipedia:'Gordian_II',rdf:type,rdfs:'Resource')
explain(schema:'Person',rdfs:subClassOf,schema:'Person')
~~~

@author Wouter Beek
@see Hayes2004, Hitzler2008
@tbd Add the check for well-typed XML literals to rule `rdf2`.
@tbd Store the blank node-to-literal assignment in a format
     that allows faster retrieval, e.g. two ordered sets.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_meta_auto_expand)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_term)).
:- use_module(tms(doyle)).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta_expand(rdf_axiom:explain_web(e,e,e,i)).

:- discontiguous(explanation/2).

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

% The mapping between blank nodes and literals, which is used
% to assert predications of literals, even though literals cannot
% be subject terms according to RDF 1.0 syntax.
:- dynamic(b2l/2).
:- dynamic(l2b/2).

:- debug(rdf_axiom).



% BLANK NODE-TO-LITERAL MAPPING %

%! add_bnode_literal_map(
%!   +Graph:atom,
%!   +BlankNode:bnode,
%!   +Literal:compound
%! ) is det.
% Adds the mapping between a single blank node and a single literal
% to the association lists that are used for storage.

add_bnode_literal_map(G, BNode, Lit):-
  % Add a mapping from blank node to literal.
  retractall(b2l(G, OldB2L)),
  put_assoc(BNode, OldB2L, Lit, NewB2L),
  assert(b2l(G, NewB2L)),

  % Add a mapping from literal to blank node.
  retractall(l2b(G, OldL2B)),
  put_assoc(Lit, OldL2B, BNode, NewL2B),
  assert(l2b(G, NewL2B)).

%! bnode_to_literal(+Graph:atom, +BNode:bnode, -Literal:compound) is semidet.

bnode_to_literal(G, BNode, Lit):-
  b2l(G, A),
  get_assoc(BNode, A, Lit).

%! literal_to_bnode(+Graph:atom, +Literal:compound, -BlankNode:bnode) is det.

literal_to_bnode(G, Lit, BNode):-
  l2b(G, A),
  get_assoc(Lit, A, BNode).



% EXPLANATION %

%! explain_web(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   -SVG:list
%! ) is det.

explain_web(S, P, O, SVG):-
  rdf_triple_name(S, P, O, TripleName),
  tms_export_argument_web(TripleName, SVG).



% MATERIALIZATION %

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
  % The default graph is called `user`.
  (nonvar(G) ; G = user),

  % Make sure there is a registered TMS that can be used.
  atom_concat(tms_, G, TMS),
  (
    is_registered_tms(TMS)
  ;
    register_tms(doyle, TMS),
    doyle_reset(TMS),
    doyle_init(TMS)
  ), !,

  % Create the blank node-to-literal store if it does not already exist.
  once((
    b2l(G, _)
  ;
    retractall(b2l/2),
    empty_assoc(A1),
    assert(b2l(G, A1))
  )),

  % Create the literal-to-blank node store if it does not already exist.
  once((
    l2b(G, _)
  ;
    retractall(l2b/2),
    empty_assoc(A2),
    assert(l2b(G, A2))
  )),

  % Let's go!
  materialize_(G, TMS).

materialize_(G, TMS):-
  % A deduction of depth one.
  rule(R, Prems, S, P, O, G),

  \+ rdf(S, P, O, G),
  % THIS ONE IS NEEDED WHEN BLANK NODES ARE ADDED IN SOME DEDUCTIONS.
  % Only collect new triples, abstracting away from the blank nodes.
  %\+ rdf_find(S, P, O, G),

  % Add to TMS.
  maplist(rdf_triple_name, [rdf(S,P,O)|Prems], [C_Label|P_Labels]),
  doyle_add_argument(TMS, P_Labels, R, C_Label, J),

  % DEB
  flag(deductions, Id, Id + 1),
  format(user_output, '~w: ', [Id]),
  with_output_to(
    user_output,
    tms_print_justification([indent(0),lang(en)], TMS, J)
  ),

  % Store the result.
  rdf_assert(S, P, O, G), !,

  % Look for more results.
  materialize_(G, TMS).
materialize_(_G, _TMS):-
  flag(deductions, N, 0),
  debug(rdf_axiom, 'Added ~w deductions.', [N]).

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

% All axioms can be deduced as if they are the outcome of a rule.
rule(axiom, [], S, P, O, _G):-
  axiom(_Lang, S, P, O).

/*
% [lg]  Literal generalization is a special case of [se1],
%       where the object term is a literal.
%       Literal generalization is used whenever something has to be
%       predicated of a literal (since literals cannot occur
%       as subject terms).
rule(lg, [rdf(S,P,Lit)], S, P, BNode, G):-
  rdf(S, P, Lit, G),
  rdf_is_literal(Lit),
  % If no mapping exists, one is created.
  % Notice that this is the only rule that can add to the
  % blank node-to-literal mapping.
  once((
    literal_to_bnode(G, Lit, BNode)
  ;
    rdf_bnode(BNode),
    add_bnode_literal_map(G, BNode, Lit)
  )).
*/

/*
% [se1] Simple entailment w.r.t. the object term.
rule(se1, [rdf(S,P,O)], S, P, B, G):-
  rdf(S, P, O, G),
  % Constraining the standard.
  rdf_is_iri(O),
  rdf_bnode(B).

% [se2] Simple entailment w.r.t. the subject term.
rule(se2, [rdf(S,P,O)], B, P, O, G):-
  rdf(S, P, O, G),
  % Constraining the standard.
  \+ rdf_is_bnode(S),
  T1 =.. [LastRule|_], LastRule \== se2,
  rdf_bnode(B).
*/

% [rdf1] Predicate terms are instances of =|rdf:'Property'|=.
explanation(
  rdf1,
  'Terms that occur in the predicate position are instances of rdf:Property.'
).
rule(rdf1, [rdf(S,P,O)], P, rdf:type, rdf:'Property', G):-
  rdf(S, P, O, G).

/*
% [rdf2] XML literals are instances of =|rdf:'XMLLiteral'|=.
explanation(rdf2, 'All XML literals are instances of rdf:XMLLiteral.').
rule(rdf2, [rdf(S,P,TypedLit)], BNode, rdf:type, rdf:'XMLLiteral', G):-
  rdf(S, P, TypedLit, G),
  % @tbd This should be a well-typed XML literal...
  rdf_is_literal(TypedLit),
  literal_to_bnode(G, TypedLit, BNode).
*/

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

/*
% [gl] Literal instantiation rule
%      This ensures that every triple that contains a literal and
%      its similar triple that contains the allocated blank node
%      (according to the literal generation rule [lg])
%      are derivable from each other.
rule(gl, [rdf(S,P,BNode)], S, P, Lit, G):-
  rdf(S, P, BNode, G),
  % If the object term is not a blank node,
  % then we do not have to search the blank node-literal mapping.
  rdf_is_bnode(BNode),
  % Not every blank node that is an object term in some triple
  % is a generalization for a literal.
  % Therefore, it has to occur in the mapping established by rule [lg].
  bnode_to_literal(G, BNode, Lit).
*/

/*
% [rdfs1] Literals are instances of =|rdfs:'Literal'|=.
explanation(
  rdfs1,
  'Literal terms belong to the extension of the RDFS literal class.'
).
rule(rdfs1, [rdf(S,P,PlainLit)], BNode, rdf:type, rdfs:'Literal', G):-
  rdf(S, P, PlainLit, G),
  rdf_is_plain_literal(PlainLit),
  literal_to_bnode(G, PlainLit, BNode).
*/

% [rdfs2] Class membership through domain restriction.
rule(rdfs2, [rdf(P,rdfs:domain,C),rdf(S,P,O)], S, rdf:type, C, G):-
  rdf(P, rdfs:domain, C, G),
  rdf_is_iri(P),
  rdf(S, P, O, G).

% [rdfs3] Class membership through range restriction.
rule(rdfs3, [rdf(P,rdfs:range,C),rdf(S,P,O)], O, rdf:type, C, G):-
  rdf(P, rdfs:range, C, G),
  rdf_is_iri(P),
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).

% [rdfs4a] Subject terms are resources.
explanation(
  rdfs4a,
  'Terms that occur in the subject position are RDFS resources.'
).
rule(rdfs4a, [rdf(S,P,O)], S, rdf:type, rdfs:'Resource', G):-
  rdf(S, P, O, G).
% [rdfs4b] Object terms are resources.
explanation(
  rdfs4b,
  'Terms that occur in the object position are RDFS resources.'
).
rule(rdfs4b, [rdf(S,P,O)], O, rdf:type, rdfs:'Resource', G):-
  rdf(S, P, O, G),
  \+ rdf_is_literal(O).

% [rdfs5] Transitive closure of the property hierarchy relation.
rule(rdfs5, Prems, P1, rdfs:subPropertyOf, P3, G):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  % `P2` is automatically constrained to blank nodes and IRIs,
  % since is must appear as the subject term of some triple.
  rdf(P2, rdfs:subPropertyOf, P3, G),
  Prems = [rdf(P1,rdfs:subPropertyOf,P2),rdf(P2,rdfs:subPropertyOf,P3)].

% [rdfs6] Reflexivity of the property hierarchy relation.
rule(rdfs6, [rdf(P,rdf:type,rdf:'Property')], P, rdfs:subPropertyOf, P, G):-
  rdf(P, rdf:type, rdf:'Property', G).

% [rdfs7] Using the property hierarchy.
rule(rdfs7, [rdf(P1,rdfs:subPropertyOf,P2),rdf(S,P1,O)], S, P2, O, G):-
  rdf(P1, rdfs:subPropertyOf, P2, G),
  rdf_is_iri(P1),
  rdf_is_iri(P2),
  rdf(S, P1, O, G).

% [rdfs8] Classes are instances of =|rdfs:Resource|=.
rule(rdfs8, Prems, C, rdfs:subClassOf, rdfs:'Resource', G):-
  rdf(C, rdf:type, rdfs:'Class', G),
  Prems = [rdf(C,rdf:type,rdfs:'Class')].

% [rdfs9] Using the class hierarchy.
rule(rdfs9, Prems, S, rdf:type, C2, G):-
  rdf(C1, rdfs:subClassOf, C2, G),
  % `C1` is automatically constrained to blank nodes and IRIs,
  % since is must have appeared as the subject term of some triple.
  rdf(S, rdf:type, C1, G),
  Prems = [rdf(C1,rdfs:subClassOf,C2),rdf(S,rdf:type,C1)].

% [rdfs10] Reflexivity of the class hierarchy relation.
rule(rdfs10, [rdf(C,rdf:type,rdfs:'Class')], C, rdfs:subClassOf, C, G):-
  rdf(C, rdf:type, rdfs:'Class', G).

% [rdfs11] Transitivity of the class hierarchy relation.
rule(rdfs11, Prems, C1, rdfs:subClassOf, C3, G):-
  rdf(C1, rdfs:subClassOf, C2, G),
  \+ rdf_is_literal(C2),
  rdf(C2, rdfs:subClassOf, C3, G),
  Prems = [rdf(C1,rdfs:subClassOf,C2),rdf(C2,rdfs:subClassOf,C3)].

% [rdfs12]
rule(rdfs12, Prems, S, rdfs:subPropertyOf, rdfs:member, G):-
  rdf(S, rdf:type, rdfs:'ContainerMembershipProperty', G),
  Prems = [rdf(S,rdf:type,rdfs:'ContainerMembershipProperty')].

% [rdfs13]
rule(rdfs13, Prems, S, rdfs:subClassOf, rdfs:'Literal', G):-
  rdf(S, rdf:type, rdfs:'Datatype', G),
  Prems = [rdf(S,rdf:type,rdfs:'Datatype')].



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

