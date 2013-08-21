:- module(
  rdf_entails,
  [
    rdf_entails/5, % ?S:uri
                   % ?P:uri
                   % ?O:uri
                   % ?G:atom
                   % -Proof:compound
    rdf_entails/7, % ?S:uri
                   % ?P:uri
                   % ?O:uri
                   % ?G:atom
                   % +MaxD:integer
                   % -D:integer
                   % -Proof:compound
    rdf_entails_dev/4, % ?S:uri
                       % ?P:uri
                       % ?O:uri
                       % ?G:atom
    rdf_entails_dev/5 % ?S:uri
                      % ?P:uri
                      % ?O:uri
                      % ?G:atom
                      % +MaxD:integer
  ]
).

/** <module> RDF entails

Entailment regime for RDF(S) and OWL.

@author Wouter Beek
@version 2013/01
*/

:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(options)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_name)).
:- use_module(rdfs(rdfs_read)).

:- rdf_meta(rdf_entails(r,r,r,?,-)).
:- rdf_meta(rdf_entails(r,r,r,?,+,-,-)).
:- rdf_meta(rdf_entails_(r,r,r,?,+,+,-,-)).
:- rdf_meta(rdf_entails_dev(r,r,r,?)).
:- rdf_meta(rdf_entails_dev(r,r,r,?,+)).



%! rdf_entails(?S:uri, ?P:uri, ?O:uri, ?G:atom, -Proof:compound) is nondet.
% @see rdf_entails/7

rdf_entails(S, P, O, G, Proof):-
  rdf_entails(S, P, O, G, infinite, _D, Proof).

%! rdf_entails(?S:uri, ?P:uri, ?O:uri, ?G:atom, +MaxD:integer,
%!   -D:integer, -Proof:compound
%! ) is nondet.
%
% @param S The subject resource.
% @param P The predicate resource.
% @param O The object resource.
% @param G The atomic name of a graph.
% @param MaxD An integer or literal `infinite` indicating
%        the maximum depth at which predicates are searched
%        to fit into the proof.
% @param D An integer indicating the highest depth at which predicates were
%        searched for the proof.
% @param Proof A compound term of the form =|proof(Conclusion, Predicates)|=
%        representing a proof.

rdf_entails(S, P, O, G, MaxD, D, Proof):-
  % Type check on maximum depth and depth parameters.
  (
    MaxD == infinite, !
  ;
    must_be(positive_integer, MaxD)
  ),
  var(D), !,
  rdf_entails_(S, P, O, G, MaxD, 0, D, Proof).

%! rdf_entails_(S, P, O, G, MaxD, MaxD, _D, _Proof) is nondet.
% Keeps track of the depth of the traversed proof space, ensuring this does not
% exceed the maximum depth.
%
% @param S The subject resource.
% @param P The predicate resource.
% @param O The object resource.
% @param G The atomic name of a graph.
% @param MaxD An integer indicating the maximum depth at which predicates
%        are searched to fit into the proof.
% @param D0 Counter, starting at 0, should not reach =MaxD=.
% @param D The detph of =Proof=.
% @param Proof A compound term of the form =|proof(Conclusion, Predicates)|=
%        representing a proof.

% The maximum depth has been reached.
rdf_entails_(_S, _P, _O, _G, MaxD, MaxD, _D, _Proof):- !,
  fail.
% Basic relation.
rdf_entails_(S, P, O, G, _MaxD, D, D, proof(rdf(S, P, O), [])):-
  rdf(S, P, O, G),
  rdf_is_resource(O).
% Instance-of relation.
rdf_entails_(S, Type, C, G, MaxD, D0,
  D, proof(rdf(S, Type, C), [proof(rdf(S, Type, C0), []), Proofs])
):-
  rdf_global_id(rdf:type, Type),
  rdfs_individual_of(S, C0),
  succ(D0, D1),
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdf_entails_(C0, SubClassOf, C, G, MaxD, D1, D, Proofs).
% Subclass relation.
rdf_entails_(S, SubClassOf, O, G, MaxD, D0,
  D, proof(rdf(S, SubClassOf, O), [proof(rdf(S, SubClassOf, S0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(S, S0),
  succ(D0, D1),
  rdf_entails_(S0, SubClassOf, O, G, MaxD, D1, D, Proofs).
/*
% Instance-of relation.
rdf_entails_(S, R, C0, G, MaxD, D0,
  D, proof(rdf(S, R, C0), [proof(rdf(S, R, C), []), Proofs])
):-
  rdf_global_id(rdf:type, R),
  rdfs_individual_of(S, C),
  succ(D0, D1),
  rdf_entails_(C, rdfs:subClassOf, C0, G, MaxD, D1, D, Proofs).
% Subproperty relation.
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(P, SubPropertyOf, P0), []), Proofs])
):-
  rdf_global_id(rdfs:subPropertyOf, SubPropertyOf),
  rdfs_subproperty_of(P, P0),
  succ(D0, D1),
  rdf_entails_(S, P0, O, G, MaxD, D1, D, Proofs).
% Subclass relation.
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, SubClassOf, S0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(S, S0),
  succ(D0, D1),
  rdf_entails_(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails_(S, P, O, _G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, SubClassOf, O0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(O, O0),
  succ(D0, D1),
  rdf_entails_(S, P, O0, G, MaxD, D1, D, Proofs).
% Identity relation.
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, SameAs, S0), []), Proofs])
):-
  rdf_global_id(owl:sameAs, SameAs),
  owl_resource_identity(S, S0),
  succ(D0, D1),
  rdf_entails_(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, SameAs, O0), []), Proofs])
):-
  rdf_global_id(owl:sameAs, SameAs),
  owl_resource_identity(O, O0),
  succ(D0, D1),
  rdf_entails_(S, P, O0, G, MaxD, D1, D, Proofs).
% Equivalence relation.
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, EquivalentClass, S0), []), Proofs])
):-
  rdf_global_id(owl:equivalentClass, EquivalentClass),
  owl_class_equivalence(S, S0),
  succ(D0, D1),
  rdf_entails_(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails_(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, EquivalentClass, O0), []), Proofs])
):-
  rdf_global_id(owl:equivalentClass, EquivalentClass),
  owl_class_equivalence(O, O0),
  succ(D0, D1),
  rdf_entails_(S, P, O0, G, MaxD, D1, D, Proofs).
*/

%! rdf_entails_dev(?S:uri, ?P:uri, ?O:uri, ?G:atom) is nondet.
% @see rdf_entails/7

rdf_entails_dev(S, P, O, G):-
  rdf_entails_dev(S, P, O, G, infinite).

%! rdf_entails_dev(?S:uri, ?P:uri, ?O:uri, ?G:atom, +MaxD:integer) is nondet.
% @see rdf_entails/7

rdf_entails_dev(S, P, O, G, MaxD):-
  rdf_entails(S, P, O, G, MaxD, D, Proof),
  debug(proof, '~w', [Proof]),
  print_proof(user, [depth(D), indent(0), max_depth(MaxD)], [Proof]).

% @tbd The predicates that appear below should be unified with some RDF module
%      used for exporting triples and with some TMS module used for exporting
%      justification chains.

print_proposition(Stream, Options, rdf(S, P, O)):-
  maplist(rdf_term_name(Options), [S, P, O], [S0, P0, O0]),
  option(indent(Indent), Options, 0),
  option(index(Index), Options, 'c'),
  indent(Stream, Indent),
  format(Stream, '[~w] ~w ~w ~w\n', [Index, S0, P0, O0]).

print_proposition0(Stream, Options, Proposition):-
  print_proposition(Stream, Options, Proposition), !.
print_proposition0(Stream, Options, Proposition):-
  option(indent(Indent), Options, 0),
  option(index(Index), Options, c),
  indent(Stream, Indent),
  format(Stream, '[~w]:\t~w', [Index, Proposition]).

print_proof(_Stream, _Options, []).
print_proof(Stream, Options, [proof(Conclusion, Premises) | Proofs]):-
  print_proposition0(Stream, Options, Conclusion),
  select_option(indent(Indent), Options, Options0),
  succ(Indent, NewIndent),
  select_option(index(Index), Options0, Options1, 1),
  succ(Index, NewIndex),
  print_proof(Stream, [indent(NewIndent), index(1) | Options1], Premises),
  print_proof(Stream, [indent(Indent), index(NewIndex) | Options1], Proofs).

