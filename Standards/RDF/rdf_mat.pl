:- module(
  rdf_mat,
  [
    materialize/2, % +Graph:atom
                   % +Regime:atom
    regime/1, % ?Regime:atom
    start_materializer/3 % +Graph:atom
                         % +Regime:atom
                         % +Interval:positive_integer
  ]
).

/** <module> RDF materialization engine

Takes axioms, rules, and the RDF index and performs materializations.

@author Wouter Beek
@version 2013/09
*/

:- use_module(tms(doyle)).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).
:- use_module(generics(thread_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(
  rdf(rdf_ent),
  [
    axiom/4 as rdf_axiom,
    explanation/3 as rdf_explanation,
    regime/2 as rdf_regime,
    rule/7 as rdf_rule
  ]
).
:- use_module(rdf(rdf_name)).
:- use_module(
  rdfs(rdfs_ent),
  [
    axiom/4 as rdfs_axiom,
    explanation/3 as rdfs_explanation,
    regime/2 as rdfs_regime,
    rule/7 as rdfs_rule
  ]
).

:- dynamic(recent_triple/4).

:- debug(rdf_mat).



%! axiom(
%!   ?Regime:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

axiom(Regime, S, P, O):-
  rdf_axiom(Regime, S, P, O).
axiom(Regime, S, P, O):-
  rdfs_axiom(Regime, S, P, O).

explanation(Regime, Rule, Explanation):-
  rdf_explanation(Regime, Rule, Explanation).
explanation(Regime, Rule, Explanation):-
  rdfs_explanation(Regime, Rule, Explanation).

%! materialize(?Graph:atom, +Regimes:ordset(atom)) is det.
% Performs all depth-one deductions for either the given RDF graph
% or no RDF graph.
%
% If the graph parameter is given, then only triples from that graph
% are considered for materialization.
% The same graph is used for storing the results.
%
% @param Graph Either the atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).
% @param Regimes An ordered set of atomic names denoting
%        the entailment regimes that are used by materialization.

materialize(G, Regimes):-
  % The default graph is called `user`.
  % This is also the default graph that rdf/3 write to.
  (nonvar(G) ; G = user),

  % Make sure there is a registered TMS that can be used.
  % Otherwise, we have to create a TMS for this purpose.
  % We choose a TMS according to Doyle's orginal specification (classic!).
  atom_concat(tms_, G, TMS),
  (
    is_registered_tms(TMS), !
  ;
    register_tms(doyle, TMS),
    doyle_reset(TMS),
    doyle_init(TMS)
  ),

  % DEB
  (  debug(rdf_mat)
  -> retractall(recent_triple(_,_,_,_))
  ;  true),

  % Let's go!
  materialize(G, Regimes, TMS).

%! materialize(+Graph:atom, +Regime:atom, +TMS:atom) is det.
% The inner loop of materialization.
% This performs all depth-1 reasoning steps (i.e., breadth-first).
%
% @param Graph The atomic name of a graph.
% @param Regime An atomic names denoting the entailment regime
%        that is used for materialization.
% @param TMS The atomic name of a Truth Maintenance System.

materialize(G, Regime, TMS):-
  % A deduction of depth one.
  rule(RuleRegime, Rule, Prems, S, P, O, G),

  % A rule applies if one of its regimes is in the set of regimes
  % that we materialize for now.
  once(regime(RuleRegime, Regime)),

  % Only accept new stuff.
  \+ rdf(S, P, O, G),
gtrace,
  % Add to TMS.
  maplist(rdf_triple_name([]), [rdf(S,P,O)|Prems], [C_Label|P_Labels]),
  doyle_add_argument(TMS, P_Labels, Rule, C_Label, J),

  % DEB
  (  debug(rdf_mat)
  -> flag(deductions, Id, Id + 1),
     format(user_output, '~w: ', [Id]),
     with_output_to(
       user_output,
       tms_print_justification([indent(0),lang(en)], TMS, J)
     ),
     assert(recent_triple(S, P, O, G))
  ;  true),

  % Store the result.
  rdf_assert(S, P, O, G), !,

  % Look for more results...
  materialize(G, Regime, TMS).
% Done!
materialize(_G, _Regime, _TMS):-
  % DEB
  (  debug(rdf_mat)
  -> flag(deductions, N, 0),
     debug(rdf_axiom, 'Added ~w deductions.', [N])
  ;  true).

%! regime(?Regime:atom) is nondet.

regime(X):-
  regime(X, _).
regime(X):-
  regime(_, X),
  \+ regime(X, _).

%! regime(?SubsumedRegime:atom, ?SubsumingRegime:atom) is nondet.

regime(X, X):-
  nonvar(X).
regime(X, Y):-
  rdf_regime(X, Y).
regime(X, Y):-
  rdfs_regime(X, Y).
regime(X, Y):-
   nonvar(Y),
   regime(X, Z),
   X \== Z,
   Z \== Y,
   regime(Z, Y).

% RDF subsumes simple entailment.
regime(se, rdf).
% RDFS subsumes RDF.
regime(rdf, rdfs).

%! rule(
%!   ?Regime:atom,
%!   ?Rule:atom,
%!   ?Premises:list(compound),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is nondet.

% All axioms can be deduced as if they are the outcome of a rule.
rule(Regime, axiom, [], S, P, O, _G):-
  axiom(Regime, S, P, O).
rule(Regime, Rule, Premises, S, P, O, G):-
  rdf_rule(Regime, Rule, Premises, S, P, O, G).
rule(Regime, Rule, Premises, S, P, O, G):-
  rdfs_rule(Regime, Rule, Premises, S, P, O, G).

%! start_materializer(
%!   +Graph:atom,
%!   +Regime:atom,
%!   +Interval:positive_integer
%! ) is det.
% Performs a depth-one materialization step every N seconds.
%
% @param Graph The atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).
% @param Regime The atomic name of an entailment regime.
% @param Interval The number of seconds between consecutive
%        materialization attempts.
%
% @see Performs materialization steps using materialize/1.

start_materializer(G, Regime, N1):-
  default(N1, 30, N2),
  intermittent_thread(materialize(G, Regime), N2, _Id, []),
  debug(mat, 'A materializer was started on graph ~w.', [G]).

