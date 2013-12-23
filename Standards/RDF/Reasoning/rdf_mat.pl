:- module(
  rdf_mat,
  [
    materialize/2, % +Regimes:list(atom)
                   % +Graph:atom
    regime/1, % ?Regime:atom
    start_materializer/3 % +Regimes:list(atom)
                         % +Graph:atom
                         % +Interval:positive_integer
  ]
).

/** <module> RDF materialization engine

Takes axioms, rules, and the RDF index and performs materializations.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12
*/

:- use_module(doyle(doyle)).
:- use_module(generics(deb_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(
  rdf_reasoning(rdf_ent),
  [
    axiom/4 as rdf_axiom,
    explanation/3 as rdf_explanation,
    regime/2 as rdf_regime,
    rule/7 as rdf_rule
  ]
).
:- use_module(
  rdfs(rdfs_ent),
  [
    axiom/4 as rdfs_axiom,
    explanation/3 as rdfs_explanation,
    regime/2 as rdfs_regime,
    rule/7 as rdfs_rule
  ]
).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).
:- use_module(tms(tms_print)).

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

%! materialize(+Regimes:ordset(atom), ?Graph:atom) is det.
% Performs all depth-one deductions for either the given RDF graph
% or no RDF graph.
%
% If the graph parameter is given, then only triples from that graph
% are considered for materialization.
% The same graph is used for storing the results.
%
% @param Regimes An ordered set of atomic names denoting
%        the entailment regimes that are used by materialization.
% @param Graph Either the atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).

% No materialization whatsoever.
materialize([none], _G):- !.
% Some form of materialization.
materialize(Regimes, G):-
  % The default graph is called `user`.
  % This is also the default graph that rdf/3 write to.
  (nonvar(G), ! ; G = user),

  % Make sure there is a registered TMS that can be used.
  % Otherwise, we have to create a TMS for this purpose.
  % We choose a TMS according to Doyle's orginal specification (classic!).
  atom_concat(tms_, G, TMS),
  (
    tms(TMS), !
  ;
    register_tms(doyle, TMS),
    doyle_reset(TMS),
    doyle_init(TMS)
  ),

  % DEB
  if_debug(rdf_mat, retractall(recent_triple(_,_,_,_))),

  % Let's go!
  materialize(Regimes, TMS, G).

%! materialize(+Regimes:list(atom), +TMS:atom, +Graph:atom) is det.
% The inner loop of materialization.
% This performs all depth-1 reasoning steps (i.e., breadth-first).
%
% # Regime specification
%
% We cannot always assume that a 'lower' regime is applicable,
%  when a 'higher' regime is requested.
% For instance with `[rdf,rdfs]` we do **not** intend to use `se`.
%
% @param Regimes An ordered set of atomic names denoting
%        the entailment regimes that are used for materialization.
% @param TMS The atomic name of a Truth Maintenance System.
% @param Graph The atomic name of a graph.

materialize(Regime, TMS, G):-
  \+ is_list(Regime), !,
  materialize([Regime], TMS, G).
materialize(Regimes, TMS, G):-
  % A deduction of depth one.
  member(RuleRegime, Regimes),
  rule(RuleRegime, Rule, Prems, S, P, O, G),

  % Only accept new stuff.
  \+ rdf(S, P, O, G),

  % Add to TMS.
  maplist(rdf_triple_name([]), [rdf(S,P,O)|Prems], [C_Label|P_Labels]),
  doyle_add_argument(TMS, P_Labels, Rule, C_Label, J),

  % DEB
  if_debug(
    rdf_mat,
    (
      flag(deductions, Id, Id + 1),
      format(user_output, '~w: ', [Id]),
      with_output_to(
        user_output,
        tms_print_justification([indent(0),lang(en)], TMS, J)
      ),
      assert(recent_triple(S, P, O, G))
    )
  ),

  % Store the result.
  rdf_assert(S, P, O, G), !,

  % Look for more results...
  materialize(Regimes, TMS, G).
% Done!
materialize(Regimes, _TMS, _G):-
  % DEB
  with_output_to(atom(RegimesAtom), print_list([], Regimes)),
  if_debug(
    rdf_mat,
    (
      flag(deductions, N, 0),
      debug(rdf_mat, 'Added ~w deductions (regimes: ~w).', [N,RegimesAtom])
    )
  ).

%! regime(?Regime:atom) is nondet.

regime(X):-
  regime(X, _).
regime(X):-
  regime(_, X),
  \+ regime(X, _).

%! regime(?SubsumedRegime:atom, ?SubsumingRegime:atom) is nondet.

regime(none, _).
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
%!   +Regimes:ordset(atom),
%!   +Interval:positive_integer,
%!   +Graph:atom
%! ) is det.
% Performs a depth-one materialization step every N seconds.
%
% @param Regimes An ordered set of atomic names of an entailment regimes.
% @param Interval The number of seconds between consecutive
%        materialization attempts.
% @param Graph The atomic name of a graph
%        or uninstantiated (not restricted to a particular graph).
%
% @see Performs materialization steps using materialize/1.

start_materializer(Regimes, N1, G):-
  default(N1, 30, N2),
  intermittent_thread(materialize(Regimes, G), true, N2, _Id, []),
  debug(rdf_mat, 'A materializer was started on graph ~w.', [G]).

