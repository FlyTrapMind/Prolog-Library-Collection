:- module(
  tms,
  [
% TMS EXPLANATIONS
    tms_argument/2, % +Node:iri
                    % -Argument:ordset(iri)

% TMS REGISTRATION
    deregister_tms/2, % +Type:atom
                      % +TMS:atom
    is_registered_tms/1, % +TMS:atom
    register_tms/2, % +Type:atom
                    % +TMS:atom
    registered_tms/2, % ?Type:atom
                      % ?TMS:atom

% GENERIC TMS PREDICATES
    is_in_node/2, % +TMS:atom
                  % +Node:iri
    is_justification/2, % +TMS:atom
                        % +Justification:iri
    is_node/2, % +TMS:atom
               % +Node:iri
    is_out_node/2, % +TMS:atom
                   % +Node:iri
    tms_create_node_iri/2, % +Label:atom
                           % -Node:iri
    tms_create_justification_iri/5, % +InNodes:list(iri)
                                    % +OutNodes:list(iri)
                                    % +Label:atom
                                    % +Consequence:iri
                                    % -Justification:iri
    tms_justification/2, % +TMS:atom
                         % -Justification:iri
    tms_justification/5, % ?TMS:atom
                         % ?Antecedents:list(iri)
                         % ?Rule:atom
                         % ?Consequent:iri
                         % ?Justification:iri
    tms_node/2 % +TMS:atom
               % -Node:iri
  ]
).

/** <module> TMS

The generic predicates for Truth-Maintenance Systems.

@author Wouter Beek
@version 2013/05, 2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_search)).
:- use_module(rdfs(rdfs_build)).
:- use_module(tms(doyle)).
:- use_module(tms(tms_export)).
:- use_module(xml(xml_namespace)).

:- dynamic(registered_tms/2).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_argument(r,-)).
:- rdf_meta(tms_create_justification_iri(+,+,+,r,-)).
:- rdf_meta(tms_justification(?,r)).
:- rdf_meta(tms_justification(?,?,?,r,r)).



% TMS EXPLANATIONS %

tms_argument(N, Argument):-
  rdf_breadth_first(
    N,
    tms:has_consequent,
    tms:has_antecedent,
    _Ns,
    Argument
  ).



% TMS REGISTRATION %

deregister_tms(Type, TMS):-
  retractall(registered_tms(Type, TMS)).

generic_tms(TMS, Pred, Args):-
  once(registered_tms(Type, TMS)),
  generic(Pred, Type, Args).

is_registered_tms(TMS):-
  nonvar(TMS),
  once(registered_tms(_Type, TMS)).

register_tms(Type, TMS):-
  db_add_novel(registered_tms(Type, TMS)).



% GENERIC TMS PREDICATES

is_in_node(TMS, Node):-
  generic_tms(TMS, is_in_node, [Node]).

is_justification(TMS, Justification):-
  is_registered_tms(TMS),
  nonvar(Justification),
  once(tms_justification(TMS, Justification)).

is_node(TMS, Node):-
  is_registered_tms(TMS),
  nonvar(Node),
  once(tms_node(TMS, Node)).

is_out_node(TMS, Node):-
  generic_tms(TMS, is_out_node, [Node]).

tms_create_node_iri(Label, N):-
  variant_sha1(n(Label), Id),
  rdf_global_id(doyle:Id, N).

tms_create_justification_iri(InNs, OutNs, L, C, J):-
  variant_sha1(j(InNs,OutNs,L,C), Id),
  rdf_global_id(doyle:Id, J).

tms_init(TMS):-
  atom(TMS),
  rdfs_assert_subproperty(tms:has_in, tms:has_antecedent, TMS),
  rdfs_assert_subproperty(tms:has_out, tms:has_antecedent, TMS).

tms_justification(TMS, Justification):-
  rdfs_individual_of(Justification, tms:'Justification'),
  rdf(Justification, _, _, TMS:_).

tms_justification(TMS, As, R, C, J):-
  tms_justification(TMS, J),
  findall(A, rdf_has(J, tms:has_antecedent, A), As),
  rdfs_label(J, R),
  rdf(J, tms:has_consequent, C, TMS).

tms_node(TMS, Node):-
  rdfs_individual_of(Node, tms:'Node'),
  rdf(Node, _, _, TMS:_).

