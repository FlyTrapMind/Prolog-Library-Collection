:- module(
  tms,
  [
    deregister_tms/2, % +Type:atom
                      % +TMS:atom
    is_in_node/2, % +TMS:atom
                  % +Node:iri
    is_out_node/2, % +TMS:atom
                   % +Node:iri
    register_tms/2, % +Type:atom
                    % +TMS:atom
    tms/1, % ?TMS:atom
    tms/2, % ?Type:atom
           % ?TMS:atom
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
    tms_justifications/3, % +Options:list(nvpair)
                          % +TMS:atom
                          % -Justification:iri
    tms_leaf_node/2, % ?TMS:atom
                     % ?LeafNode:iri
    tms_node/2, % +TMS:atom
                % -Node:iri
    tms_node_to_url/3 % +Node:iri
                      % ?BaseURL:url
                      % -NodeURL:url
  ]
).

/** <module> TMS

The generic predicates for Truth-Maintenance Systems.

@author Wouter Beek
@version 2013/05, 2013/09-2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_search)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- dynamic(tms/2).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_argument(r,-)).
:- rdf_meta(tms_create_justification_iri(+,+,+,r,-)).
:- rdf_meta(tms_justification(?,r)).
:- rdf_meta(tms_justification(?,?,?,r,r)).



deregister_tms(Type, TMS):-
  retractall(tms(Type, TMS)).

generic_tms(TMS, Pred, Args):-
  once(tms(Type, TMS)),
  generic(Pred, Type, Args).

is_in_node(TMS, Node):-
  generic_tms(TMS, is_in_node, [Node]).

is_out_node(TMS, Node):-
  generic_tms(TMS, is_out_node, [Node]).

register_tms(Type, TMS):-
  db_add_novel(tms(Type, TMS)).

%! tms(+TMS:atom) is semidet.
% Succeeds if a TMS with the given name exists.
%! tms(-TMS:atom) is nondet.
% Enumerates the names of the current TMS-es.

tms(TMS):-
  once(tms(_Type, TMS)).

%! tms_create_node_iri(+Label:atom, -Node:iri) is semidet.
% Returns the node with the given label, if it exists.

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

tms_justification(TMS, J):-
  rdfs_individual_of(J, tms:'Justification'),
  rdf(J, _, _, TMS:_).

%! tms_justification(
%!   ?TMS:atom,
%!   ?Antecedents:list(iri),
%!   ?Rule:atom,
%!   ?Consequent:iri,
%!   ?Justification:iri
%! ) is nondet.
% TMS justifications.
%
% @param TMS The atomic name of a TMS.
% @param Antecedents A list of TMS nodes that are antecedents to this
%        justification.
% @param Rule The atomic name of the rule that was applied in this
%        justification.
% @param Consequence A TMS node that is the consequent of this
%        justification.
% @param Justification A TMS justification.

tms_justification(TMS, As, R, C, J):-
  tms_justification(TMS, J),
  findall(A, rdf_has(J, tms:has_antecedent, A), As),
  rdfs_label(J, R),
  rdf(J, tms:has_consequent, C, TMS).

%! tms_justifications(
%!   +Options:list(nvpair),
%!   +Node:iri,
%!   -Justifications:ordset(iri)
%! ) is det.
% Returns all justifications that explain the given node, recursively.

tms_justifications(O1, N, Js):-
  option(recursive(true), O1, false), !,
  rdf_breadth_first(
    N,
    tms:has_consequent,
    tms:has_antecedent,
    _Ns,
    Js
  ).
tms_justifications(_O1, N, Js):-
  tms_node(TMS, N),
  setoff(J, tms_justification(TMS, _As, _R, N, J), Js).

%! tms_leaf_node(?TMS:atom, ?LeafNode:iri) is nondet.
% TMS leaf nodes are nodes in the TMS that are antecedent to no justification.
%
% TMS leaf nodes are handy for exporting the full justificational structure
% of a TMS, since non-leaf nodes will be visited by recursively searching
% from leaf nodes only.

tms_leaf_node(TMS, N):-
  tms_node(TMS, N),
  \+ ((
    tms_justification(TMS, As, _R, _C, _J),
    memberchk(N, As)
  )).

%! tms_node(+TMS:atom, -Node:iri) is nondet.
% Enumerates the nodes in the TMS with the given name.
%
% @param TMS The atomic name of a TMS.
% @param Node An IRI denoting a TMS node.

tms_node(TMS, Node):-
  rdfs_individual_of(Node, tms:'Node'),
  once(rdf(Node, _, _, TMS:_)).

%! tms_node_to_iri(+Node:iri, ?BaseURL:url, -NodeURL:url) is det.

tms_node_to_url(N, BaseURL, N):-
  var(BaseURL), !.
tms_node_to_url(N, BaseURL, N_URL):-
  rdf_global_id(_NS:LocalN, N),
  uri_query_add(BaseURL, node, LocalN, N_URL).

