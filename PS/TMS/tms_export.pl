:- module(
  tms_export,
  [
    tms_export_graph/2, % +TMS:atom
                        % -GraphInterchangeFormat:compound
    tms_export_node/3 % +Options:list(nvpair)
                      % +Node:iri
                      % -GraphInterchangeFormat:compound
  ]
).

/** <module> TMS export

Exports TMS belief states,

@author Wouter Beek
@version 2013/05, 2013/09-2013/10
*/

:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(tms(tms)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_export_edges(+,+,r,r,+,-)).
:- rdf_meta(tms_export_node(+,r,-)).



%! tms_export_graph(+TMS:atom, -GIF:compound) is det.
% Exports the TMS using GraphViz.

tms_export_graph(TMS, GIF):-
  tms(TMS),
  setoff(N, tms_node(TMS, N), Ns),
  setoff(J, tms_justification(TMS, J), Js),
  tms_export_graph(TMS, Ns, Js, GIF).

tms_export_graph(TMS, Ns, Js, graph(Vs,Es4,G_Attrs)):-
  maplist(tms_export_node_(TMS), Ns, N_Vs),
  maplist(tms_export_justification_(TMS), Js, J_Vs),
  ord_union(N_Vs, J_Vs, Vs),
  ord_empty(Es1),
  foldl(tms_export_cons_edges(TMS), Js, Es1, Es2),
  foldl(tms_export_in_edges(TMS), Js, Es2, Es3),
  foldl(tms_export_out_edges(TMS), Js, Es3, Es4),
  G_Attrs = [
    charset('UTF-8'),
    directedness(forward),
    fontsize(11),
    label(TMS),
    overlap(false)
  ].

%! tms_export_node(+Options:list(nvpair), +Node:iri, -GIF:compound) is det.
% The following options are supported:
%   * =|recursive(boolean)|=
%     Whether the justifications graph is traversed recursively or not.

tms_export_node(O1, N, GIF):-
  % The argument for the node consists of a list of justifications.
  tms_justifications(O1, N, Js),

  % Collect the nodes that are involved in the justifications.
  setoff(
    N,
    (
      member(J, Js),
      (
        rdf_has(J, tms:has_antecedent, N)
      ;
        rdf_has(J, tms:has_consequent, N)
      )
    ),
    Ns
  ),

  % Retrieve the TMS in which the node appears.
  tms_node(TMS, N),

  % The nodes (vertices) and justifications (edges) form a graph.
  tms_export_graph(TMS, Ns, Js, GIF).



% SUPPORT PREDICATES: EDGES %

%! tms_export_edges(
%!   +TMS:atom,
%!   +InverseDirection:boolean,
%!   +Justification:iri,
%!   +PredicateTerm:iri,
%!   +EdgeTerms1:list(compound),
%!   -EdgeTerms:list(compound)
%! ) is det.

tms_export_edges(TMS, Inv, J, P, Es1, Es2):-
  setoff(
    edge(From,To,E_Attrs),
    (
      rdf(J, P, N, TMS),
      rdf_global_id(_:N_Id, N),
      rdf_global_id(_:J_Id, J),
      tms_export_edge_style(P, Style),
      E_Attrs = [color(black),style(Style)],
      (Inv == true -> From = J_Id, To = N_Id ; From = N_Id, To = J_Id)
    ),
    NewEs
  ),
  ord_union(Es1, NewEs, Es2).

tms_export_edge_style(P, solid ):-
  rdf_global_id(tms:has_consequent, P), !.
tms_export_edge_style(P, solid ):-
  rdf_global_id(tms:has_in, P), !.
tms_export_edge_style(P, dashed):-
  rdf_global_id(tms:has_out, P), !.

tms_export_cons_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, true, J, tms:has_consequent, Es1, Es2).

tms_export_in_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_in, Es1, Es2).

tms_export_out_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_out, Es1, Es2).



% SUPPORT PREDICATES: VERTICES %

%! tms_export_justification_(
%!   +TMS:atom,
%!   +Justification:iri,
%!   -VertexTerm:compound
%! ) is det.

tms_export_justification_(_TMS, J, vertex(J_Id,J,V_Attrs)):-
  rdf_global_id(_:J_Id, J),
  rdfs_label(J, L),
  V_Attrs = [color(blue),label(L),shape(rectangle),style(solid)].

%! tms_export_node(+TMS:atom, +Node:iri, -VertexTerm:compound) is det.

tms_export_node_(TMS, N, vertex(N_Id,N,V_Attrs)):-
  rdf_global_id(_:N_Id, N),
  rdfs_label(N, L),
  tms_export_node_color(TMS, N, C),
  V_Attrs = [color(C),label(L),shape(ellipse),style(solid),'URL'('http://www.wouterbeek.com')].

%! tms_export_node_color(+TMS:atom, +Node:iri, -Color:atom) is det.
% Returns the color indicating the support status of the node.

tms_export_node_color(TMS, N, green):-
  is_in_node(TMS, N), !.
tms_export_node_color(TMS, N, red):-
  is_out_node(TMS, N), !.
tms_export_node_color(_TMS, _N, black).

