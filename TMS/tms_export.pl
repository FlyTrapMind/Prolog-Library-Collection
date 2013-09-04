:- module(
  tms_export,
  [
    tms_export_argument/2, % +Node:iri
                           % -GraphInterchangeFormat:compound
    tms_export_graph/2, % +TMS:atom
                        % -GraphInterchangeFormat:compound
    tms_export_justifications/3, % +TMS:atom
                                 % +Justifications:list(iri)
                                 % -GraphInterchangeFormat:compound
    tms_print_argument/3, % +Options:list(nvpair)
                          % +TMS:atom
                          % +Node:iri
    tms_print_justification/3 % +Options:list(nvpair)
                              % +TMS:atom
                              % +Justification:iri
  ]
).

/** <module> TMS EXPORT

Exports TMS belief states,

@author Wouter Beek
@tbd Update Doyle export to the new GV modules.
@version 2013/05, 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(tms(tms)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(doyle, 'http://www.wouterbeek.com/doyle.owl#').
:- xml_register_namespace(tms,   'http://www.wouterbeek.com/tms.owl#'  ).

:- rdf_meta(tms_export_edge_style(r,-)).
:- rdf_meta(tms_export_edges(+,+,r,r,+,-)).
:- rdf_meta(tms_export_node_color(+,r,-)).
:- rdf_meta(tms_print_argument(+,r)).
:- rdf_meta(tms_print_argument(+,+,r)).



%! tms_export_argument(+Node:iri, -GIF:compound) is det.

tms_export_argument(N, GIF):-
  % Retrieve the TMS in which the node appears.
  tms_node(TMS, N),
  % The argument for the node consists of a list of justifications.
  tms_argument(N, Js),
  % Export the justifications that constitute the argument.
  tms_export_justifications(TMS, Js, GIF).

%! tms_export(+TMS:atom) is det.
% Exports the TMS using GraphViz.

tms_export_graph(TMS, GIF):-
  is_registered_tms(TMS),
  setoff(N, tms_node(TMS, N), Ns),
  setoff(J, tms_justification(TMS, J), Js),
  tms_export_graph(TMS, Ns, Js, GIF).

tms_export_graph(TMS, Ns, Js, graph(Vs,Es4,G_Attrs)):-
  maplist(tms_export_node(TMS), Ns, N_Vs),
  maplist(tms_export_justification(TMS), Js, J_Vs),
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

tms_export_justifications(TMS, Js, GIF):-
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
  tms_export_graph(TMS, Ns, Js, GIF).

tms_print_argument(O1, TMS, C):-
  default_option(O1, indent, 0, O2),
  tms_print_argument_(O2, TMS, C).

tms_print_argument_(O, TMS, C):-
  tms_node(TMS, C),
  rdf(J, tms:has_consequent, C, TMS), !,
  tms_print_justification(O, TMS, J).
tms_print_argument_(O, _TMS, C):-
  option(indent(I), O, 0),
  indent(I),
  write('[rdf] '),
  tms_print_node(O, C),
  nl.

tms_print_justification(O1, TMS, J):-
  tms_justification(TMS, As, R, C, J),
  option(indent(I), O1, 0),
  indent(I),
  write('['), write(R), write(']'),
  write(' '),
  tms_print_node(O1, C),
  nl,
  update_option(O1, indent, succ, _I, O2),
  maplist(tms_print_argument(O2, TMS), As).

tms_print_node(O, N):-
  option(lang(Lang), O, en),
  rdfs_preferred_label(N, Lang, _PreferredLang, L),
  write(L).



% EDGES %

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

tms_export_edge_style(tms:has_consequent, solid ):- !.
tms_export_edge_style(tms:has_in,         solid ):- !.
tms_export_edge_style(tms:has_out,        dashed):- !.

tms_export_cons_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, true, J, tms:has_consequent, Es1, Es2).

tms_export_in_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_in, Es1, Es2).

tms_export_out_edges(TMS, J, Es1, Es2):-
  tms_export_edges(TMS, false, J, tms:has_out, Es1, Es2).



% VERTICES %

tms_export_justification(_TMS, J, vertex(J_Id,J,V_Attrs)):-
  rdf_global_id(_:J_Id, J),
  rdfs_label(J, L),
  V_Attrs = [color(blue),label(L),shape(rectangle),style(solid)].

tms_export_node(TMS, N, vertex(N_Id,N,V_Attrs)):-
  rdf_global_id(_:N_Id, N),
  rdfs_label(N, L),
  tms_export_node_color(TMS, N, C),
  V_Attrs = [color(C),label(L),shape(ellipse),style(solid)].

%! tms_export_node_color(+TMS:atom, +Node:iri, -Color:atom) is det.
% Returns the color indicating the support status of the node.
tms_export_node_color(TMS, N, green):-
  is_in_node(TMS, N), !.
tms_export_node_color(TMS, N, red):-
  is_out_node(TMS, N), !.
tms_export_node_color(_TMS, _N, black).

