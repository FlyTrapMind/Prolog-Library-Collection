:- module(
  tms_web,
  [
    tms_node_web/2, % +NodeLabel:atom
                    % -SVG:list
    tms_web/1, % -DOM:list
    tms_web/2 % +TMS:atom
              % -SVG:list
  ]
).

/** <module> TMS web

@author Wouter Beek
@version 2013/10
*/

:- use_module(generics(meta_ext)).
:- use_module(gv(gv_file)).
:- use_module(html(html_table)).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).



%! tms_node_web(+NodeLabel:atom, -SVG:list) is det.

tms_node_web(NLabel, SVG):-
  tms_create_node_iri(NLabel, N),
gtrace,
  tms_export_node([recursive(false)], N, GIF),
  graph_to_svg_dom([], GIF, dot, SVG).

%! tms_web(-DOM:list) is det.
% Returns a DOM description of the currently loaded TMS-es.

tms_web([HTML_Table]):-
  findall(
    [TMS,Type,NumberOfJs,NumberOfNs],
    (
      tms(Type, TMS),
      setoff(J, tms_justification(TMS, J), Js),
      length(Js, NumberOfJs),
      setoff(N, tms_node(TMS, N), Ns),
      length(Ns, NumberOfNs)
    ),
    Rows
  ),
  html_table(
    [
      caption('The currently loaded Truth Maintenance Systems.'),
      header(true),
      indexed(true)
    ],
    [['TMS','Type','#Justifications','#Nodes']|Rows],
    HTML_Table
  ).

%! tms_web(+TMS:atom, -SVG:list) is det.

tms_web(TMS, SVG):-
  tms_export_graph(TMS, GIF),
  graph_to_svg_dom([], GIF, sfdp, SVG).

