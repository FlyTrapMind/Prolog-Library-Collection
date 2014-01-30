:- module(
  rdfs_voc,
  [
    load_rdfs_vocabulary/1, % +Graph:atom
    rdf_voc_pdf/1, % ?File:atom
    rdf_voc_web/1, % -SVG:dom
    rdfs_voc_pdf/1, % ?File:atom
    rdfs_voc_web/1 % -SVG:dom
  ]
).

/** <module> RDFS_VOC

Exports the vocabulary for RDFS.

@author Wouter Beek
@version 2013/08, 2013/11
*/

:- use_module(gv(gv_file)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_reasoning(rdf_mat)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').



%! load_rdfs_vocabulary(+Graph:atom) is det.
% Pre-load the RDF(S) vocabulary.
% This means that materialization has to make less deductions
% (tested on 163 less), and there are some labels and comments
% that deduction would not produce.

load_rdfs_vocabulary(Graph):-
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_load([], Graph, File).

load_rdfs_vocabulary_(G2):-
  G1 = rdfs_voc,
  load_rdfs_vocabulary(G1),
  materialize(
    [entailment_regimes([rdf,rdfs]),multiple_justifications(false)],
    G1
  ),
  rdf_graph_merge([G1], G2).

rdf_voc(GIF):-
  load_rdfs_vocabulary_(G),

  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, G),
  rdf_register_namespace_color(G, rdf, darkblue),

  % Remove the RDFS-only triples.
  forall(
    (
      rdf(S, P, O, G),
      rdf_global_id(rdfs:_, S),
      rdf_global_id(rdfs:_, P),
      rdf_global_id(rdfs:_, O)
    ),
    rdf_retractall(S, P, O, G)
  ),

  % Thats it, let's export the RDF graph to GIF.
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(preferred_label),
      uri_desc(uri_only)
    ],
    G,
    GIF
  ).

rdf_voc_pdf(File):-
  (nonvar(File) -> access_file(File, write); true),
  rdf_voc(GIF),
  graph_to_gv_file([method(sfdp),to_file_type(pdf)], GIF, File).

rdf_voc_web(SVG):-
  rdf_voc(GIF),
  graph_to_svg_dom([method(sfdp)], GIF, SVG).



% RDFS VOCABULARY %

rdfs_voc(GIF):-
  load_rdfs_vocabulary_(G),

  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, G),
  rdf_register_namespace_color(G, rdf, darkblue),
  rdf_register_namespace_color(G, rdfs, darkgreen),

  % Thats it, let's export the RDF graph to GIF.
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(all),
      uri_desc(uri_only)
    ],
    G,
    GIF
  ).

rdfs_voc_pdf(File):-
  (nonvar(File) -> access_file(File, write); true),
  rdfs_voc(GIF),
  graph_to_gv_file([method(sfdp),to_file_type(pdf)], GIF, File).

rdfs_voc_web(SVG):-
  rdfs_voc(GIF),
  graph_to_svg_dom([method(sfdp)], GIF, SVG).

