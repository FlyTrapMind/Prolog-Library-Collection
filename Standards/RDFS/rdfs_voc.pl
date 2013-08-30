:- module(
  rdfs_voc,
  [
    rdf_voc_pdf/1, % ?File:atom
    rdf_voc_web/1, % -SVG:dom
    rdfs_voc_pdf/1, % ?File:atom
    rdfs_voc_web/1 % -SVG:dom
  ]
).

/** <module> RDFS_VOC

Exports the vocabulary for RDFS.

@author Wouter Beek
@version 2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(gv(gv_file)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').



% RDF VOCABULARY %

rdf_voc(GIF):-
  load_in_graph(G),

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
  graph_to_gv_file([], GIF, sfdp, pdf, File).

rdf_voc_web(SVG):-
  rdf_voc(GIF),
  graph_to_svg_dom([], GIF, sfdp, SVG).



% RDFS VOCABULARY %

rdfs_voc(GIF):-
  load_in_graph(G),

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
  graph_to_gv_file([], GIF, sfdp, pdf, File).

rdfs_voc_web(SVG):-
  rdfs_voc(GIF),
  graph_to_svg_dom([], GIF, sfdp, SVG).



% GENERICS %

%! load_in_graph(-Graph:atom) is det.

load_in_graph(G2):-
  % Load the W3C file specifying the vocabulary for RDFS.
  rdf_new_graph(rdfs_schema, G1),
  absolute_file_name(rdfs(rdfs), File, [access(read), file_type(rdf)]),
  rdf_load2(File, [graph(G1)]),

  % We want to hide some aspects from the displayed version:
  % Every concept has an =|rdfs:isDefinedBy|= relation to
  % either =|:rdf|= or =|:rdfs|=.
  % We indicate this distinction using colors instead.
  rdf_new_graph(rdfs_schema, G2),
  % @tbd Modules?!
  rdf_graph:rdf_graph_copy(G1, G2).

