:- module(
  rdfs_voc,
  [
    rdfs_voc_to_pdf/0
  ]
).

/** <module> RDFS_VOC

Exports the vocabulary for RDFS.

@author Wouter Beek
@version 2013/08
*/

:- use_module(gv(gv_file)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).



rdfs_voc_to_pdf:-
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
  rdf_graph:rdf_graph_copy(G1, G2),
  rdf_retractall(_, rdfs:isDefinedBy, _, G2),
  rdf_register_namespace_color(G2, rdf, darkblue),
  rdf_register_namespace_color(G2, rdfs, darkgreen),
  
  % Thats it, let's export the RDF graph and convert it
  % to a PDF using GraphViz.
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(preferred_label),
      uri_desc(uri_only)
    ],
    G2,
    GIF
  ),
  graph_to_gv_file([], GIF, sfdp, pdf, PDF_File),
  open_pdf(PDF_File).
