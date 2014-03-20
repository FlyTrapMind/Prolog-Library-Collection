:- module(
  rdf_tabular_graph,
  [
    rdf_tabular_graph//1, % +RdfGraph:atom
    rdf_tabular_graphs//0
  ]
).

/** <module> RDF tabular graph

Generates HTML tables for overviews of a single and for multiple
RDF graphs.

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(dcg(dcg_content)). % Used in HTML table caption.
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(rdf_web(rdf_tabular_class)).
:- use_module(rdf_web(rdf_tabular_datatype)).
:- use_module(rdf_web(rdf_tabular_property)).
:- use_module(tms(tms)).



%! rdf_tabular_graph(+RdfGraph:atom)// is det.
% Generates an HTML table describing the contents of the given RDF graph.
%
% The generated HTML consists of overviews of:
%   * All RDFS classes in the graph.
%   * All RDF properties in the graph.
%   * All datatype IRIs in the graph.

rdf_tabular_graph(Graph) -->
  rdf_tabular_classes(Graph),
  rdf_tabular_properties(Graph),
  rdf_tabular_datatypes(Graph).


%! rdf_tabular_graphs// is det.
% Generates an HTML table describing the currently loaded RDF graphs.
%
% The RDF graphs are orderd by the number of triples they contain.
%
% We exclude TMS-es from this overview.

rdf_tabular_graphs -->
  {
    findall(
      NumberOfTriples-Graph,
      (
        rdf_graph(Graph),
        \+ tms(Graph),
        rdf_statistics(triples_by_graph(Graph,NumberOfTriples))
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    pairs_keys(Pairs2, Keys),
    sum_list(Keys, Triples),
    reverse(Pairs2, Pairs3),
    findall(
      [Graph,NumberOfTriples],
      member(NumberOfTriples-Graph, [Triples-'All'|Pairs3]),
      Rows
    )
  },
  rdf_html_table(
    [header_row(true)],
    html('RDF graphs (non-TMS)'),
    [['Graph','Number of triples']|Rows]
  ).

