:- module(
  rdf_tabular_graph,
  [
    rdf_tabular_graph//1, % +Graph:atom
    rdf_tabular_graphs//0
  ]
).

/** <module> RDF tabular graph

Generates HTML tables for overviews of RDF graphs.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_content)). % Used in HTML table caption.
:- use_module(generics(meta_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(tms(tms)).



rdf_tabular_graph(Graph) -->
  {
    setoff(
      S,
      (
        rdf(S, _, _, Graph),
        \+ rdf_is_bnode(S)
      ),
      Ss
    ),
    setoff(
      NumberOfTriples-S,
      (
        member(S, Ss),
        rdf_estimate_complexity(S, _, _, NumberOfTriples)
      ),
      Pairs1
    ),
    keysort(Pairs1, Pairs2),
    reverse(Pairs2, Pairs3),
    findall(
      [S,NumberOfTriples],
      member(NumberOfTriples-S, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    Graph,
    (`Subject terms in graph `, atom(Graph)),
    ['Subject','Number of triples'],
    Rows
  ).


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
    reverse(Pairs2, Pairs3),
    findall(
      [Graph,NumberOfTriples],
      member(NumberOfTriples-Graph, Pairs3),
      Rows
    )
  },
  rdf_html_table(
    _NoGraph,
    `RDF graphs (non-TMS)`,
    ['Graph','Number of triples'],
    Rows
  ).

