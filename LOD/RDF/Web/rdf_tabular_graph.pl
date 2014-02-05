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
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(tms(tms)).



%! rdf_tabular_graph(+Graph:atom)// is det.
% Generates an HTML table describing the contents of the given graph.

rdf_tabular_graph(Graph) -->
  {
    % Collect the subject terms in the graph.
    setoff(
      S,
      (
        rdf(S, _, _, Graph),
        \+ rdf_is_bnode(S)
      ),
      Ss
    ),
    
    % Order the subjects by the number of triples that describe them
    % (using estimates).
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
    
    % Restrict the number of rows in the table to the *n* top-dogs.
    list_truncate(Pairs3, 1000, Pairs4),
    
    % Construct the table rows.
    findall(
      [S,NumberOfTriples],
      member(NumberOfTriples-S, Pairs4),
      Rows
    )
  },
  
  % Generate the HTML table using a special writer for the RDF terms.
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

