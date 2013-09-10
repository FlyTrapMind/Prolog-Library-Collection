:- module(
  rdf_deb,
  [
    rdf_graphs_web/1 % -DOM:list
  ]
).

/** <module> RDF debug tools

@author Wouter Beek
@version 2013/09
*/

:- use_module(html(html)).
:- use_module(library(semweb/rdf_db)).



rdf_graphs(Rows):-
  findall(
    [G,NumberOfTriples],
    (
      rdf_graph(G),
      rdf_statistics(triples_by_graph(G, NumberOfTriples))
    ),
    Rows
  ).

rdf_graphs_web(DOM):-
  rdf_graphs(Rows),
  list_to_table(
    [caption('Overview of currently loaded graphs.'),header(true)],
    [['Name','Number of triples']|Rows],
    DOM
  ).

