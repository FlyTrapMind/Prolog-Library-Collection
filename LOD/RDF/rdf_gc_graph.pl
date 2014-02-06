:- module(
  rdf_gc_graph,
  [
    rdf_graph_touch/1, % +Graph:atom
    rdf_gc_graph/0
  ]
).

/** <module> RDF graph garbage collector

@author Wouter Beek
@version 2014/02
*/

:- use_module(generics(thread_ext)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- dynamic(rdf_graph/2).

:- initialization(init_rdf_gc_graph).



rdf_graph_touch(Graph):-
  with_mutex(rdf_gc_graph, rdf_graph_touch_sync(Graph)).

rdf_graph_touch_sync(Graph):-
  retract(rdf_graph(_, Graph)), !,
  rdf_graph_touch_now(Graph).
rdf_graph_touch_sync(Graph):-
  rdf_graph_touch_now(Graph).

rdf_graph_touch_now(Graph):-
  get_time(Time),
  assert(rdf_graph(Time, Graph)).


rdf_gc_graph:-
  rdf_statistics(triples(Triples)),
  rdf_gc_graph(Triples).

rdf_gc_graph(Triples):-
  % 1,000,000
  Triples =< 1000000, !.
rdf_gc_graph(_):-
  findall(
    Time-Graph,
    rdf_graph(Time, Graph),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [OldestGraph|_]),
  rdf_unload_graph(OldestGraph),
  rdf_gc_graph.


init_rdf_gc_graph:-
  % Run every minute.
  intermittent_thread(rdf_gc_graph, fail, 60, _, []).

