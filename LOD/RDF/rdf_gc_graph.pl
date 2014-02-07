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
  % 10,000,000
  Triples =< 10000000, !.
rdf_gc_graph(_):-
  findall(
    Time-Graph,
    rdf_graph(Time, Graph),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [OldestGraph|_]),
  
  % Remove from administration.
  retract(rdf_graph(LastTouch, OldestGraph)),
  
  % Assemble information to be displayed in debug message.
  duration(LastTouch, Duration),
  rdf_statistics(triples_by_graph(OldestGraph,Triples)),
  
  % Unload the graph and all of its contents.
  rdf_unload_graph(OldestGraph),
  
  % Display the debug message.
  format(
    user_output,
    '[-~d,~w] Unloaded graph ~w\n.',
    [Triples,Duration,OldestGraph]
  ),
  flush_output(user_output),
  
  % See whether there is more work to do.
  rdf_gc_graph.

duration(Timestamp, Duration):-
  get_time(Now),
  Delta is Now - Timestamp,
  stamp_date_time(Delta, date(Y1,MM1,D1,H1,M1,S1,_,_,_), 'UTC'),
  Y2 is Y1 - 1970,
  MM2 is MM1 - 1,
  D2 is D1 - 1,
  format_time(atom(Duration), '%FT%T', date(Y2,MM2,D2,H1,M1,S1,0,-,-)).


init_rdf_gc_graph:-
  % Run 5 seconds.
  intermittent_thread(rdf_gc_graph, fail, 5, _, []).

