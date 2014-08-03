:- module(
  graph_travel,
  [
    traverse/7, % +Graph
                % :EdgePred
                % :NeighborPred
                % +First
                % +Last
                % -Distance:nonneg
                % +Options:list(nvpair)
    traverse/10, % +Graph
                 % :EdgePred
                 % :NeighborPred
                 % +First
                 % +Last
                 % -Distance:nonneg
                 % -Vertices:ordset
                 % -Edges:ordset(pair)
                 % -History:list
                 % +Options:list(nvpair)
    travel_min/7, % +Graph
                  % :EdgePred
                  % :NeighborPred
                  % +First
                  % +Last
                  % -MinimumDistance:nonneg
                  % +Options:list(nvpair)
    travel_min/10 % +Graph
                  % :EdgePred
                  % :NeighborPred
                  % +First
                  % +Last
                  % -MinimumDistance:nonneg
                  % -Vertices:ordset
                  % -Edges:ordset(pair)
                  % -History:list
                  % +Options:list(nvpair)
  ]
).

/** <module> Graph traversal

@author Wouter Beek
@version 2013/01-2013/04, 2013/07, 2013/09, 2013/12, 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % RDF meta predicate declarations.

:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).

:- meta_predicate(traverse(+,+,2,3,+,+,-)).
:- meta_predicate(traverse(+,+,2,3,+,+,-,-,-,-)).
:- meta_predicate(traverse1(+,+,2,3,+,+,-,-,-,-,-,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-,-,-,-)).

% Graph traversal can operate on abbreviated URIs.
:- rdf_meta(traverse(+,+,:,:,r,r,-)).
:- rdf_meta(traverse(+,+,:,:,r,r,-,-,-,-)).
:- rdf_meta(traverse1(+,+,:,:,r,r,-,-,-,-,-,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-,-,-,-)).

% Meta-option.
is_meta(deb_vertex_name).



%! traverse(
%!   +Graph:atom,
%!   :EdgePred,
%!   :NeighborPred,
%!   +First,
%!   +Last,
%!   -Distance:nonneg,
%!   +Options:list(nvpair)
%! ) is nondet.
% @see Wrapper around traverse/10.

traverse(Graph, EdgePred, NeighborPred, First, Last, Distance, Options):-
  traverse(
    Graph, EdgePred, NeighborPred, First, Last, Distance, _, _, _, Options
  ).

%! traverse(
%!   +Graph:atom,
%!   :EdgePred,
%!   :NeighborPred,
%!   +First,
%!   +Last,
%!   -Distance:nonneg,
%!   -Vertices:ordset,
%!   -Edges:ordset(pair),
%!   -History:list,
%!   +Options:list(nvpair)
%! ) is nondet.
% Lets travel through graph land!
%
% A *walk* is an alternating sequence of vertices and edges,
% starting and ending in a vertex.
% ~~~{.pl}
% Options = []
% ~~~
%
% In a *closed* walk sequence, the first and the last vertex are the same.
% *Open* walk sequences are not closed.
%
% A *trail* is a walk with distinct edges.
% ~~~{.pl}
% Options = [distinct_edges(true)]
% ~~~
%
% A *tour* is a closed trail.
% ~~~{.pl}
% Options = [closed(true),distinct_edges(true)]
% ~~~
%
% A *path* is a trail with distinct vertices.
% ~~~{.pl}
% Options = [distinct_edges(true),distinct_vertices(true)]
% ~~~
%
% A *cycle* is a closed path.
% ~~~{.pl}
% Options = [closed(true),distinct_edges(true),distinct_vertices(true)]
% ~~~
%
% An *Euler tour* is a tour in which all edges are traversed exactly one.
% ~~~{.pl}
% Options = [closed(true),every_edge(true),distinct_edges(true)]
% ~~~
%
% The following options are defined:
%   1. =|closed(boolean)|=
%   2. =|distance(oneof([edge,vertex]))|=
%      For statiscs we return either
%      the number of edges (value `edges`; the default value)
%      or the number of vertices (value `vertices`) that were traversed.
%   3. =|euler(boolean)|=
%   4. =|every_edge(boolean)|=
%   5. =|every_vertex(boolean)|=
%   6. =|graph(Graph)|=
%   7. =|distinct_edges(boolean)=
%   8. =|distinct_vertices(boolean)=
%   9. =|deb_vertex_name(:VertexNaming)|=
%      Debugging option for assigning readable names to vertices.
%      The predicate should have the arguments
%      =|(+Vertex,-VertexName:atom)|=.
%
% @arg Graph
% @arg EdgePred Maps a graph to its edges.
% @arg NeighborPred Maps a vertex to its neighbor vertices.
% @arg First The first vertex in the path.
% @arg Last The last vertex in the path.
% @arg Distance An integer representing a distance between the first and
%      the last vertex, counted as the number of traversed edges.
% @arg Vertices A list of vertices.
% @arg Edges A list of edges.
% @arg History
% @arg Options A list of name-value pairs.

traverse(
  Graph, EdgePred, NeighborPred, First, Last,
  Distance, Vertices, Edges, History, Options1
):-
  % First we make sure we have the right options set,
  % so we do not have to do this in every iteration.
  
  % Cycles, tours, and Euler tours are closed.
  add_default_option(Options1, closed, false, Options2),
  
  % Euler tours must use every edge.
  add_default_option(Options2, every_edge, false, Options3),
  
  % The default distance metric is edges.
  add_default_option(Options3, distance, edge, Options4),
  
  % Cycles and paths have a distinct number of vertices.
  add_default_option(Options4, distinct_vertices, false, Options5),
  
  % Trails and Euler tours has a no duplicate edges.
  add_default_option(Options5, distinct_edges, false, Options6),
  
  meta_options(is_meta, Options6, Options7),
  
  traverse1(
    Graph, EdgePred, NeighborPred, First, Last,
    Distance, [First], Vertices, [], Edges, History, Options7
  ).

% Done, perform some option-dependent checks
% and calculate some option-dependent statistics.
traverse1(
  Graph, EdgePred, _, Last, Last,
  Distance, SolV, SolV, SolE, SolE, [Last], Options
):-
  % In a closed walk (or tour) the first and the last vertex must be
  % the same.
  (  option(closed(true), Options)
  -> last(SolV, Last)
  ;  true),
  
  % In an Euler tour all edges must be visited.
  (  option(every_edge(true), Options)
  -> call(EdgePred, Graph, AllEdges),
     ord_subtract(AllEdges, SolE, UntraversedEdges),
     ord_empty(UntraversedEdges)
  ;  true),
  
  % Distance metric. The statistics we return.
  (  option(distance(edge), Options)
  -> length(SolE, Distance)
  ;  length(SolV, Distance)).
% Recursion: traversal by visiting neighboring vertices.
traverse1(
  Graph, EdgePred, NeighborPred, FirstV, Last,
  Distance, Vs, SolV, Es, SolE, [FirstV,FirstV-NextV|History], Options
):-
  % Neighbor
  call(NeighborPred, Graph, FirstV, NextV),
  
  % Debugging.
  (  debugging(graph_traversal),
     option(deb_vertex_name(VName), Options)
  -> call(VName, FirstV, FirstVName),
     call(VName, NextV, NextVName),
     debug(graph_travel, '~w\t--->\t~w', [FirstVName,NextVName])
  ;  true),
  
  % Check the walk restriction: no duplicate vertices.
  (option(distinct_vertices(true), Options) -> \+ member(NextV, Vs) ; true),
  
  % Check the trail and Euler tour restriction: no duplicate edges.
  (  option(distinct_edges(true), Options)
  -> \+ member(FirstV-NextV, Es)
  ;  true),
  
  ord_add_element(Vs, NextV, NewVs),
  ord_add_element(Es, FirstV-NextV, NewEs),
  traverse1(
    Graph, EdgePred, NeighborPred, NextV, Last,
    Distance, NewVs, SolV, NewEs, SolE, History, Options
  ).

/*
travel2(Graph, NeighborPred, FromV, ToV, Length, AllVs, Path):-
  travel2(Graph, NeighborPred, FromV, ToV, Length, [FromV], AllVs, Path).

travel2(_G, _N_P, ToV, ToV, Length, AllVs, AllVs, [ToV]):-
  length(AllVs, Length), !.
travel2(Graph, NeighborPred, FromV, ToV, Length, Vs, AllVs, [FromV | Path]):-
  call(NeighborPred, FromV, Graph, ToVs),
  member(ViaV, ToVs),
  % Make sure we haven't be here yet.
  \+ member(ViaV, Vs),
  ord_add_element(Vs, ToV, NewVs),
  travel2(Graph, NeighborPred, ViaV, ToV, Length, NewVs, AllVs, Path).
*/

%! tarvel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :EdgePred,
%!   :NeighborPred,
%!   +First,
%!   +Last,
%!   -MinimumDistance:nonneg
%! ) is det.
% @see Wrapper around travel_min/10.

travel_min(O, Graph, EdgePred, NeighborPred, First, Last, MinimumDistance):-
  travel_min(O, Graph, EdgePred, NeighborPred, First, Last, MinimumDistance, _Vs, _Es, _History).

%! travel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :EdgePred,
%!   :NeighborPred,
%!   +First,
%!   +Last,
%!   -MinimumDistance:nonneg,
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge),
%!   -History:list
%! ) is det.
% Returns the minimum distance between the given subject and predicate terms.
%
% @arg Options A list of name-value pairs.
%      See travel/7 for the list of supported options.
% @arg Graph
% @arg EdgePred
% @arg NeighborPred
% @arg First A vertex, the first in the travel.
% @arg Last A respource, the last in the travel.
% @arg MinimumDistance An integer representing the minimum distance
%      between the first and last vertex. The kind of distances is set
%      in =Options=.
% @arg Vertices An ordered set of vertices.
% @arg Edges An ordered set of Edges.
% @arg History A list representing a minimum travel between the first and
%      last resources.

travel_min(O, Graph, EdgePred, NeighborPred, First, Last, MinimumDistance, Vs, Es, History):-
  aggregate_all(
    set(Distance-History),
    traverse(O, Graph, EdgePred, NeighborPred, First, Last, Distance, Vs, Es, History),
    Pairs
  ),
  first(Pairs, MinimumDistance-History).

