:- module(
  graph_generic,
  [
    beam/6, % +Options:list(nvpair)
            % :EsVs_P
            % +Vertex
            % +Predicates:list
            % -Vertices:ordset
            % -Edges:ordset(edge)
    betweenness/5, % +Graph
                   % :V_P
                   % :E_P
                   % :N_P
                   % +V
                   % -Betweenness:float
    bipartite/4, % +Graph
                 % :E_P
                 % -Vs1:ord_set
                 % -Vs2:ord_set
    component/4, % :V_P
                 % :E_P
                 % ?Component
                 % +Graph
    connected/3, % :V_P
                 % :E_P
                 % +Graph
    cubic/2, % :V_P
             % +Graph:ugraph
    degree/3, % +Graph
              % +Vertex
              % -Degree:integer
    degree_sequence/3, % +Graph
                       % :V_P
                       % -DegreeSequence:list(integer)
    depth/6, % +Options:list(nvpair)
             % :N_P
             % +Vertex
             % +Depth:integer
             % -Vertices:ordset
             % -Edges:ordset(edge)
    graphic/1, % +S:list(integer)
    has_cycle/3, % :V_P
                 % :E_P
                 % +Graph
    is_undirected/2, % :E_P
                     % +Graph
    regular/2, % :V_P
               % +Graph
    regular/3, % :V_P
               % +Graph
               % ?K:integer
    shortest_paths/7, % +Graph
                      % :E_P
                      % :N_P
                      % +From
                      % +To
                      % +Pass
                      % -ShortestPaths:list
    simple/3, % :V_P
              % :E_P
              % +Graph
    strict_subgraph/4, % :V_P
                       % :E_P
                       % ?StrictSubGraph
                       % ?Graph
    subgraph/4, % :V_P
                % :E_P
                % +SubGraph
                % +Graph
    travel/7, % +Options:list(nvpair)
              % +Graph
              % :E_P
              % :N_P
              % +First:vertex
              % +Last:vertex
              % -Distance:integer
    travel/10, % +Options:list(nvpair)
               % +Graph
               % :E_P
               % :N_P
               % +First:vertex
               % +Last:vertex
               % -Distance:integer
               % -Vertices:ordset(vertex)
               % -Edges:ordset(edge)
               % -History:list
    travel_min/7, % +Options:list(nvpair)
                  % +Graph
                  % :E_P
                  % :N_P
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
    travel_min/10 % +Options:list(nvpair)
                  % +Graph
                  % :N_P
                  % :E_P
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
                  % -Vertices:ordset(vertex)
                  % -Edges:ordset(edge)
                  % -History:list
  ]
).

/** <module> GRAPH_GENERIC

Predicate that implement generic graph operations.

Generic operations are possible via meta-arguments for retrieving
the edges and vertices.

@author Wouter Beek
@version 2013/01-2013/04, 2013/07
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(ugraph_ext)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)). % Meta-predicates.
:- use_module(rdf(rdf_graph_theory)).

:- meta_predicate(beam(+,2,+,+,-,-)).
:- meta_predicate(beam(+,2,+,+,-,+,-)).
:- meta_predicate(betweennes(+,2,2,-)).
:- meta_predicate(betweennes(+,2,2,+,-)).
:- meta_predicate(bipartite(+,2,-,-)).
:- meta_predicate(cubic(2,+)).
:- meta_predicate(component(2,2,?,+)).
:- meta_predicate(connected(2,2,+)).
:- meta_predicate(degree_sequence(+,2,-)).
:- meta_predicate(depth(+,2,+,?,-,-)).
:- meta_predicate(depth_(+,2,+,?,+,-,+,-)).
:- meta_predicate(has_cycle(2,2,+)).
:- meta_predicate(is_undirected(2,+)).
:- meta_predicate(regular(2,+)).
:- meta_predicate(regular(2,+,-)).
:- meta_predicate(shortest_paths(+,2,2,+,+,?,-)).
:- meta_predicate(simple(2,2,+)).
:- meta_predicate(strict_subgraph(2,2,?,+)).
:- meta_predicate(subgraph(2,2,?,+)).
:- meta_predicate(travel(+,+,2,2,+,+,-)).
:- meta_predicate(travel(+,+,2,2,+,+,-,-,-,-)).
:- meta_predicate(travel_(+,+,2,2,+,+,-,-,-,-,-,-)).
:- meta_predicate(travel_min(+,+,2,2,+,+,-)).
:- meta_predicate(travel_min(+,+,2,2,+,+,-,-,-,-)).

:- rdf_meta(beam(+,:,r,+,-,-)).
:- rdf_meta(beam(+,:,+,+,-,+,-)).
:- rdf_meta(betweennes(+,:,:,r,-)).
:- rdf_meta(degree(+,r,-)).
:- rdf_meta(depth(+,:,r,?,-,-)).
:- rdf_meta(depth_(+,:,r,?,+,-,+,-)).
:- rdf_meta(shortest_paths(+,:,:,r,r,r,-)).
:- rdf_meta(travel(+,+,:,:,r,r,-)).
:- rdf_meta(travel(+,+,:,:,r,r,-,-,-,-)).
:- rdf_meta(travel_(+,+,:,:,r,r,-,-,-,-,-,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-,-,-,-)).



beam(O, EsVs_P, V, Ps, Vs, Es):-
  beam(O, EsVs_P, [V], Ps, Vs, [], Es).

beam(_O, EsVs_P, [], _Ps, AllVs, AllEs, AllEs):-
  call(EsVs_P, AllEs, AllVs), !.
beam(O, EsVs_P, Vs, Ps, AllVs, Es, AllEs):-
  setoff(
    V-NextV,
    (
      member(V, Vs),
      member(P, Ps),
      rdf_has(V, P, NextV),
      \+ member(V-NextV, Es)
    ),
    NextEs
  ),
  ord_union(Es, NextEs, NewEs),
  call(EsVs_P, NextEs, NextVs),
  beam(O, EsVs_P, NextVs, Ps, AllVs, NewEs, AllEs).

%! betweenness(+Graph, :V_P, :E_P, :N_P, -SortedPairs) is det.

betweenness(G, V_P, E_P, N_P, SortedPairs):-
  call(V_P, G, Vs1),
  rdf_global_id(rdfs:'Class', RDFS_Class),
  once(select(RDFS_Class, Vs1, Vs2)),
  map_list_to_pairs(betweenness(G, V_P, E_P, N_P), Vs2, Pairs1),
  call(E_P, G, Es),
  findall(
    Sum-V1/V2,
    (
      member(V1-V2, Es),
      member(X-V1, Pairs1),
      member(Y-V2, Pairs1),
      Sum is X + Y
    ),
    Pairs2
  ),
  sort([duplicates(true), inverted(true)], Pairs2, SortedPairs).

%! betweenness(
%!   +Graph,
%!   :V_P,
%!   :E_P,
%!   :N_P,
%!   +Vertex:vertex,
%!   -Betweenness:float
%! ) is det.
% Betweenness centrality.

betweenness(G, V_P, E_P, N_P, V, Betweenness):-
  call(V_P, G, Vs),
  findall(
    O,
    (
      member(From, To, Vs),
      shortest_paths(UG, E_P, N_P, From, To, _, GenericShortestPaths),
      length(GenericShortestPaths, NumberOfGenericShortestPaths),
      shortest_paths(UG, E_P, N_P, From, To, V, SpecificShortestPaths),
      length(SpecificShortestPaths, NumberOfSpecificShortestPaths),
      O is NumberOfSpecificShortestPaths / NumberOfGenericShortestPaths
    ),
    Os
  ),
  sum_list(Os, Betweenness).

bipartite(G, E_P, Vs1, Vs2):-
  call(E_P, G, Es),
  bipartite_(Es, [], Vs1, [], Vs2).

bipartite_([], Vs1, Vs1, Vs2, Vs2).
bipartite_([V-W | Es], H_S1, Vs1, H_S2, Vs2):-
  % For unordered graphs we only need to consider each edge in one direction.
  V > W, !,
  bipartite_(Es, H_S1, Vs1, H_S2, Vs2).
bipartite_([V-W | Es], H_S1, Vs1, H_S2, Vs2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite_(Es, New_H_S1, Vs1, New_H_S2, Vs2).
% Fit the edge either way.
bipartite_([W-V | Es], H_S1, Vs1, H_S2, Vs2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite_(Es, New_H_S1, Vs1, New_H_S2, Vs2).

%! component(:V_P, :E_P, ?Component, +Graph) is nondet.
% Succeeds of the former graph is a component of the latter.
%
% *Definition*: A component is a maximally connected subgraph.

component(V_P, E_P, C, G):-
  subgraph(V_P, E_P, C, G),
  connected(V_P, E_P, C),
  \+((
    subgraph(V_P, E_P, D, G),
    strict_subgraph(V_P, E_P, C, D),
    connected(V_P, E_P, D)
  )).

%! connected(:V_P, :E_P, +Graph) is semidet.
% Succeeds if the given graph is connected.
%
% *Definition*: A graph is connected if all pairs of vertices are connected.
%
% *Definition*: Two vertices are connected if there is a path between them.
% So vertice connectedness is just path existence.

connected(V_P, E_P, G):-
  call(V_P, G, Vs),
  forall(
    member(V1, V2, Vs),
    % A path connects vertices V1 and V2.
    travel([unique_vertex(true)], G, V_P, E_P, V1, V2, _Distance)
  ).

%! cubic(:V_P, +Graph) is semidet.
% Succeeds if the given graph is cubic.
%
% *Definition*: A cubic graph is a 3-regular graph.

cubic(V_P, G):-
  regular(V_P, G, 3).

%! degree(+Graph, +Vertex:vertex, -Degree:integer) is det.
% Returns the degree of the given vertex.
%
% @arg Options A list of name-value pairs.
%        1. =graph(Graph:ugraph)=
%           Supported: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Supported for: RDF.

degree(G, V, Degree):-
  neighbors(G, V, Ns),
  length(Ns, Degree).

%! degree_sequence(+Graph, :V_P, -DegreeSequence:list(integer)) is det.
% Returns the degree sequence of the given graph.

degree_sequence(G, V_P, DegreeSequence):-
  call(V_P, G, Vs),
  findall(
    Degree,
    (
      member(V, Vs),
      degree(G, V, Degree)
    ),
    UnsortedDegreeSequence
  ),
  % Sorting from largest to smallest degree, including duplicates.
  sort(
    [duplicates(true), inverted(true)],
    UnsortedDegreeSequence,
    DegreeSequence
  ).

%! depth(
%!   +Options:list(nvpair),
%!   :N_P,
%!   +Vertex:vertex,
%!   +Depth:integer,
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge)
%! ) is det.
% Returns all vertices and edges that are found within the given depth
% distance from the given vertex.
%
% @arg Options A list of name-value pairs, consisting of the following:
%      1. `directed(boolean)`
%         Whether only outgoing or also incoming arcs are included
%         in the export.
%      2. `graph(Graph:graph)`
%      3. `in(Format:oneof([rdf,ugraph]))`

depth(O, N_P, V, Depth, Vs, Es):-
  if_then(
    nonvar(Depth),
    Depth > 0
  ),
  depth_(O, N_P, [V], Depth, [], Vs, [], Es).
depth_(_O, _N_P, Vs, 0, VerticesH, AllVs, AllEs, AllEs):- !,
  ord_union(VerticesH, Vs, AllVs).
depth_(O, N_P, CurrentVs, Depth, VerticesH, AllVs, EdgesH, AllEs):-
  setoff(
    V-N,
    (
      member(V, CurrentVs),
      call(N_P, V, N)
    ),
    CurrentEdges0
  ),
  ord_subtract(CurrentEdges0, EdgesH, CurrentEs),
  setoff(
    N,
    member(_V-N, CurrentEs),
    Ns
  ),
  ord_union(CurrentVs, VerticesH, NewVerticesH),
  ord_subtract(Ns, NewVerticesH, NextVs),
  NewDepth is Depth - 1,
  ord_union(EdgesH, CurrentEdges0, NewEdgesH),
  depth_(O, N_P, NextVs, NewDepth, NewVerticesH, AllVs, NewEdgesH, AllEs).

%! graphic(+Seq:list(integer)) is semidet.
% Succeeds if the given degree sequence represents a simple graph.
%
% *Definition*: A sequence is graphic if the integers correspond
%               to the degrees of a simple graph.
%
% Havel-Hakimi theorem: Consider a list $s = [d_1, \ldots, d_n]$ of $n$
% numbers in descending order. This list is graphic iff
% $s^* = [d_1^*, \ldots, d_n^*]$ of $n - 1$ numbers is graph as well, where
% $d_i^* =
%          d_{i + 1} - 1, \text{for} i = 1, \ldots, d_1
%          d_{i + 1}, \text{otherwise}$

graphic(Zeros):-
  repeating_list(0, _Length, Zeros), !.
graphic([H | T]):-
  length(T, LT),
  H =< LT,
  length_cut(T, H, T1, T2),
  maplist(succ, NewT1, T1),
  append(NewT1, T2, NewT_),
  sort([duplicates(true), inverted(true)], NewT_, NewT),
  graphic(NewT).

%! has_cycle(:V_P, :E_P, +G) is semidet.
% Succeeds if the given graph has a cycle.

has_cycle(V_P, E_P, G):-
  call(V_P, G, Vs),
  member(FirstLast, Vs),
  travel(
    [closed(true), unique_edge(true), unique_vertex(true)],
    G,
    V_P,
    E_P,
    FirstLast,
    FirstLast,
    _Distance
  ), !.

%! is_undirected(:E_P, +Graph) is semidet.
% Succeeds if the given graph could be undirected.
%
% An undirected graph is represented as a ugraph that has a symmerical
% closure over its edges.
%
% Every undirected graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is undirected. This depends on the
% intention of the programmer, since a directed graph may have symmetric
% closure of its edges as well.

is_undirected(E_P, G):-
  call(E_P, G, Es),
  forall(
    member(V-W, Es),
    member(W-V, Es)
  ).

%! regular(:V_P, +Graph) is semidet.
% Succeeds if the graph is regular.
%
% *Definition*: In a regular graph each vertex has the same degree.

regular(V_P, G):-
  regular(V_P, G, _K).

%! regular(:V_P, +Graph, -K:integer) is semidet.
% Returns the degree of the given graph, if it is regular.
%
% *Definition*: A graph is regular if all its vertices have the same degree.

regular(V_P, G, K):-
  call(V_P, G, [V1 | Vs]),
  degree(G, V1, K),
  forall(
    member(V2, Vs),
    degree(G, V2, K)
  ).

%! shortest_paths(
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +From:vertex,
%!   +To:vertex,
%!   ?Pass:vertex,
%!   -ShortestPaths:list(path)
%! ) is det.
% Returns the shortest paths in graph Graph from vertex `From` to vertex `To`,
% passing vertex `Pass`.
%
% @arg Pass This one is optional. "Wir haben ein Abstich gemacht."

shortest_paths(G, E_P, N_P, From, To, Pass, ShortestPaths):-
  setoff(
    Length-Path,
    (
      travel([unique_vertex(true)], G, E_P, N_P, From, To, Length, Vs, _Es, Path),
      member(Pass, Vs)
    ),
    KeyValuePairs
  ),
  group_pairs_by_key(KeyValuePairs, Joined),
  (
    Joined == []
  ->
    ShortestPaths = []
  ;
    Joined = [_ShortestDistance-ShortestPaths | _]
  ).

%! simple(:V_P, :E_P, +Graph) is semidet.
% Generates simple graphs.
%
% *Definition*: A simple graph has no doubly occurring edges and no loops.

simple(V_P, E_P, G):-
  \+ has_cycle(V_P, E_P, G).

%! strict_subgraph(:V_P, :E_P, ?SubGraph, +Graph) is nondet.

strict_subgraph(V_P, E_P, SubG, G):-
  subgraph(V_P, E_P, SubG, G),
  SubG \== G.

%! subgraph(:V_P, :E_P, +SubGraph:graph, +Graph:graph) is semidet.

subgraph(V_P, E_P, SubG, G):-
  % Vertices.
  call(V_P, SubG, SubVs),
  call(V_P, G, Vs),
  ord_subset(SubVs, Vs),
  
  % Edges.
  call(E_P, SubG, SubEs),
  call(E_P, G, Es),
  ord_subset(SubEs, Es).

%! travel(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -Distance:uinteger
%! ) is nondet.
% @see Wrapper around travel/10.

travel(O, G, E_P, N_P, First, Last, Distance):-
  travel(O, G, E_P, N_P, First, Last, Distance, _Vertexs, _Edges, _History).

%! travel(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -Distance:integer
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge),
%!   -History:list
%! ) is nondet.
% Lets travel through graph land.
%
% A *walk* is an alternating sequence of vertices and edges, ending in a
% vertex.
%
% A *tour* is a closed walk. Closed means that the first and the last element
% in the sequence are the same vertex.
% Options = [closed(true)]
%
% A *trail* is a walk with unique edges.
% Options = [unique_edge(true)]
%
% A *path* is a walk with unique vertices.
% Options = [unique_vertex(true)]
%
% A *cycle* is a closed path trail.
% Options = [closed(true), unique_edge(true), unique_vertex(true)]
%
% An *Euler tour* is a tour in which all edges are traversed exactly one.
% Options = [closed(true), every_edge(true), unique_edge(true)]
%
% @arg Options A list of name-value pairs. The following options are
%        defined:
%        1. =|closed(boolean)|=
%        2. =|distance(oneof([edge,vertex]))|= For statiscs we return either
%           the number of edges or the number of vertices that were traversed.
%           Default: =edge=.
%        3. =|euler(boolean)|=
%        4. =|every_edge(boolean)|=
%        5. =|every_vertex(boolean)|=
%        6. =|graph(Graph)|=
%        7. =|unique_edge(boolean)=
%        8. =|unique_vertex(boolean)=
% @arg Graph
% @arg E_P
% @arg N_P
% @arg First The first vertex in the path.
% @arg Last The last vertex in the path.
% @arg Distance An integer representing a distance between the first and
%      the last vertex, counted as the number of traversed edges.
% @arg Vertices A list of vertices.
% @arg Edges A list of edges.
% @arg History

travel(O, G, E_P, N_P, First, Last, Distance, Vertices, Edges, History):-
  travel_(
    O, G, E_P, N_P, First, Last,
    Distance, [First], Vertices, [], Edges, History
  ).

travel_(
  O, G, E_P, _N_P, Last, Last, Distance, SolV, SolV, SolE, SolE, [Last]
):-
  % Check whether this is a tour, i.e., whether the walk is closed.
  if_then(
    option(close(true), O, false),
    % The first and the last vertex must be the same.
    last(SolV, Last)
  ),

  % Check whether this is an Euler, i.e., all edges were visited.
  if_then(
    option(every_edge(true), O, false),
    (
      call(E_P, G, AllEs),
      ord_subtract(AllEs, SolE, UntraversedEdges),
      ord_empty(UntraversedEdges)
    )
  ),

  % Distance metric. The statistics we return.
  option(distance(DistanceMetric), O, edge),
  if_then(
    DistanceMetric == edge,
    length(SolE, Distance)
  ),
  if_then(
    DistanceMetric == vertex,
    length(SolV, Distance)
  ), !.
travel_(
  O, G, E_P, N_P, FirstV, Last,
  Distance, Vs, SolV, Es, SolE, [FirstV, FirstV-NextV | History]
):-
  % Neighbor
  call(N_P, FirstV, NextV),

  % Check the walk restriction: no duplicate vertices.
  if_then(
    option(unique_vertex(true), O, false),
    \+ member(NextV, Vs)
  ),

  % Check the Euler restriction: no duplicate edges.
  if_then(
    option(trail(true), O, false),
    \+ member(FirstV-NextV, Es)
  ),

  ord_add_element(Vs, NextV, NewVs),
  ord_add_element(Es, FirstV-NextV, NewEs),
  travel_(
    O, G, E_P, N_P, NextV, Last, Distance, NewVs, SolV, NewEs, SolE, History
  ).

%! tarvel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -MinimumDistance:integer
%! ) is det.
% @see Wrapper around travel_min/10.

travel_min(O, G, E_P, N_P, First, Last, MinimumDistance):-
  travel_min(O, G, E_P, N_P, First, Last, MinimumDistance, _Vs, _Es, _History).

%! travel_min(
%!   +Options:list(nvpair),
%!   +Graph,
%!   :E_P,
%!   :N_P,
%!   +First:vertex,
%!   +Last:vertex,
%!   -MinimumDistance:integer,
%!   -Vertices:ordset(vertex),
%!   -Edges:ordset(edge),
%!   -History:list
%! ) is det.
% Returns the minimum distance between the given subject and predicate terms.
%
% @arg Options A list of name-value pairs.
%      See travel/7 for the list of supported options.
% @arg Graph
% @arg E_P
% @arg N_P
% @arg First A vertex, the first in the travel.
% @arg Last A respource, the last in the travel.
% @arg MinimumDistance An integer representing the minimum distance
%      between the first and last vertex. The kind of distances is set
%      in =Options=.
% @arg Vertices An ordered set of vertices.
% @arg Edges An ordered set of Edges.
% @arg History A list representing a minimum travel between the first and
%      last resources.

travel_min(O, G, E_P, N_P, First, Last, MinimumDistance, Vs, Es, History):-
  setoff(
    Distance-History,
    travel(O, G, E_P, N_P, First, Last, Distance, Vs, Es, History),
    Pairs
  ),
  first(Pairs, MinimumDistance-History).
