:- module(
  graph_generic,
  [
    bipartite/4, % +Graph
                 % :E_P
                 % -Vs1:ordset
                 % -Vs2:ordset
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
    depth/5, % :NeighborPred
             % +Vertex
             % +Depth:integer
             % -Vertices:ordset
             % -Edges:ordset(edge)
    edge_components/3, % +Edge:compound
                       % -FromVertex
                       % -ToVertex
    edge_components/4, % +Edge:compound
                       % -FromVertex
                       % -EdgeType
                       % -ToVertex
    edges_to_vertices/2, % +Edges:list(compound)
                         % -Vertices:ordset
    graphic_graph/1, % +S:list(integer)
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
    simple/3, % :V_P
              % :E_P
              % +Graph
    strict_subgraph/4, % :V_P
                       % :E_P
                       % ?StrictSubGraph
                       % ?Graph
    subgraph/4 % :V_P
               % :E_P
               % +SubGraph
               % +Graph
  ]
).

/** <module> GRAPH_GENERIC

Predicate that implement generic graph operations.

Generic operations are possible via meta-arguments for retrieving
the edges and vertices.

@author Wouter Beek
@tbd Compare shortest path and travel predicates two versions.
@version 2013/01-2013/04, 2013/07, 2014/03, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_traversal)).
:- use_module(pl(pl_control)).
:- use_module(ugraph(ugraph_ext)).

:- use_module(plRdf(rdf_graph_theory)).

:- meta_predicate(bipartite(+,2,-,-)).
:- meta_predicate(cubic(2,+)).
:- meta_predicate(component(2,2,?,+)).
:- meta_predicate(connected(2,2,+)).
:- meta_predicate(degree_sequence(+,2,-)).
:- meta_predicate(depth(2,+,+,-,-)).
:- meta_predicate(depth(2,+,+,-,+,-)).
:- meta_predicate(has_cycle(2,2,+)).
:- meta_predicate(is_undirected(2,+)).
:- meta_predicate(regular(2,+)).
:- meta_predicate(regular(2,+,-)).
:- meta_predicate(simple(2,2,+)).
:- meta_predicate(strict_subgraph(2,2,?,+)).
:- meta_predicate(subgraph(2,2,?,+)).

:- rdf_meta(degree(+,r,-)).
:- rdf_meta(depth(:,+,r,-,-)).



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
    traverse([unique_vertex(true)], G, V_P, E_P, V1, V2, _Distance)
  ).


%! cubic(:V_P, +Graph) is semidet.
% Succeeds if the given graph is cubic.
%
% *Definition*: A cubic graph is a 3-regular graph.

cubic(V_P, G):-
  regular(V_P, G, 3).


%! degree(+Graph, +Vertex:vertex, -Degree:integer) is det.
% Returns the degree of the given vertex.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=

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
%!   :NeighborPred,
%!   +Depth:nonneg,
%!   +SeedVertex,
%!   -Vertices:ordset,
%!   -Edges:ordset(compound)
%! ) is det.
% Returns all vertices and edges that are found within the given depth
% distance from the given vertex.
% This is the same as bounded breadth-first search.

depth(NeighborPred, Depth, V, Vs, Es):-
  depth(NeighborPred, Depth, [V], Vs, [], Es).

% Depth was reached.
depth(_, 0, Vs, Vs, Es, Es):- !.
depth(NeighborPred, Depth1, Vs1, Vs3, Es1, Es3):-
  aggregate_all(
    set(X-Y),
    (
      member(X, Vs1),
      call(NeighborPred, X, Y),
      \+ member(X-Y, Es1)
    ),
    NewEs
  ),
  edges_to_vertices(NewEs, NewVs),
  ord_union(Es1, NewEs, Es2),
  ord_union(Vs1, NewVs, Vs2),
  
  Depth2 is Depth1 - 1,
  depth(NeighborPred, Depth2, Vs2, Vs3, Es2, Es3).


%! edge_components(+Edge:compound, -FromVertex, -ToVertex) is det.
%! edge_components(-Edge:compound, +FromVertex, +ToVertex) is det.
% Relates an unnamed/untyped edge to its constituting vertices.

edge_components(Edge, FromVertex, ToVertex):-
  edge_components(Edge, FromVertex, _, ToVertex).

%! edge_components(+Edge:compound, -FromVertex, -EdgeType, -ToVertex) is det.
%! edge_components(-Edge:compound, +FromVertex, ?EdgeType, +ToVertex) is det.
% Relates a named/typed edge to its constituting vertices and type.

edge_components(FromV-EdgeType-ToV, FromV, EdgeType, ToV):-
  nonvar(EdgeType).
edge_components(FromV-ToV, FromV, EdgeType, ToV):-
  var(EdgeType).


%! edges_to_vertices(+Edges:list(compound), -Vertices:ordset) is det.
% Returns the vertices that occur in the given edges.

edges_to_vertices([], []).
edges_to_vertices([E|Es], Vs3):-
  edges_to_vertices(Es, Vs1),
  edge_components(E, FromV, ToV),
  ord_add_element(Vs1, FromV, Vs2),
  ord_add_element(Vs2, ToV, Vs3).


%! graphic_graph(+Seq:list(integer)) is semidet.
% Succeeds if the given degree sequence represents a simple graph.
%
% *Definition*: A sequence is graphic_graph if the integers correspond
%               to the degrees of a simple graph.
%
% Havel-Hakimi theorem: Consider a list $s = [d_1, \ldots, d_n]$ of $n$
% numbers in descending order. This list is graphic_graph iff
% $s^* = [d_1^*, \ldots, d_n^*]$ of $n - 1$ numbers is graph as well, where
% $d_i^* =
%          d_{i + 1} - 1, \text{for} i = 1, \ldots, d_1
%          d_{i + 1}, \text{otherwise}$

graphic_graph(Zeros):-
  repeating_list(0, _Length, Zeros), !.
graphic_graph([H | T]):-
  length(T, LT),
  H =< LT,
  length_cut(T, H, T1, T2),
  maplist(succ, NewT1, T1),
  append(NewT1, T2, NewT_),
  sort([duplicates(true), inverted(true)], NewT_, NewT),
  graphic_graph(NewT).

%! has_cycle(:V_P, :E_P, +G) is semidet.
% Succeeds if the given graph has a cycle.

has_cycle(V_P, E_P, G):-
  call(V_P, G, Vs),
  member(FirstLast, Vs),
  traverse(
    [closed(true),unique_edge(true),unique_vertex(true)],
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

