:- module(
  ugraph_ext,
  [
    bipartite/3, % +Graph:ugraph
                 % -S1:list(vertex)
                 % -S2:list(vertex)
    complete/1, % +Graph:ugraph
    complete/2, % +VG:list(vertex)
                % ?Graph:ugraph
    component/2, % +C:ugraph
                 % +Graph:ugraph
    edge_induced_subgraph/3, % +Graph:ugraph
                             % +ESubG:list(edge)
                             % -SubG:ugraph
    harary/3, % +K:integer
              % +N:integer
              % -H:ugraph
    line_graph/2, % +Graph:ugraph
                  % -LineG:ugraph
    ugraph/1, % +Graph:ugraph
    ugraph_direct_subgraph/2, % ?SubGraph:ugraph
                              % +Graph:ugraph
    ugraph_edge/2, % +Options:list(nvpair)
                   % ?Edge:edge
    ugraph_empty/1, % ?Graph:ugraph
    ugraph_maximum_components/2, % +Graph:ugraph
                                 % -MaximumComponent:ugraph
    ugraph_neighbor/3, % ?Vertex:vertex
                       % ?Graph:atom
                       % ?Neighbor:vertex
    ugraph_subgraph/2, % ?G1:ugraph
                       % +G2:ugraph
    ugraph_vertex/2, % +Options:list(nvpair)
                     % ?Vertex:vertex
    ugraph_vertex_induced_subgraph/3 % +Graph:ugraph
                                     % +VSubG:list(vertex)
                                     % -SubG:ugraph
  ]
).
:- reexport(
  library(ugraphs),
  [
    add_edges/3 as ugraph_add_edges, % +Graph
                                     % +Edges
                                     % -NewGraph
    add_vertices/3 as ugraph_add_vertices, % +Graph
                                           % +Vertices
                                           % -NewGraph
    complement/2 as ugraph_complement, % +Graph
                                       % -NewGraph
    compose/3 as ugraph_compose, % +LeftGraph
                                 % +RightGraph
                                 % -NewGraph
    del_edges/3 as ugraph_del_edges, % +Graph
                                     % +Edges
                                     % -NewGraph
    del_vertices/3 as ugraph_del_vertices, % +Graph
                                           % +Vertices
                                           % -NewGraph
    edges/2 as ugraph_edges, % +Graph
                             % -Edges
    neighbors/3 as ugraph_neighbors, % +Vertex
                                     % +Graph
                                     % -Vertices
    reachable/3 as ugraph_reachable, % +Vertex
                                     % +Graph
                                     % -Vertices
    top_sort/2 as ugraph_top_sort, % +Graph
                                   % -Sort
    top_sort/3 as ugraph_top_sort, % +Graph
                                   % -Sort0
                                   % -Sort
    transitive_closure/2 as ugraph_transitive_closure, % +Graph
                                                       % -Closure
    transpose/2 as ugraph_transpose, % +Graph
                                     % -NewGraph
    vertices/2 as ugraph_vertices, % +Graph
                                   % -Vertices
    vertices_edges_to_ugraph/3 as ugraph_vertices_edges_to_ugraph, % +Vertices
                                                                   % +Edges
                                                                   % -Graph
    ugraph_union/3 % +Graph1
                   % +Graph2
                   % -Graph
  ]
).

/** <module> UGRAPH_EXT

Methods that extend the SWI-Prolog builtin library for undirected graphs.

This uses the SWI-Prolog library =ugraph=, originally
written by Richard O'Keefe. Also implemented in YAP and
SICStus Prolog.

Searching for a pair takes $2|V(G)|$ instead of $|V(G)|^2$.

All edges $\tuple{v, w}$ occur twice in a ugraph,
i.e. $v-[\ldots, w, \ldots]$ and $w-[\ldots, v, \ldots]$.

# Types

## 2D vertex

A 2D vertex (or =|vertice_coorinate|=) is a compound term representing
a 2D representation of a vertex.
It has the form =|vertex_coordinate(<vertive>, <2d_coordinate>)|=.

@author Wouter Beek
@version 2012/09-2013/02, 2013/07
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext), except([complete/3])).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(lists)).
:- use_module(math(math_ext)).

:- meta_predicate harary(+,2,+,-).



bipartite(Graph, S1, S2):-
  ugraphs:edges(Graph, EG),
  bipartite0(EG, [], S1, [], S2).

bipartite0([], S1, S1, S2, S2).
bipartite0([V-W | EG], H_S1, S1, H_S2, S2):-
  % For unordered graphs we only need to consider each edge in one direction.
  V > W,
  !,
  bipartite0(EG, H_S1, S1, H_S2, S2).
bipartite0([V-W | EG], H_S1, S1, H_S2, S2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite0(EG, New_H_S1, S1, New_H_S2, S2).
% Fit the edge either way.
bipartite0([W-V | EG], H_S1, S1, H_S2, S2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite0(EG, New_H_S1, S1, New_H_S2, S2).

%! complete(+Graph:ugraph) is semidet.
% Succeeds for complete graphs.
%
% @see complete/2

complete(Graph):-
  ugraphs:vertices(Graph, VG),
  complete(VG, Graph).

%! complete(+VG:list(vertex), +Graph:ugraph) is semidet.
%! complete(+VG:list(vertex), -Graph:ugraph) is det.
% Succeeds if the given graph is complete, or generates the complete graph
% from the given vertices.
%
% *Definition*: A complete graph is one in which all different vertices
%               are connected.
%
% @arg VG An ordered set of vertices.
% @arg Graph A ugraph, i.e., a list of S-expressions.

complete(VG, Graph):-
  complete(VG, VG, Graph).

complete(_VG, [], []).
complete(VG, [V | Vs], [V-Ws | Graph]):-
  ord_del_element(VG, V, Ws),
  complete(VG, Vs, Graph).

ugraph_maximum_components(Graph, MaxComps):-
  ugraph_maximum_components0([Graph], MaxComps).

ugraph_maximum_components0([], []).
ugraph_maximum_components0([H | T], [H | Sol]):-
  connected(ugraph_vertices, ugraph_edges, H), !,
  ugraph_maximum_components0(T, Sol).
ugraph_maximum_components0([H | T], Sol):-
  findall(
    DSG,
    ugraph_direct_subgraph(DSG, H),
    DSGs
  ),
  append(T, DSGs, NewT),
  ugraph_maximum_components0(NewT, Sol).

ugraph_direct_subgraph(DirectSubGraph, Graph):-
  ugraphs:vertices(Graph, Vertices),
  ugraphs:edges(Graph, Edges1),
  nth0(_I, Edges1, V-W, Edges2),
  V > W,
  nth0(_J, Edges2, W-V, Edges3),
  vertices_edges_to_ugraph(Vertices, Edges3, DirectSubGraph).

%! ugraph_edge(+UG:ugraph, ?Edge:edge) is nondet.
% Edges in an undirected graph.
%
% @tbd Undirected behavior?

ugraph_edge(Graph, From-To):-
  ugraph_edges(Graph, Edges),
  member(From-To, Edges).

%! edge_induced_subgraph(
%!   +Graph:ugraph,
%!   +ESubG:list(edge),
%!  -SubG:ugraph
%! ) is det.
% Returns the edge-induced subgraph.

edge_induced_subgraph(Graph, ESubG, SubG):-
  ugraphs:edges(Graph, Es),
  ord_subtract(Es, ESubG, DelEs),
  del_edges(Graph, DelEs, SubG).

%! ugraph_empty(?Graph:ugraph) is semidet.
% Succeeds on the empty graph or returns the empty graph.

ugraph_empty([]).

%! harary(+K:integer, +N:integer, -H:ugraph) is det.
% Generates a Harary graph that is K-connected and that has N vertices.
%
% *Definition*: A Harary graph is a K-connected simple graph with
%               N vertices and the minimal number of edges.
%
% @arg K The connectedness of the Harary graph.
% @arg N The number of vertices.
% @arg H An undirected Harary graph.

harary(K, N, H):-
  even(K), !,
  V_Last is N - 1,
  numlist(0, V_Last, Vs),
  Half_K is K / 2,
  findall(
    V-Neighbors,
    (
      member(V, Vs),
      Min is (V - Half_K) mod N,
      Max is (V + Half_K) mod N,
      cyclic_numlist(Min, Max, N, Ns_),
      select(V, Ns_, Neighbors)
    ),
    H
  ).
harary(K, N, H):-
  even(N), !,
  NewK is K - 1,
  harary(NewK, N, Graph),
  harary(Graph, id, N, H).
harary(K, N, H):-
  NewK is K - 1,
  harary(NewK, N, Graph),
  harary(Graph, pred, N, H).

harary(Graph, Mod:P, N, H):-
  Call =.. [P, N, NewN],
  call(Mod:Call),
  findall(
    V-Neighbors,
    (
      member(V-Ms, Graph),
      W is V + (NewN / 2),
      (
        W =< N
      ->
        Neighbors = [V | Ms]
      ;
        Neighbors = Ms
      )
    ),
    H
  ).

is_ugraph(UG):-
  is_list(UG), maplist(is_ugraph_edge, UG).

is_ugraph_edge(V-Ws):-
  atomic(V), is_list(Ws).

%! line_graph(+Graph:ugraph, -LineG:ugraph) is det.
% Returns the line graph for the given graph.
%
% *Definition*: The line graph G' of graph G has V(G') = E(G) and
%               $E(G') = \setoff{\tuple{\tuple{x, y},\tuple{y, z}} \vert
%               \tuple{x, y}, \tuple{y, z} \in E(G)}$.
%
% *Representation*: Vertex $V \in V(LineG)$ that is based on edge
%                   $\tuple{X, Y} \in E(G)$ is represented in the following
%                   way: =|V = X/Y|=, where $X < Y$.
%
% *Restriction*: Line graphs can only be created for undirected graphs,
%                see undirected/1.
%
% *Restriction*: Line graphs can only consist of vertices that adhere to
%                the sorting relation <.
%
% @tbd Allow a comparator argument, so that vertices that do not compare
%      with < are allowed as well.

line_graph(UG, LineG):-
  ugraph_edges(UG, EG),
  findall(
    V/W-Neighbors,
    (
      member(V-W, EG),
      V < W,
      findall(
        X/Y,
        (
          member(X-Y, EG),
          X < Y,
          (
            X == V
          ;
            X == W
          ;
            Y == V
          ;
            Y == W
          ),
          X/Y \== V/W
        ),
        Neighbors
      )
    ),
    LineG
  ).

%! ugraph(+Graph:ugraph) is semidet.
% Succeeds if the given graph could be undirected.
%
% An undirected graph is represented as a ugraph that has a symmerical
% closure over its edges.
%
% Every undirected graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is undirected. This depends on the
% intention of the programmer, since a directed graph may have symmetric
% closure of its edges as well.

ugraph(UG):-
  ugraph_edges(UG, Es),
  forall(
    member(V-W, Es),
    member(W-V, Es)
  ).

%! ugraph_neighbor(+Vertex:vertex, +UG:ugraph, -Neighbor:vertex) is nondet.
% Neighboring vertex.

ugraph_neighbor(Vertex, UG, Neighbor):-
  ugraph_edge(UG, Vertex-Neighbor).

%! ugraph_subgraph(?G1:ugraph, +G2:ugraph) is nondet.
% G1 is a subgraph of G2.

ugraph_subgraph(G1, G2):-
  ugraph_subgraph0(G1, G2, [], []).
ugraph_subgraph0([], [], _In, _Out).
ugraph_subgraph0([V-V1s | G1], [V-V2s | G2], In, Out):-
  % Predicate used from LIST_EXT.
  sublist(V1s_, V2s),
  ord_subtract(V1s_, Out, V1s),
  ord_union(In, V1s, NewIn),
  ugraph_subgraph0(G1, G2, NewIn, Out).
ugraph_subgraph0(G1, [V-_Vs | G2], In, Out):-
  \+ member(V, In),
  ord_add_element(Out, V, NewOut),
  ugraph_subgraph0(G1, G2, In, NewOut).

%! ugraph_vertex(+G:ugraph, ?Vertex:vertex) is nondet.
% Vertices in a graph.

ugraph_vertex(G, V):-
  ugraph_vertices(G, Vs),
  member(V, Vs).

%! unsymmetric_edges(+Edges:list(edge), -UnsymmetricEdges:list(edge)) is det.
% Returns the unsymmetric edges for the given edges.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

unsymmetric_edges(Edges, UnsymmetricEdges):-
  findall(
    V-W,
    (
      member(V-W, Edges),
      V < W
    ),
    UnsymmetricEdges
  ).

%! ugraph_vertex_induced_subgraph(
%!   +Graph:ugraph,
%!   +VSubG:list(vertex),
%!   -SubG:ugraph
%! ) is det.
% Returns the vertex-induced subgraph.

ugraph_vertex_induced_subgraph(Graph, VSubG, SubG):-
  ugraphs:vertices(Graph, Vs),
  ord_subtract(Vs, VSubG, DelVs),
  del_vertices(Graph, DelVs, SubG).

