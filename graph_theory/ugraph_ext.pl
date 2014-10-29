:- module(
  ugraph_ext,
  [
    ugraph_complete/1, % +Graph:ugraph
    ugraph_complete/2, % +Vs:ordset
                       % ?Graph:ugraph
    ugraph_harary/3, % +K:integer
                     % +N:integer
                     % -H:ugraph
    ugraph_line_graph/2, % +Graph:ugraph
                         % -LineG:ugraph
    ugraph_maximum_components/2, % +Graph:ugraph
                                 % -MaximumComponents:list(ugraph)
    ugraph_unsymmetric_edges/2 % +Graph:ugraph
                               % -UnsymmetricEdges:ordset(edge)
  ]
).

/** <module> UGRAPH_EXT

@author Wouter Beek
@version 2012/09-2013/02, 2013/07, 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(ordsets)).

:- use_module(generics(list_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(math(math_ext)).



%! ugraph_complete(+Graph:ugraph) is semidet.
% Succeeds for complete graphs.
%
% @see Wrapper around ugraph_complete/2.

ugraph_complete(UG):-
  ugraph_vertices(UG, Vs),
  ugraph_complete(Vs, UG).

%! ugraph_complete(+Vs:ordset, ?Graph:ugraph) is semidet.
% Succeeds if the given graph is complete, or generates the complete graph
% from the given vertices.
%
% *Definition*: A complete graph is one in which all different vertices
%               are connected.
%
% @arg Vs An ordered set of vertices.
% @arg Graph A ugraph, i.e., a list of S-expressions.

ugraph_complete(Vs, UG):-
  ugraph_complete_(Vs, Vs, UG).

ugraph_complete_(_Vs, [], []).
ugraph_complete_(Vs, [FromV | FromVs], [FromV-ToVs | UG]):-
  ord_del_element(Vs, FromV, ToVs),
  ugraph_complete_(Vs, FromVs, UG).


%! ugraph_edge_induced_subgraph(
%!   +Graph:ugraph,
%!   +ESubG:list(edge),
%!  -SubG:ugraph
%! ) is det.
% Returns the edge-induced subgraph.

ugraph_edge_induced_subgraph(G, ESubG, SubG):-
  ugraph_edges(G, Es),
  ord_subtract(Es, ESubG, DelEs),
  del_edges(G, DelEs, SubG).

%! ugraph_harary(+K:integer, +N:integer, -H:ugraph) is det.
% Generates a Harary graph that is K-connected and that has N vertices.
%
% *Definition*: A Harary graph is a K-connected simple graph with
%               N vertices and the minimal number of edges.
%
% @arg K The connectedness of the Harary graph.
% @arg N The number of vertices.
% @arg H An undirected Harary graph.

ugraph_harary(K, N, H):-
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
ugraph_harary(K, N, H):-
  even(N), !,
  NewK is K - 1,
  ugraph_harary(NewK, N, Graph),
  ugraph_harary(Graph, =, N, H).
ugraph_harary(K, N, H):-
  NewK is K - 1,
  ugraph_harary(NewK, N, Graph),
  ugraph_harary(Graph, pred, N, H).

ugraph_harary(G, P, N, H):-
  call(P, N, NewN),
  findall(
    V-Ns,
    (
      member(V-Ms, G),
      W is V + (NewN / 2),
      (
        W =< N
      ->
        Ns = [V | Ms]
      ;
        Ns = Ms
      )
    ),
    H
  ).

%! ugraph_line_graph(+Graph:ugraph, -LineG:ugraph) is det.
% Returns the line graph for the given graph.
%
% *Definition*: The line graph G' of graph G has V(G') = E(G) and
%               $E(G') = \  {\tuple{\tuple{x, y},\tuple{y, z}} \vert
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

ugraph_line_graph(UG, LineG):-
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

%! ugraph_maximum_components(+G:ugraph, -MaxComps:list(ugraph)) is det.

ugraph_maximum_components(G, MaxComps):-
  ugraph_maximum_components_([G], MaxComps).
ugraph_maximum_components_([], []).
ugraph_maximum_components_([H | T], [H | Sol]):-
  connected(ugraph_vertices, ugraph_edges, H), !,
  ugraph_maximum_components_(T, Sol).
ugraph_maximum_components_([H | T], Sol):-
  findall(
    DSG,
    ugraph_direct_subgraph(DSG, H),
    DSGs
  ),
  append(T, DSGs, NewT),
  ugraph_maximum_components_(NewT, Sol).


%! ugraph_unsymmetric_edges(
%!   +Graph:ugraph,
%!   -UnsymmetricEdges:ordset(edge)
%! ) is det.
% Returns the unsymmetric edges for the given edges.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

ugraph_unsymmetric_edges(G, UnsymmetricEs):-
  ugraph_edges(G, Es),
  aggregate_all(
    set(V-W),
    (
      member(V-W, Es),
      V < W
    ),
    UnsymmetricEs
  ).

