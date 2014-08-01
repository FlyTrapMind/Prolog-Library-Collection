:- module(
  graph_theory,
  [
    adjacent/3, % +Graph:ugraph
                % ?Vertex1
                % ?Vertex2
    connected_component/2, % ?ConnectedComponent:ordset
                           % +Graph:ugraph
    degree/3, % +Graph:ugraph
              % ?Vertex
              % -Degree:nonneg
    degree_sequence/2, % +Graph:ugraph
                       % -DegreeSequence:list(nonneg)
    direct_subgraph/2, % ?Subgraph:ugraph
                       % +Graph:ugraph
    directed/1, % +Graph:ugraph
    head/2, % +Edge:pair
            % ?Vertex
    edge/2, % +Graph:ugraph
            % ?Edge:pair
    edge/3, % ?Edge:pair
            % ?Tail
            % ?Head
    edge_induced_subgraph/3, % ?Subgraph:ugraph
                             % ?KeepEdges:list(pair)
                             % +Graph:ugraph
    edges_to_vertices/2, % +Edges:list(pair)
                         % -Vertices:ordset
    empty_graph/1, % ?EmptyGraph:ugraph
    endpoint/2, % +Edge:pair
                % ?Vertex
    graph/3, % ?Graph:ugraph
             % ?Vertices:ordset
             % ?Edges:ordset(pair)
    head/2, % +Edge:pair
            % ?Head
    link/2, % +Graph:ugraph
            % ?Link:pair
    loop/2, % +Graph:ugraph
            % ?Loop:pair
    maximum_degree/2, % +Graph:ugraph
                      % ?MaxDegree:nonneg
    minimum_degree/2, % +Graph:ugraph
                      % ?MinDegree:nonneg
    order/2, % +Graph:ugraph
             % ?Order:nonneg
    reachable/3, % +Graph:ugraph
                 % +FromVertex
                 % ?ToVertex
    size/2, % +Graph:ugraph
            % ?Size:nonneg
    strict_subgraph/2, % +StrictSubgraph:ugraph
                       % +Graph:ugraph
    subgraph/2, % ?Subgraph:ugraph
                % +Graph:ugraph
    tail/2, % +Edge:pair
            % ?Tail
    undirected/1, % +Graph:ugraph
    vertex/2, % +Graph:ugraph
              % ?Vertex
    vertex_induced_subgraph/3 % ?Subgraph:ugraph
                              % ?KeepVertices:list
                              % +Graph:ugraph
  ]
).

/** <module> Graph theory

Graph theory.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(lists), except([subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs), [reachable/3 as reachables]).

:- use_module(generics(sort_ext)).
:- use_module(logic(set_theory)).
:- use_module(pl(pl_mode)).



%! adjacent(+Graph:ugraph, +Vertex1, +Vertex2) is semidet.
%! adjacent(+Graph:ugraph, +Vertex1, -Vertex2) is nondet.
%! adjacent(+Graph:ugraph, -Vertex1, +Vertex2) is nondet.
%! adjacent(+Graph:ugraph, -Vertex1, -Vertex2) is nondet.

adjacent(Graph, Vertex1, Vertex2):-
  call_ground_as_semidet(adjacent0(Graph, Vertex1, Vertex2)).
adjacent0(Graph, Vertex1, Vertex2):-
  member(Vertex1-Vertices, Graph),
  member(Vertex2, Vertices).


%! connected_component(
%!   +ConnectedComponent:ordset,
%!   +Graph:ugraph
%! ) is semidet.
%! connected_component(
%!   -ConnectedComponent:ordset,
%!   +Graph:ugraph
%! ) is nondet.

:- dynamic(graph/2).
connected_component(CC, Graph):-
  graph(Graph, Vs0, Es0),
  replace_graph_components(Vs0, Es0),
  repeat,
  (graph([V|Vs], Es) -> true ; !, fail),
  connected_component(Vs, SolVs, Es, SolEs, [V], CC),
  replace_graph_components(SolVs, SolEs).

connected_component(Vs1, SolVs, Es1, SolEs, [H1|T], CC2):-
  % @tbd Use the fact that `Es1` is sorted.
  select(H1-H2, Es1, Es2), !,
  ord_del_element(Vs1, H2, Vs2),
  connected_component(Vs2, SolVs, Es2, SolEs, [H2,H1|T], CC1),
  ord_add_element(CC1, H2, CC2).
connected_component(Vs, Vs, Es, Es, [H], [H]):- !.
connected_component(Vs, SolVs, Es, SolEs, [_|T], CC):-
  connected_component(Vs, SolVs, Es, SolEs, T, CC).

replace_graph_components(Vs, Es):-
  retractall(graph(_,_)),
  assert(graph(Vs,Es)).


%! degree(+Graph:ugraph, +Vertex, -Degree:nonneg) is det.
%! degree(+Graph:ugraph, -Vertex, -Degree:nonneg) is nondet.

degree(Graph, Vertex, Degree2):-
  vertex(Graph, Vertex),
  neighbors(Vertex, Graph, Vertices),
  length(Vertices, Degree1),
  % Correct for reflexive edges.
  (
    memberchk(Vertex, Vertices)
  ->
    Degree2 is Degree1 + 1
  ;
    Degree2 = Degree1
  ).


%! degree_sequence(+Graph:ugraph, -DegreeSequence:list(nonneg)) is det.

degree_sequence(Graph, DegreeSeq):-
  vertices(Graph, Vertices),
  maplist(\Vertex^Degree^degree(Vertex, Graph, Degree), Vertices, Degrees),
  sort(Degrees, DegreeSeq, [duplicates(true),inverted(true)]).


%! direct_subgraph(+Subgraph:ugraph, +Graph:ugraph) is semidet.
%! direct_subgraph(-Subgraph:ugraph, +Graph:ugraph) is nondet.

direct_subgraph(Subgraph, Graph):-
  subgraph(Subgraph, Graph),
  Subgraph \== Graph.


%! directed(+Graph:ugraph) is semidet.
% Succeeds for some directed graphs.
%
% Every directed graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is directed. This depends on the
% intention of the programmer, since an directed graph may have symmetric
% closure of its edges merely by chance.

directed(Graph):-
  member(X-Ys, Graph),
  member(Y, Ys),
  \+((
    member(Y-Xs, Graph),
    member(X, Xs)
  )).


%! edge(+Graph:ugraph, +Edge:pair) is semidet.
%! edge(+Graph:ugraph, -Edge:pair) is nondet.

edge(Graph, X-Y):-
  member(X-Ys, Graph),
  member(Y, Ys).


%! edge(+Edge:pair, -Tail, -Head) is det.
%! edge(-Edge:pair, +Tail, +Head) is det.
% Relates an unnamed/untyped edge to its constituting vertices.

edge(Tail-Head, Tail, Head).


%! edge_induced_subgraph(
%!   +Subgraph:ugraph,
%!   +KeepEdges:list(pair),
%!   +Graph:ugraph
%! ) is semidet.
%! edge_induced_subgraph(
%!   +Subgraph:ugraph,
%!   -KeepEdges:list(pair),
%!   +Graph:ugraph
%! ) is semidet.
%! edge_induced_subgraph(
%!   -Subgraph:ugraph,
%!   +KeepEdges:list(pair),
%!   +Graph:ugraph
%! ) is nondet.
%! edge_induced_subgraph(
%!   -Subgraph:ugraph,
%!   -KeepEdges:list(pair),
%!   +Graph:ugraph
%! ) is nondet.

edge_induced_subgraph(Subgraph, KeepEdges1, Graph):-
  edges(Graph, Edges),
  list_to_ord_set(KeepEdges1, KeepEdges2),
  subset(KeepEdges2, Edges),
  ord_subtract(Edges, KeepEdges2, RemoveEdges),
  del_edges(Graph, RemoveEdges, Subgraph).


%! edges_to_vertices(+Edges:list(pair), -Vertices:ordset) is det.
% Returns the vertices that occur in the given edges.

edges_to_vertices([], []).
edges_to_vertices([E|Es], Vs3):-
  edges_to_vertices(Es, Vs1),
  edge(E, FromV, ToV),
  ord_add_element(Vs1, FromV, Vs2),
  ord_add_element(Vs2, ToV, Vs3).


%! empty_graph(+EmptyGraph:ugraph) is semidet.
%! empty_graph(-EmptyGraph:ugraph) is det.

empty_graph([]).


%! endpoint(+Edge:pair, -Vertex) is multi.
% For reflexive edges, the same vertex is returned twice.

endpoint(X-_, X).
endpoint(_-Y, Y).


%! graph(+Graph:ugraph, -Vertices:ordset, -Edges:ordset(pair)) is det.
%! graph(-Graph:ugraph, +Vertices:ordset, +Edges:ordset(pair)) is det.

graph(Graph, Vertices, Edges):-
  nonvar(Graph), !,
  vertices(Graph, Vertices),
  edges(Graph, Edges).
graph(Graph, Vertices, Edges):-
  vertices_edges_to_ugraph(Vertices, Edges, Graph).


%! head(+Edge:pair, +Head) is semidet.
%! head(+Edge:pair, -Head) is det.

head(_-Head, Head).


%! link(+Graph:ugraph, +Link:pair) is semidet.
%! link(+Graph:ugraph, -Link:pair) is nondet.

% Optimization for the semidet case where `Link` is a loop.
link(Graph, X-Y):-
  ground(X-Y), !,
  X \== Y,
  edge(Graph, X-Y).
link(Graph, X-Y):-
  edge(Graph, X-Y),
  X \== Y.


%! loop(+Graph:ugraph, +Loop:pair) is semidet.
%! loop(+Graph:ugraph, -Loop:pair) is nondet.

loop(Graph, X-X):-
  edge(Graph, X-X).


%! maximum_degree(+Graph:ugraph, +MaxDegree:nonneg) is semidet.
%! maximum_degree(+Graph:ugraph, -MaxDegree:nonneg) is det.

maximum_degree(Graph, MaxDegree):-
  aggregate_all(
    max(Degree),
    % Call degree/3 in generative mode.
    degree(_, Graph, Degree),
    MaxDegree
  ).


%! minimum_degree(+Graph:ugraph, +MinDegree:nonneg) is semidet.
%! minimum_degree(+Graph:ugraph, -MinDegree:nonneg) is det.

minimum_degree(Graph, MinDegree):-
  aggregate_all(
    min(Degree),
    % Call degree/3 in generative mode.
    degree(_, Graph, Degree),
    MinDegree
  ).


%! order(+Graph:ugraph, +Order:nonneg) is semidet.
%! order(+Graph:ugraph, -Order:nonneg) is det.
% The *order* of a graph is the cardinality of its vertices.

order(Graph, Order):-
  vertices(Graph, Vertices),
  length(Vertices, Order).


%! reachable(+Graph:ugraph, +FromVertex, +ToVertex) is semidet.
%! reachable(+Graph:ugraph, +FromVertex, -ToVertex) is nondet.

reachable(_, Vertex, Vertex).
reachable(Graph, FromVertex, ToVertex):-
  reachable(Graph, FromVertex, InbetweenVertex),
  adjacent(Graph, InbetweenVertex, ToVertex).


%! size(+Graph:ugraph, +Size:nonneg) is semidet.
%! size(+Graph:ugraph, -Size:nonneg) is det.
% The *size* of a graph is the cardinality of its edges.

size(Graph, Size):-
  edges(Graph, Edges),
  length(Edges, Size).


%! strict_subgraph(+StrictSubgraph:ugraph, +Graph:ugraph) is semidet.
%! strict_subgraph(-StrictSubgraph:ugraph, +Graph:ugraph) is nondet.

strict_subgraph(StrictSubgraph, Graph):-
  subgraph(StrictSubgraph, Graph),
  StrictSubgraph \== Graph.


%! subgraph(+Subgraph:ugraph, +Graph:ugraph) is semidet.
%! subgraph(-Subgraph:ugraph, +Graph:ugraph) is nondet.

subgraph(Subgraph, Graph):-
  graph(Graph, Vertices, Edges),
  subset(Subedges, Edges),
  graph(Subgraph, Vertices, Subedges).


%! tail(+Edge:pair, +Tail) is semidet.
%! tail(+Edge:pair, -Tail) is det.

tail(Tail-_, Tail).


%! undirected(+Graph:ugraph) is semidet.
% Succeeds if the given graph could be undirected.
%
% An undirected graph is represented as a ugraph that has a symmerical
% closure over its edges.
%
% Every undirected graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is undirected. This depends on the
% intention of the programmer, since a directed graph may have symmetric
% closure of its edges as well.

undirected(Graph):-
  \+ directed(Graph).


%! vertex(+Graph:ugraph, +Vertex) is semidet.
%! vertex(+Graph:ugraph, -Vertex) is nondet.

vertex(Graph, Vertex):-
  call_ground_as_semidet(vertex0(Graph, Vertex)).
vertex0(Graph, Vertex):-
  member(Vertex-_, Graph).


%! vertex_induced_subgraph(
%!   +Subgraph:ugraph,
%!   +Vertices:list,
%!   +Graph:ugraph
%! ) is semidet.
%! vertex_induced_subgraph(
%!   +Subgraph:ugraph,
%!   -Vertices:list,
%!   +Graph:ugraph
%! ) is semidet.
%! vertex_induced_subgraph(
%!   -Subgraph:ugraph,
%!   +Vertices:list,
%!   +Graph:ugraph
%! ) is nondet.
%! vertex_induced_subgraph(
%!   -Subgraph:ugraph,
%!   -Vertices:list,
%!   +Graph:ugraph
%! ) is nondet.

vertex_induced_subgraph(Subgraph, KeepVertices1, Graph):-
  vertices(Graph, Vertices),
  list_to_ord_set(KeepVertices1, KeepVertices2),
  subset(KeepVertices2, Vertices),
  ord_subtract(Vertices, KeepVertices2, RemoveVertices),
  del_vertices(Graph, RemoveVertices, Subgraph).

