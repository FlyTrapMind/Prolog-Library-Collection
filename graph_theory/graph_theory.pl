:- module(
  graph_theory,
  [
    adjacent/3, % +Graph:compound
                % ?Vertex1
                % ?Vertex2
    connected_component/2, % ?ConnectedComponent:ordset
                           % +Graph:compound
    degree/3, % +Graph:compound
              % ?Vertex
              % -Degree:nonneg
    degree_sequence/2, % +Graph:compound
                       % -DegreeSequence:list
    direct_subgraph/2, % ?Subgraph:compound
                       % +Graph:compound
    directed/1, % +Graph:compound
    edge/2, % +Graph:compound
            % ?Edge:pair
    edge_components/3, % ?Edge:compound
                       % ?FromVertex
                       % ?ToVertex
    edge_components/4, % ?Edge:compound
                       % ?FromVertex
                       % ?EdgeType
                       % ?ToVertex
    edge_induced_subgraph/3, % ?Subgraph:compound
                             % ?KeepEdges:ordset(pair)
                             % +Graph:compound
    edges_to_vertices/2, % +Edges:list(compound)
                         % -Vertices:ordset
    empty_graph/1, % ?EmptyGraph:compound
    endpoint/2, % +Edge:pair
                % ?Vertex
    graph_components/3, % ?Graph:compound
                        % ?Vertices:ordset
                        % ?Edges:ordset(pair)
    link/2, % +Graph:compound
            % ?Link:pair
    loop/2, % +Graph:compound
            % ?Loop:pair
    maximum_degree/2, % +Graph:compound
                      % ?MaxDegree:nonneg
    minimum_degree/2, % +Graph:compound
                      % ?MinDegree:nonneg
    order/2, % +Graph:compound
             % ?Order:nonneg
    reachable/3, % +Graph:compound
                 % +FromVertex
                 % ?ToVertex
    size/2, % +Graph:compound
            % ?Size:nonneg
    strict_subgraph/2, % +StrictSubgraph:compound
                       % +Graph:compound
    subgraph/2, % ?Subgraph:compound
                % +Graph:compound
    undirected/1, % +Graph:compound
    vertex/2, % +Graph:compound
              % ?Vertex
    vertex_induced_subgraph/3 % ?Subgraph:compound
                              % ?KeepVertices:ordset
                              % +Graph:compound
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

:- use_module(generics(db_ext)).
:- use_module(generics(sort_ext)).
:- use_module(logic(set_theory)).
:- use_module(pl(pl_mode)).



%! adjacent(+Graph:atom, +Vertex1, +Vertex2) is semidet.
%! adjacent(+Graph:atom, +Vertex1, -Vertex2) is nondet.
%! adjacent(+Graph:atom, -Vertex1, +Vertex2) is nondet.
%! adjacent(+Graph:atom, -Vertex1, -Vertex2) is nondet.

adjacent(Graph, Vertex1, Vertex2):-
  call_ground_as_semidet(adjacent0(Graph, Vertex1, Vertex2)).
adjacent0(Graph, Vertex1, Vertex2):-
  member(Vertex1-Vertices, Graph),
  member(Vertex2, Vertices).


%! connected_component(
%!   +ConnectedComponent:ordset,
%!   +Graph:compound
%! ) is semidet.
%! connected_component(-ConnectedComponent:ordset, +Graph:compound) is nondet.

:- dynamic(graph_components/2).
connected_component(CC, Graph):-
  graph_components(Graph, Vs0, Es0),
  replace_graph_components(Vs0, Es0),
  repeat,
  (graph_components([V|Vs], Es) -> true ; !, fail),
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
  retractall(graph_components(_,_)),
  assert(graph_components(Vs,Es)).


%! degree(+Graph:compound, +Vertex, -Degree:nonneg) is det.
%! degree(+Graph:compound, -Vertex, -Degree:nonneg) is nondet.

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


%! degree_sequence(+Graph:compound, -DegreeSequence:list) is det.

degree_sequence(Graph, DegreeSeq):-
  vertices(Graph, Vertices),
  maplist(\Vertex^Degree^degree(Vertex, Graph, Degree), Vertices, Degrees),
  sort(Degrees, DegreeSeq, [duplicates(true),inverted(true)]).


%! direct_subgraph(+Subgraph:compound, +Graph:compound) is semidet.
%! direct_subgraph(-Subgraph:compound, +Graph:compound) is nondet.

direct_subgraph(Subgraph, Graph):-
  subgraph(Subgraph, Graph),
  Subgraph \== Graph.


%! directed(+Graph:compound) is semidet.
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


%! edge(+Graph:compound, +Edge:pair) is semidet.
%! edge(+Graph:compound, -Edge:pair) is nondet.

edge(Graph, X-Y):-
  member(X-Ys, Graph),
  member(Y, Ys).


%! edge_components(+Edge:compound, -FromVertex, -ToVertex) is det.
%! edge_components(-Edge:compound, +FromVertex, +ToVertex) is det.
% Relates an unnamed/untyped edge to its constituting vertices.

edge_components(Edge, FromVertex, ToVertex):-
  edge_components(Edge, FromVertex, _, ToVertex).

%! edge_components(+Edge:compound, -FromVertex, -EdgeType, -ToVertex) is det.
%! edge_components(-Edge:compound, +FromVertex, ?EdgeType, +ToVertex) is det.
% Relates a named/typed edge to its constituting vertices and type.

edge_components(FromV-EdgeType-ToV, FromV, EdgeType, ToV):-
  nonvar(EdgeType), !.
edge_components(FromV-ToV, FromV, EdgeType, ToV):-
  var(EdgeType).


%! edge_induced_subgraph(
%!   +Subgraph:compound,
%!   +KeepEdges:ordset(pair),
%!   +Graph:compound
%! ) is semidet.
%! edge_induced_subgraph(
%!   +Subgraph:compound,
%!   -KeepEdges:ordset(pair),
%!   +Graph:compound
%! ) is semidet.
%! edge_induced_subgraph(
%!   -Subgraph:compound,
%!   +KeepEdges:ordset(pair),
%!   +Graph:compound
%! ) is nondet.
%! edge_induced_subgraph(
%!   -Subgraph:compound,
%!   -KeepEdges:ordset(pair),
%!   +Graph:compound
%! ) is nondet.

edge_induced_subgraph(Subgraph, KeepEdges, Graph):-
  edges(Graph, Edges),
  subset(KeepEdges, Edges),
  ord_subtract(Edges, KeepEdges, RemoveEdges),
  del_edges(Graph, RemoveEdges, Subgraph).


%! edges_to_vertices(+Edges:list(compound), -Vertices:ordset) is det.
% Returns the vertices that occur in the given edges.

edges_to_vertices([], []).
edges_to_vertices([E|Es], Vs3):-
  edges_to_vertices(Es, Vs1),
  edge_components(E, FromV, ToV),
  ord_add_element(Vs1, FromV, Vs2),
  ord_add_element(Vs2, ToV, Vs3).


%! empty_graph(+EmptyGraph:compound) is semidet.
%! empty_graph(-EmptyGraph:compound) is det.

empty_graph([]).


%! endpoint(+Edge:pair, -Vertex) is multi.
% For reflexive edges, the same vertex is returned twice.

endpoint(X-_, X).
endpoint(_-Y, Y).


%! graph_components(
%!   +Graph:compound,
%!   -Vertices:ordset,
%!   -Edges:ordset(pair)
%! ) is det.
%! graph_components(
%!   -Graph:compound,
%!   +Vertices:ordset,
%!   +Edges:ordset(pair)
%! ) is det.

graph_components(Graph, Vertices, Edges):-
  nonvar(Graph), !,
  vertices(Graph, Vertices),
  edges(Graph, Edges).
graph_components(Graph, Vertices, Edges):-
  vertices_edges_to_ugraph(Vertices, Edges, Graph).


%! link(+Graph:compound, +Link:pair) is semidet.
%! link(+Graph:compound, -Link:pair) is nondet.

% Optimization for the semidet case where `Link` is a loop.
link(Graph, X-Y):-
  ground(X-Y), !,
  X \== Y,
  edge(Graph, X-Y).
link(Graph, X-Y):-
  edge(Graph, X-Y),
  X \== Y.


%! loop(+Graph:compound, +Loop:pair) is semidet.
%! loop(+Graph:compound, -Loop:pair) is nondet.

loop(Graph, X-X):-
  edge(Graph, X-X).


%! maximum_degree(+Graph:compound, +MaxDegree:nonneg) is semidet.
%! maximum_degree(+Graph:compound, -MaxDegree:nonneg) is det.

maximum_degree(Graph, MaxDegree):-
  aggregate_all(
    max(Degree),
    % Call degree/3 in generative mode.
    degree(_, Graph, Degree),
    MaxDegree
  ).


%! minimum_degree(+Graph:compound, +MinDegree:nonneg) is semidet.
%! minimum_degree(+Graph:compound, -MinDegree:nonneg) is det.

minimum_degree(Graph, MinDegree):-
  aggregate_all(
    min(Degree),
    % Call degree/3 in generative mode.
    degree(_, Graph, Degree),
    MinDegree
  ).


%! order(+Graph:compound, +Order:nonneg) is semidet.
%! order(+Graph:compound, -Order:nonneg) is det.
% The *order* of a graph is the cardinality of its vertices.

order(Graph, Order):-
  vertices(Graph, Vertices),
  length(Vertices, Order).


%! reachable/3(+Graph:compound, +FromVertex, +ToVertex) is semidet.
%! reachable/3(+Graph:compound, +FromVertex, -ToVertex) is nondet.

reachable(_, Vertex, Vertex).
reachable(Graph, FromVertex, ToVertex):-
  reachable(Graph, FromVertex, InbetweenVertex),
  adjacent(Graph, InbetweenVertex, ToVertex).


%! size(+Graph:compound, +Size:nonneg) is semidet.
%! size(+Graph:compound, -Size:nonneg) is det.
% The *size* of a graph is the cardinality of its edges.

size(Graph, Size):-
  edges(Graph, Edges),
  length(Edges, Size).


%! strict_subgraph(+StrictSubgraph:compound, +Graph:compound) is semidet.
%! strict_subgraph(-StrictSubgraph:compound, +Graph:compound) is nondet.

strict_subgraph(StrictSubgraph, Graph):-
  subgraph(StrictSubgraph, Graph),
  StrictSubgraph \== Graph.


%! subgraph(+Subgraph:compound, +Graph:compound) is semidet.
%! subgraph(-Subgraph:compound, +Graph:compound) is nondet.

subgraph(Subgraph, Graph):-
  graph_components(Graph, Vertices, Edges),
  subset(Subedges, Edges),
  graph_components(Subgraph, Vertices, Subedges).


%! undirected(+Graph:compound) is semidet.
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


%! vertex(+Graph:compound, +Vertex) is semidet.
%! vertex(+Graph:compound, -Vertex) is nondet.

vertex(Graph, Vertex):-
  call_ground_as_semidet(vertex0(Graph, Vertex)).
vertex0(Graph, Vertex):-
  member(Vertex-_, Graph).


%! vertex_induced_subgraph(
%!   +Subgraph:compound,
%!   +Vertices:ordset,
%!   +Graph:compound
%! ) is semidet.
%! vertex_induced_subgraph(
%!   +Subgraph:compound,
%!   -Vertices:ordset,
%!   +Graph:compound
%! ) is semidet.
%! vertex_induced_subgraph(
%!   -Subgraph:compound,
%!   +Vertices:ordset,
%!   +Graph:compound
%! ) is nondetdet.
%! vertex_induced_subgraph(
%!   -Subgraph:compound,
%!   -Vertices:ordset,
%!   +Graph:compound
%! ) is nondet.

vertex_induced_subgraph(Subgraph, KeepVertices, Graph):-
  vertices(Graph, Vertices),
  subset(KeepVertices, Vertices),
  ord_subtract(Vertices, KeepVertices, RemoveVertices),
  del_vertices(Graph, RemoveVertices, Subgraph).

