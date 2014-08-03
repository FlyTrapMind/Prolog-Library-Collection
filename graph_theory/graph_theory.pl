:- module(
  graph_theory,
  [
    adjacent_edges/3, % +Graph:ugraph
                      % ?Edge1:pair
                      % ?Edge2:pair
    adjacent_vertices/3, % +Graph:ugraph
                         % ?Vertex1
                         % ?Vertex2
/*
    chain/3, % +Graph:ugraph
             % ?StartVertex
             % ?EndVertex
    chain/4, % +Graph:ugraph
             % ?StartVertex
             % ?EndVertex
             % ?Path:list(pair)
*/
    connected_component/2, % ?ConnectedComponent:ordset
                           % +Graph:ugraph
/*
    cycle/1, % +Graph:ugraph
    cycle/2, % +Graph:ugraph
             % ?Order:nonneg
*/
    degree/2, % +Graph:ugraph
              % -Degree:nonneg
    degree/3, % +Graph:ugraph
              % ?Vertex
              % -Degree:nonneg
    degree_sequence/2, % +Graph:ugraph
                       % -DegreeSequence:list(nonneg)
    direct_subgraph/2, % ?Subgraph:ugraph
                       % +Graph:ugraph
    directed/1, % +Graph:ugraph
    dominating_vertex/2, % +Graph:ugraph
                         % ?Vertex
    head/2, % +Edge:pair
            % ?Vertex
    edge/2, % +Graph:ugraph
            % ?Edge:pair
    edge/3, % +Graph:ugraph
            % ?Tail
            % ?Head
    edge_components/3, % ?Edge:pair
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
    star/1, % +Graph:ugraph
    star/2, % +Graph:ugraph
            % ?Order:nonneg
    strict_subgraph/2, % +StrictSubgraph:ugraph
                       % +Graph:ugraph
    subgraph/2, % ?Subgraph:ugraph
                % +Graph:ugraph
    tail/2, % +Edge:pair
            % ?Tail
    undirected/1, % +Graph:ugraph
    vertex/2, % +Graph:ugraph
              % ?Vertex
    vertex_induced_subgraph/3, % ?Subgraph:ugraph
                               % ?KeepVertices:list
                               % +Graph:ugraph
    walk/3, % +Graph:ugraph
            % ?StartVertex
            % ?EndVertex
    walk/4 % +Graph:ugraph
           % ?StartVertex
           % ?EndVertex
           % ?Path:list(pair)
  ]
).

/** <module> Graph theory

Graph theory.

@author Wouter Beek
@version 2014/07-2014/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(lists), except([subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).
:- use_module(library(ugraphs), [reachable/3 as reachables]).

:- use_module(generics(pair_ext)).
:- use_module(generics(sort_ext)).
:- use_module(logic(set_theory)).
:- use_module(pl(pl_mode)).



%! adjacent_edges(+Graph:ugraph, +Edge1:pair, +Edge2:pair) is semidet.
%! adjacent_edges(+Graph:ugraph, +Edge1:pair, -Edge2:pair) is nondet.
%! adjacent_edges(+Graph:ugraph, -Edge1:pair, +Edge2:pair) is nondet.
%! adjacent_edges(+Graph:ugraph, -Edge1:pair, -Edge2:pair) is nondet.
% Edges are *adjacent* iff they share exactly one vertex.

adjacent_edges(Graph, X-Y1, X-Y2):-
  edge(Graph, X-Y1),
  edge(Graph, X-Y2),
  Y1 \== Y2.
adjacent_edges(Graph, X-Y1, Y2-X):-
  edge(Graph, X-Y1),
  edge(Graph, Y2-X),
  Y1 \== Y2.
adjacent_edges(Graph, Y1-X, X-Y2):-
  edge(Graph, Y1-X),
  edge(Graph, X-Y2),
  Y1 \== Y2.
adjacent_edges(Graph, Y1-X, Y2-X):-
  edge(Graph, Y1-X),
  edge(Graph, Y2-X),
  Y1 \== Y2.



%! adjacent_vertices(+Graph:ugraph, +Vertex1, +Vertex2) is semidet.
%! adjacent_vertices(+Graph:ugraph, +Vertex1, -Vertex2) is nondet.
%! adjacent_vertices(+Graph:ugraph, -Vertex1, +Vertex2) is nondet.
%! adjacent_vertices(+Graph:ugraph, -Vertex1, -Vertex2) is nondet.
% Vertices are *adjacent* iff they are the endpoints of the same edge.
%
% Notice that adjacency does not respect the directedness of edges.

adjacent_vertices(Graph, Vertex1, Vertex2):-
  call_ground_as_semidet((
    edge(Graph, Vertex1-Vertex2)
  ;
    edge(Graph, Vertex2-Vertex1)
  )).



%! chain(+Graph:ugraph, +StartVertex, +EndVertex) is semidet.
%! chain(+Graph:ugraph, -StartVertex, +EndVertex) is nondet.
%! chain(+Graph:ugraph, +StartVertex, -EndVertex) is nondet.
%! chain(+Graph:ugraph, -StartVertex, -EndVertex) is nondet.

chain(Graph, Start, End):-
  chain(Graph, Start, End, _).

%! chain(+Graph:ugraph, +StartVertex, +EndVertex, -Path:list(pair)) is nondet.
%! chain(+Graph:ugraph, +StartVertex, -EndVertex, -Path:list(pair)) is nondet.
%! chain(+Graph:ugraph, -StartVertex, +EndVertex, -Path:list(pair)) is nondet.
%! chain(+Graph:ugraph, -StartVertex, -EndVertex, -Path:list(pair)) is nondet.
% @tbd

chain(Graph, Start, End, Path):-
  walk(Graph, Start, End, Path).



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



/*
%! cycle(+Graph:ugraph) is semidet.

cycle(Graph):-
  cycle(Graph, _).

%! cycle(+Graph:ugraph, +Order:nonneg) is semidet.
%! cycle(+Graph:ugraph, -Order:nonneg) is semidet.

cycle(Graph, Order):-
  degree_sequence(Graph, [H|T]),
  maplist(\X^(X=:=H), T),
  length([H|T], Order).

:- begin_tests(cycle).

test('cycle(+,-) is semidet. TRUE', [forall(cycle(Graph,Order,true))]):-
  cycle(Graph, Order).

cycle([1-[2,3],2-[1,3],3-[1,2]], 3, true).
cycle([1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]], 4, true).

:- end_tests(cycle).
*/



%! degree(+Graph, -Degree:nonneg) is semidet.
% The *degree* of a graph is the degree of each of its vertices.
%
% This fails silently for non-regular graphs (which do not have a degree).

degree(Graph, Degree):-
  forall(
    vertex(Graph, Vertex),
    degree(Graph, Vertex, Degree)
  ).



%! degree(+Graph:ugraph, +Vertex, +Degree:nonneg) is semidet.
%! degree(+Graph:ugraph, +Vertex, -Degree:nonneg) is det.
%! degree(+Graph:ugraph, -Vertex, -Degree:nonneg) is nondet.
% The *degree* of a vertex is the summation, for each edge, of
% the number of times the vertex occurs in the edge.
%
% This means that a reflective edge, if present,
% is counted twice in calculating the vertex degree.

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
  maplist(\Vertex^Degree^degree(Graph, Vertex, Degree), Vertices, Degrees),
  sort(Degrees, DegreeSeq, [duplicates(true),inverted(true)]).



%! direct_subgraph(+Subgraph:ugraph, +Graph:ugraph) is semidet.
%! direct_subgraph(-Subgraph:ugraph, +Graph:ugraph) is nondet.
% S' is a *direct subgraph* of S iff S' is a subgraph of S
% and every strict subgraph S'' of S is also a subgraph of S'.

direct_subgraph(Subgraph, Graph):-
  vertices(Graph, Vertices),
  direct_subset(Subvertices, Vertices),
  vertex_induced_subgraph(Subgraph, Subvertices, Graph).



%! directed(+Graph:ugraph) is semidet.
% Succeeds for some directed graphs.
%
% Every directed graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is directed. This depends on the
% intention of the programmer, since an directed graph may have symmetric
% closure of its edges merely by chance.

directed(Graph):-
  edge(Graph, X-Y),
  \+(edge(Graph, Y-X)).



%! dominating_vertex(+Graph:ugraph, +Vertex) is semidet.
%! dominating_vertex(+Graph:ugraph, -Vertex) is nondet.
% A *dominating* vertex is one that is adjacent to every _other_ vertex.

dominating_vertex(Graph, Vertex):-
  vertices(Graph, Vertices1),
  select(Vertex, Vertices1, Vertices2),
  maplist(edge(Graph, Vertex), Vertices2).



%! edge(+Graph:ugraph, +Edge:pair) is semidet.
%! edge(+Graph:ugraph, -Edge:pair) is nondet.

edge(Graph, Tail-Head):-
  member(Tail-Heads, Graph),
  member(Head, Heads).

%! edge(+Graph:ugraph, +Tail, +Head) is semidet.
%! edge(+Graph:ugraph, +Tail, -Head) is nondet.
%! edge(+Graph:ugraph, -Tail, +Head) is nondet.
%! edge(+Graph:ugraph, -Tail, -Head) is nondet.

edge(Graph, Tail, Head):-
  edge(Graph, Tail-Head).



%! edge_components(+Edge:pair, -Tail, -Head) is det.
%! edge_components(-Edge:pair, +Tail, +Head) is det.
% Relates an unnamed/untyped edge to its constituting vertices.

edge_components(Edge, Tail, Head):-
  pair_ext:pair(Edge, Tail, Head).



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
  edge_components(E, FromV, ToV),
  ord_add_element(Vs1, FromV, Vs2),
  ord_add_element(Vs2, ToV, Vs3).



%! empty_graph(+EmptyGraph:ugraph) is semidet.
%! empty_graph(-EmptyGraph:ugraph) is det.

empty_graph([]).



%! endpoint(+Edge:pair, +Vertex) is semidet.
%! endpoint(+Edge:pair, -Vertex) is multi.
% For reflexive edges, the same vertex is returned twice.

endpoint(Edge, Vertex):-
  pair_ext:pair_element(Edge, Vertex).



%! graph(+Graph:ugraph, -Vertices:ordset, -Edges:ordset(pair)) is det.
%! graph(-Graph:ugraph, +Vertices:ordset, +Edges:ordset(pair)) is det.
% Decomposes/composes a graph into/based in its vertices and edges.

graph(Graph, Vertices, Edges):-
  nonvar(Graph), !,
  vertices(Graph, Vertices),
  edges(Graph, Edges).
graph(Graph, Vertices, Edges):-
  vertices_edges_to_ugraph(Vertices, Edges, Graph).



%! head(+Edge:pair, +Head) is semidet.
%! head(+Edge:pair, -Head) is det.
% The head of a directed edge is the vertex the edge is "pointing to".

head(Edge, Head):-
  pair_ext:pair(Edge, _, Head).



%! link(+Graph:ugraph, +Link:pair) is semidet.
%! link(+Graph:ugraph, -Link:pair) is nondet.
% A *link* is a non-reflexive edge.

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
% A *loop* is a reflexive edge.

loop(Graph, X-X):-
  edge(Graph, X-X).



%! maximum_degree(+Graph:ugraph, +MaxDegree:nonneg) is semidet.
%! maximum_degree(+Graph:ugraph, -MaxDegree:nonneg) is det.

maximum_degree(Graph, MaxDegree):-
  aggregate_all(
    max(Degree),
    % Call degree/3 in generative mode.
    degree(Graph, _, Degree),
    MaxDegree
  ).



%! minimum_degree(+Graph:ugraph, +MinDegree:nonneg) is semidet.
%! minimum_degree(+Graph:ugraph, -MinDegree:nonneg) is det.

minimum_degree(Graph, MinDegree):-
  aggregate_all(
    min(Degree),
    % Call degree/3 in generative mode.
    degree(Graph, _, Degree),
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
  adjacent_vertices(Graph, InbetweenVertex, ToVertex).



%! size(+Graph:ugraph, +Size:nonneg) is semidet.
%! size(+Graph:ugraph, -Size:nonneg) is det.
% The *size* of a graph is the cardinality of its edges.

size(Graph, Size):-
  edges(Graph, Edges),
  length(Edges, Size).



%! star(+Graph:ugraph) is semidet.

star(Graph):-
  star(Graph, _).

%! star(+Graph:ugraph, +Order:nonneg) is semidet.
%! star(+Graph:ugraph, -Order:nonneg) is semidet.

star(Graph, Order):-
  order(Graph, Order),
  degree_sequence(Graph, [Order|Ones]),
  maplist(\One^(One=:=1), Ones).



%! strict_subgraph(+StrictSubgraph:ugraph, +Graph:ugraph) is semidet.
%! strict_subgraph(-StrictSubgraph:ugraph, +Graph:ugraph) is nondet.
% S' is a *strict subgraph* of S iff S' is a subgraph of S and S' is not S.

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
% The tail of a directed edge is the vertex the edge is "pointing from".

tail(Edge, Tail):-
  pair_ext:pair(Edge, Tail, _).



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
  call_ground_as_semidet(member(Vertex-_, Graph)).



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



%! walk(+Graph:ugraph, +StartVertex, +EndVertex) is semidet.
%! walk(+Graph:ugraph, -StartVertex, +EndVertex) is nondet.
%! walk(+Graph:ugraph, +StartVertex, -EndVertex) is nondet.
%! walk(+Graph:ugraph, -StartVertex, -EndVertex) is nondet.
% Suceeds if a path between start and end exists,
% or enumerates the vertices that can be reached
%  from a given start or end vertex,
% or pairs of vertices between which a walk exists.

walk(Graph, Start, End):-
  walk(Graph, Start, End, _).

%! walk(+Graph:ugraph, +StartVertex, +EndVertex, -Path:list(pair)) is nondet.
%! walk(+Graph:ugraph, +StartVertex, -EndVertex, -Path:list(pair)) is nondet.
%! walk(+Graph:ugraph, -StartVertex, +EndVertex, -Path:list(pair)) is nondet.
%! walk(+Graph:ugraph, -StartVertex, -EndVertex, -Path:list(pair)) is nondet.

walk(Graph, Start, End, Path):-
  nonvar(Start), !,
  walk_forward(Graph, Start, End, [Start], Path).
walk(Graph, Start, End, Path):-
  nonvar(End), !,
  walk_backward(Graph, Start, End, [End], Path).
walk(Graph, Start, End, Path):-
  vertex(Graph, Start),
  walk(Graph, Start, End, Path).

walk_backward(_, Start, Start, _, []).
walk_backward(Graph, Start, End, Vs1, [InBetween-End|Path]):-
  % Make sure there are no loops or repetitions.
  edge(Graph, InBetween-End),
  \+ member(InBetween, Vs1),
  ord_add_element(Vs1, InBetween, Vs2),
  walk_backward(Graph, Start, InBetween, Vs2, Path).

walk_forward(_, End, End, _, []).
walk_forward(Graph, Start, End, Vs1, [Start-InBetween|Path]):-
  % Make sure there are no loops or repetitions.
  edge(Graph, Start-InBetween),
  \+ member(InBetween, Vs1),
  ord_add_element(Vs1, InBetween, Vs2),
  walk_forward(Graph, InBetween, End, Vs2, Path).
