:- module(
  dgraph_ext,
  [
    directed_cycle/2, % +V:vertex
                      % +G:dgraph
    directed_cycle/3, % +V:vertex
                      % +G:dgraph
                      % -Cycle:list(arc_vertice)
    directed_cycle/5, % +V:vertex
                      % +G:dgraph
                      % -Vs:list(vertex)
                      % -As:list(arc)
                      % -Cycle:list(arc_vertice)
    directed_trail/3, % +V:vertex
                      % +G:dgraph
                      % ?W:vertex
    directed_trail/4, % +V:vertex
                      % +G:dgraph
                      % ?W:vertex
                      % -DirTrail:list(arc_vertice)
    directed_trail/6, % +V:vertex
                      % +G:dgraph
                      % ?W:vertex
                      % -Vs:list(vertex)
                      % -As:list(arc)
                      % -DirTrail:list(arc_vertice)
    directed_path/3, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
    directed_path/4, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
                     % -DirPath:list(arc_vertice)
    directed_path/6, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
                     % -Vs:list(vertex)
                     % -As:list(arc)
                     % -DirPath:list(arc_vertice)
    directed_walk/3, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
    directed_walk/4, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
                     % ?DirectedWalk:list(arc&edge)
    directed_walk/6, % +V:vertex
                     % +G:dgraph
                     % ?W:vertex
                     % -Vs:list(vertex)
                     % -As:list(arc)
                     % -DirectedWalk:list(arc_vertice)
    reachable/3 % +V:vertex
                % +G:dgraph
                % +W:vertex
  ]
).

%! directed_cycle(+V:vertex, +G:dgraph) is semidet.
% Succeeds if there is a directed cycle at the given vertex, in the given
% graph.
%
% @see directed_cycle/3

directed_cycle(V, G):-
  directed_cycle(V, G, _DirCycle).

%! directed_cycle(
%!   +V:vertex,
%!   +G:dgraph,
%!   -DirCycle:list(edge_vertice)
%! ) is nondet.
% Returns a directed cycle through the given directed graph, starting at the
% given vertex.
%
% @see directed_cycle/5

directed_cycle(V, G, DirCycle):-
  directed_cycle(V, G, _Vs, _Es, DirCycle).

%! directed_cycle(
%!   +V:vertex,
%!   +G:dgraph,
%!   -Vs:list(vertex),
%!   -Es:list(edge),
%!   -DirCycle:list(edge_vertice)
%! ) is nondet.
% Returns directed cycles that start at the given vertex in the given graph.
% Also returns the edges and vertices that form the cycle, in the order in
% which they were visited.
%
% *Definition*: A directed cycle is a directed closed trail where all vertices
%               are unique (as in a directed path) but with V_0 = V_n.
%
% @arg V A vertex.
% @arg G A directed graph.
% @arg Vs A list of vertices.
% @arg Es A list of edges.
% @arg DirCycle A list that consists of interchanging vertices and edges.

directed_cycle(V, G, [V | Vs], [V-W | Es], [V, V-W | DirCycle]):-
  neighbor(G, V, W),
  directed_path(W, G, V, [], Vs, [V-W], Es, DirCycle).

directed_trail(V, G, W):-
  directed_trail(V, G, W, _DirTrail).

directed_trail(V, G, W, DirTrail):-
  directed_trail(V, G, W, _Vs, _As, DirTrail).

directed_trail(V, G, W, Vs, As, DirTrail):-
  directed_trail(V, G, W, Vs, [], As, DirTrail).

directed_trail(V, _G, V, _History, [V], [], [V]):- !.
directed_trail(V, G, W, [V | Vs], H_As, [V-X | As], [V, V-X | DirTrail]):-
  neighbor(G, V, X),
  \+(member(V-X, H_As)),
  directed_trail(X, G, W, Vs, [V-X | H_As], As, DirTrail).

directed_path(V, G, W):-
  directed_path(V, G, W, _DirPath).

directed_path(V, G, W, DirPath):-
  directed_path(V, G, W, _Vs, _As, DirPath).

directed_path(V, G, W, Vs, As, DirPath):-
  directed_path(V, G, W, [], Vs, [], As, DirPath).

directed_path(V, _G, V, _H_Vs, [V], _H_As, [], [V]):- !.
directed_path(V, G, W, H_Vs, [V | Vs], H_As, [V-X | As], [V, V-X | DirPath]):-
  neighbor(G, V, X),
  \+(member(X, H_Vs)),
  \+(member(V-X, H_As)),
  directed_path(X, G, W, [X | H_Vs], Vs, [V-X | H_As], As, DirPath).

%! directed_walk(+V:vertex, +G:dgraph, ?W:vertex) is semidet.
% @see directed_walk/6

directed_walk(V, G, W):-
  directed_walk(V, G, W, _DirWalk).

%! directed_walk(
%!   +V:vertex,
%!   +G:dgraph,
%!   ?W:vertex,
%!   -DirWalk:list(arc_vertice)
%! ) is nondet.
% @see directed_walk/6

directed_walk(V, G, W, DirWalk):-
  directed_walk(V, G, W, _Vs, _As, DirWalk).

%! directed_walk(
%!   +V:vertex,
%!   +G:dgraph,
%!   ?W:vertex,
%!   -Vs:list(vertex),
%!   -As:list(arc),
%!   -DirWalk:list(arc_vertice)
%! ) is nondet.
% Returns the path along which the given vertices are joined
% in the given graph.

directed_walk(V, _G, V, [], [], []):- !.
directed_walk(V, G, W, [V, X | Vs], [V-X | As], [V, V-X, X | DirWalk]):-
  neighbor(G, V, X),
  directed_walk(X, G, W, Vs, As, DirWalk).



%! reachable(V, G, W) is semidet.
% Succeeds if the latter vertex is reachable from the former.
%
% *Definition*: A vertex W is reable from vertex V if there is
%               a directed path from V to W.

reachable(V, G, W):-
  directed_path(V, G, W).
