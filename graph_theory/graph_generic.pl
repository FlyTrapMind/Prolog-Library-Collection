:- module(
  graph_generic,
  [
    bipartite/4, % +Graph
                 % :E_P
                 % -Vs1:ordset
                 % -Vs2:ordset
    connected/3, % :V_P
                 % :E_P
                 % +Graph
    cubic/2, % :V_P
             % +Graph:ugraph
    graphic_graph/1, % +S:list(integer)
    has_cycle/3, % :V_P
                 % :E_P
                 % +Graph
    regular/2, % :V_P
               % +Graph
    regular/3, % :V_P
               % +Graph
               % ?K:integer
    simple/3 % :V_P
             % :E_P
             % +Graph
  ]
).

/** <module> Graph theory: generic

Predicate that implement generic graph operations.

Generic operations are possible via meta-arguments for retrieving
the edges and vertices.

@author Wouter Beek
@tbd Compare shortest path and travel predicates two versions.
@version 2013/01-2013/04, 2013/07, 2014/03, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(list_ext)).
:- use_module(generics(sort_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_traversal)).
:- use_module(pl(pl_control)).

:- use_module(plRdf(rdf_graph_theory)).

:- meta_predicate(bipartite(+,2,-,-)).
:- meta_predicate(cubic(2,+)).
:- meta_predicate(connected(2,2,+)).
:- meta_predicate(has_cycle(2,2,+)).
:- meta_predicate(regular(2,+)).
:- meta_predicate(regular(2,+,-)).
:- meta_predicate(simple(2,2,+)).



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
  maplist(\I^'=='(I,0), Zeros), !.
graphic_graph([H | T]):-
  length(T, LT),
  H =< LT,
  length_cut(T, H, T1, T2),
  maplist(succ, NewT1, T1),
  append(NewT1, T2, NewT_),
  sort(NewT_, NewT, [duplicates(true),inverted(true)]),
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

