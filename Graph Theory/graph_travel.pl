:- module(
  graph_travel,
  [
    travel1/7, % +Options:list(nvpair)
               % +Graph
               % :E_P
               % :N_P
               % +First:vertex
               % +Last:vertex
               % -Distance:integer
    travel1/10, % +Options:list(nvpair)
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
                  % :E_P
                  % :N_P
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
                  % -Vertices:ordset(vertex)
                  % -Edges:ordset(edge)
                  % -History:list
  ]
).

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1

:- meta_predicate(travel1(+,+,2,3,+,+,-)).
:- meta_predicate(travel1(+,+,2,3,+,+,-,-,-,-)).
:- meta_predicate(travel1_(+,+,2,3,+,+,-,-,-,-,-,-)).
:- meta_predicate(travel2(+,3,+,+,-,-,-)).
:- meta_predicate(travel2(+,3,+,+,-,+,-,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-)).
:- meta_predicate(travel_min(+,+,2,3,+,+,-,-,-,-)).

:- rdf_meta(travel1(+,+,:,:,r,r,-)).
:- rdf_meta(travel1(+,+,:,:,r,r,-,-,-,-)).
:- rdf_meta(travel1_(+,+,:,:,r,r,-,-,-,-,-,-)).
:- rdf_meta(travel2(+,:,r,r,-,-,-)).
:- rdf_meta(travel2(+,:,r,r,-,+,-,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-)).
:- rdf_meta(travel_min(+,+,:,:,r,r,-,-,-,-)).



%! travel1(
%!   +Options:list(nvpair),
%!   +Graph:graph,
%!   :E_P,
%!   :N_P,
%!   +FirstV:vertex,
%!   +LastV:vertex,
%!   -Distance:uinteger
%! ) is nondet.
% @see Wrapper around travel1/10.

travel1(O, G, E_P, N_P, First, Last, Distance):-
  travel1(O, G, E_P, N_P, First, Last, Distance, _Vertexs, _Edges, _History).

%! travel1(
%!   +Options:list(nvpair),
%!   +Graph:graph,
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
% ~~~{.pl}
% Options = [closed(true)]
% ~~~
%
% A *trail* is a walk with unique edges.
% ~~~{.pl}
% Options = [unique_edge(true)]
% ~~~
%
% A *path* is a walk with unique vertices.
% ~~~{.pl}
% Options = [unique_vertex(true)]
% ~~~
%
% A *cycle* is a closed path trail.
% ~~~{.pl}
% Options = [closed(true),unique_edge(true),unique_vertex(true)]
% ~~~
%
% An *Euler tour* is a tour in which all edges are traversed exactly one.
% ~~~{.pl}
% Options = [closed(true),every_edge(true),unique_edge(true)]
% ~~~
%
% The following options are defined:
%   1. =|closed(boolean)|=
%   2. =|distance(oneof([edge,vertex]))|= For statiscs we return either
%      the number of edges or the number of vertices that were traversed.
%      Default: =edge=.
%   3. =|euler(boolean)|=
%   4. =|every_edge(boolean)|=
%   5. =|every_vertex(boolean)|=
%   6. =|graph(Graph)|=
%   7. =|unique_edge(boolean)=
%   8. =|unique_vertex(boolean)=
%
% @param Options A list of name-value pairs.
% @param Graph
% @param E_P Maps a graph to its edges.
% @param N_P Maps a vertex to its neighbor vertices.
% @param First The first vertex in the path.
% @param Last The last vertex in the path.
% @param Distance An integer representing a distance between the first and
%      the last vertex, counted as the number of traversed edges.
% @param Vertices A list of vertices.
% @param Edges A list of edges.
% @param History

travel1(O, G, E_P, N_P, First, Last, Distance, Vertices, Edges, History):-
  travel1_(
    O, G, E_P, N_P, First, Last,
    Distance, [First], Vertices, [], Edges, History
  ).

travel1_(
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
travel1_(
  O, G, E_P, N_P, FirstV, Last,
  Distance, Vs, SolV, Es, SolE, [FirstV, FirstV-NextV | History]
):-
  % Neighbor
  call(N_P, G, FirstV, NextV),

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
  travel1_(
    O, G, E_P, N_P, NextV, Last, Distance, NewVs, SolV, NewEs, SolE, History
  ).

travel2(G, N_P, FromV, ToV, Length, AllVs, Path):-
  travel2(G, N_P, FromV, ToV, Length, [FromV], AllVs, Path).

travel2(_G, _N_P, ToV, ToV, Length, AllVs, AllVs, [ToV]):-
  length(AllVs, Length), !.
travel2(G, N_P, FromV, ToV, Length, Vs, AllVs, [FromV | Path]):-
  call(N_P, FromV, G, ToVs),
  member(ViaV, ToVs),
  % Make sure we haven't be here yet.
  \+ member(ViaV, Vs),
  ord_add_element(Vs, ToV, NewVs),
  travel2(G, N_P, ViaV, ToV, Length, NewVs, AllVs, Path).

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
% @param Options A list of name-value pairs.
%      See travel/7 for the list of supported options.
% @param Graph
% @param E_P
% @param N_P
% @param First A vertex, the first in the travel.
% @param Last A respource, the last in the travel.
% @param MinimumDistance An integer representing the minimum distance
%      between the first and last vertex. The kind of distances is set
%      in =Options=.
% @param Vertices An ordered set of vertices.
% @param Edges An ordered set of Edges.
% @param History A list representing a minimum travel between the first and
%      last resources.

travel_min(O, G, E_P, N_P, First, Last, MinimumDistance, Vs, Es, History):-
  setoff(
    Distance-History,
    travel1(O, G, E_P, N_P, First, Last, Distance, Vs, Es, History),
    Pairs
  ),
  first(Pairs, MinimumDistance-History).

