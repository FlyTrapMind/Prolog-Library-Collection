:- module(
  graph_closure,
  [
    graph_closure/3 % +Elements:ordset
                    % :NeighborPredicate
                    % -ElementsUnderClosure:ordset
  ]
).

/** <module> Graph closure

@author Wouter Beek
@version 2013/12
*/

:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate(graph_closure(+,2,-)).
:- meta_predicate(graph_closure(+,2,+,-)).



%! graph_closure(+Set:ordset, :NeighborPredicate, -ClosedSet:ordset) is det.
% @arg Set
% @arg NeighborPredicate Must return the neighbors in ordered sets.
% @arg ClosedSet

graph_closure([], _, []):- !.
graph_closure([H|T], NeighborPred, Sol):-
  graph_closure([H|T], NeighborPred, [H], Sol).

graph_closure([], _, Sol, Sol):- !.
graph_closure([H1|T1], NeighborPred, Vs1, Sol):-
  call(NeighborPred, H1, Neighbors),
  exclude(in_ordset(Vs1), Neighbors, UnvisitedNeighbors),
  append(T1, UnvisitedNeighbors, T2),
  ord_union(Vs1, Neighbors, Vs2),
  graph_closure(T2, NeighborPred, Vs2, Sol).

in_ordset(Set, Element):-
  memberchk(Element, Set).
