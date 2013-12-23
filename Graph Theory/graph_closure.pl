:- module(
  graph_closure,
  [
    graph_closure/3 % +Elements:ordset
                    % :NeighborPred
                    % -ElementsUnderClosure:ordset
  ]
).

/** <module> Graph closure

@author Wouter Beek
@version 2013/12
*/

:- use_module(generics(meta_ext)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate(graph_closure(+,2,-)).
:- meta_predicate('_graph_closure'(+,2,+,-)).



graph_closure(Set, NeighborPred, Sol):-
  '_graph_closure'(Set, NeighborPred, [], Sol).

'_graph_closure'([], _NeighborPred, Sol, Sol):-
  !.
'_graph_closure'([H|T], NeighborPred, Vs, Sol):-
  memberchk(H, Vs), !,
  '_graph_closure'(T, NeighborPred, Vs, Sol).
'_graph_closure'([H1|T1], NeighborPred, Vs1, Sol):-
  call(NeighborPred, H1, T2),
  append(T1, T2, T),
  ord_union(Vs1, T2, Vs2),
  '_graph_closure'(T, NeighborPred, Vs2, Sol).

