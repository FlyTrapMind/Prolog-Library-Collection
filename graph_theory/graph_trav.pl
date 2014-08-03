:- module(
  graphs_trav,
  [
    bounded_breadthfirst_graph_search/5, % :NeighborPred
                                         % +Vertex
                                         % +Depth:integer
                                         % -Vertices:ordset
                                         % -Edges:ordset(pair)
    cyclic_graph_traversal/5, % +Predicates:list
                              % +Sets:list(ordset)
                              % +LastSets:list(ordset)
                              % +SetSizes:list(integer)
                              % -Solution:list(ordset)
    depth_path/4, % +Element
                  % +Predicate:term
                  % +Depth:integer
                  % -Path:list
    depth_path/5 % +Element
                 % +Predicate:term
                 % +Depth:integer
                 % +Inv:boolean
                 % -Path:list
  ]
).

/** <module> Graph theory: traversal

Predicates for graph traversal.

Two methods created during my last days at the IvI institute:
  * cyclic_graph_traversal/5
  * depth_path/4

@author Wouter Beek
@version 2012/07-2012/08, 2012/10, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(graph_theory(graph_generic)).

:- use_module(plRdf(rdfs_read)).

:- meta_predicate(bounded_breadthfirst_graph_search(2,+,+,-,-)).
:- meta_predicate(bounded_breadthfirst_graph_search(2,+,+,-,+,-)).
:- meta_predicate depth_path(+,0,+,-).
:- meta_predicate depth_path(+,0,+,+,-).
:- meta_predicate depth_path(?,*,2,*,*,*).

:- rdf_meta(bounded_breadthfirst_graph_search(:,+,r,-,-)).



%! bounded_breadthfirst_graph_search(
%!   :NeighborPred,
%!   +Depth:nonneg,
%!   +SeedVertex,
%!   -Vertices:ordset,
%!   -Edges:ordset(compound)
%! ) is det.
% Returns all vertices and edges that are found within the given depth
% distance from the given vertex.
%
% This is the same as bounded breadth-first search.

bounded_breadthfirst_graph_search(NeighborPred, Depth, V, Vs, Es):-
  bounded_breadthfirst_graph_search(NeighborPred, Depth, [V], Vs, [], Es).

% Depth was reached.
bounded_breadthfirst_graph_search(_, 0, Vs, Vs, Es, Es):- !.
bounded_breadthfirst_graph_search(NeighborPred, Depth1, Vs1, Vs3, Es1, Es3):-
  aggregate_all(
    set(X-Y),
    (
      member(X, Vs1),
      call(NeighborPred, X, Y),
      \+ member(X-Y, Es1)
    ),
    NewEs
  ),
  edges_to_vertices(NewEs, NewVs),
  ord_union(Es1, NewEs, Es2),
  ord_union(Vs1, NewVs, Vs2),

  Depth2 is Depth1 - 1,
  bounded_breadthfirst_graph_search(NeighborPred, Depth2, Vs2, Vs3, Es2, Es3).


%! cyclic_graph_traversal(
%!   +Predicates:list(atom),
%!   +Sets:list(ordset(object)),
%!   +LastSets:list(ordset(object)),
%!   +Sizes:list(integer),
%!   -Solution:list(ordset(object))
%! ) is det.
%
% @arg Predicates
% @arg Sets
% @arg LastSets
% @arg Sizes
% @arg Solution

cyclic_graph_traversal(
  [Module:PredicateXY | Predicates],
  [SetX, SetY | Sets],
  [LastSetX, LastSetY | LastSets],
  [SetSizeX, SetSizeY | SetSizes],
  Solution
):-
  Call =.. [PredicateXY, SetX, NewSetY__],
  call(Module:Call),
  ord_union(SetY, NewSetY__, NewSetY_),
  length(NewSetY_, NewSizeY),
  (
    (
      NewSetY_ == SetY
    ;
      NewSizeY == SetSizeY
    )
  ->
    Solution = [SetX, SetY | Sets]
  ;
    NewSizeY > SetSizeY
  ->
    Remove is SetSizeY - NewSizeY,
    length(LastSetY, LastSizeY),
    Keep is LastSizeY - Remove,
    length(KeepSubset, Keep),
    append(KeepSubset, RemoveSubset, LastSetY),
    ord_subtract(NewSetY_, RemoveSubset, NewSetY),
    Solution = [SetX, NewSetY | Sets]
  ;
    NewSetY = NewSetY_,
    append(Predicates, [Module:PredicateXY], NewPredicates),
    append([NewSetY | Sets], [SetX], NewSets),
    ord_subtract(NewSetY, SetY, NewLastSetY),
    append([NewLastSetY | LastSets], [LastSetX], NewLastSets),
    append([SetSizeY | SetSizes], [SetSizeX], NewSetSizes),
    cyclic_graph_traversal(NewPredicates, NewSets, NewLastSets, NewSetSizes, Solution)
  ).

%! depth_path(
%!   +Element:uri,
%!   +Predicate:term,
%!   +Depth:integer,
%!   -Path:list(uri)
%! ) is det.
% @see depth_path/5

depth_path(Element, Predicate, Depth, Path):-
  depth_path(Element, Predicate, Depth, fail, Path).

%! depth_path(
%!   +Element:uri,
%!   +Predicate:term,
%!   +Depth:integer,
%!   +Inv:boolean
%!   -Path:list(uri)
%! ) is det.
% Returns a single path from the given element via the given Prolog or RDF
% predicate, going maximally until the given depth and possibly inverting
% the relationship.
%
% @arg Element The element at which the path starts.
% @arg Predicate The predicate that is used to traverse the path.
%        1. If this is an atom, then use the currently loaded SWI-Prolog
%           predicate with that name.
%        2. If this is a complex term with structure =Namespace:PredicateName=,
%           then use the predicates from the RDF library.
% @arg Depth An integer. The maximum length of the path.
% @arg Inv When set to =true= the path is follows backwards,
%        otherwise the forward variant is used.
% @arg Path A list of resources.

% Remove the module (e.g. =user=) prefix from the predicate. This allows
% depth_path/6 to reliably discern RDF predicates from native Prolog ones.
depth_path(Element, _Module:Predicate, Depth, Inv, Path):-
  depth_path(Element, [], Predicate, Depth, Inv, Path).

depth_path(Element, _History, _Predicate, 0, _Inv, [Element]):-
  !.
% Special version for RDF predicates.
depth_path(Element, History, Predicate, Depth, Inv, [Element | Path]):-
  rdf_global_id(_Namespace:_PredicateName, Predicate),
  % RDF predicates should not be associated with a module.
  strip_module(Predicate, _Module, Predicate0),
  (
    Inv == true
  ->
    rdfs(NextElement, Predicate0, Element, _Graph1)
  ;
    rdfs(Element, Predicate0, NextElement, _Graph2)
  ),
  \+(member(NextElement, History)),
  NewDepth is Depth - 1,
  depth_path(NextElement, [NextElement | History], Predicate, NewDepth, Inv, Path),
  !.
% The normal version, for native Prolog predicates.
depth_path(Element, History, Predicate, Depth, Inv, [Element | Path]):-
  \+ rdf_global_id(_Namespace:_PredicateName, Predicate),
  (
    Inv == true
  ->
    call(Predicate, NextElement, Element)
  ;
    call(Predicate, Element, NextElement)
  ),
  \+(member(NextElement, History)),
  NewDepth is Depth - 1,
  depth_path(NextElement, [NextElement | History], Predicate, NewDepth, Inv, Path),
  !.
depth_path(Element, _History, _Predicate, _Depth, _Inv, [Element]).

