:- module(
  trees,
  [
    all_subpaths_to_tree/2, % +AllSubPaths:list(list)
                            % -Tree:tree
    some_subpaths_to_tree/2, % +SomeSubPaths:list(list)
                             % -Tree:tree
    tree_to_ugraph/2, % +Tree:compound
                      % -UGraph:ugraph
    tree_to_vertices_and_edges/3 % +Tree:compound,
                                 % -Vertices:ordset,
                                 % -Edges:ordset
  ]
).

/** <module> TREES

@author Wouter Beek
@tbd Test this module.
@version 2013/04-2013/05, 2013/07
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(ugraph(ugraph_ext)).



all_subpaths_to_tree(Subpaths, [Trees]):-
  all_subpaths_to_tree(Subpaths, [], Trees).

all_subpaths_to_tree(Lists, List, List-Trees):-
  setoff(
    Tree,
    (
      member(LongerList, Lists),
      append(List, [_], LongerList),
      all_subpaths_to_tree(Lists, LongerList, Tree)
    ),
    Trees
  ).

some_subpaths_to_tree(SomeSubPaths, Tree):-
  setoff(
    SubPath,
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  all_subpaths_to_tree(AllSubPaths, Tree).

tree_to_ugraph(T, G):-
  tree_to_vertices_and_edges(T, Vs, Es),
  ugraph_vertices_edges_to_ugraph(Vs, Es, G).

%! tree_to_vertices_and_edges(
%!   +Tree:compound,
%!   -Vertices:ordset,
%!   -Edges:ordset
%! ) is det.

tree_to_vertices_and_edges(T, Vs, Es):-
  flag(vertex, _Id, 0),
  tree_to_vertices_and_edges(T, Vs, Es, _TopV_Term).

tree_to_vertices_and_edges(Leaf, [V_Term], [], V_Term):-
  atomic(Leaf), !,
  create_vertex_term(Leaf, V_Term).
tree_to_vertices_and_edges(T, V_Terms, E_Terms, V_Term):-
  T =.. [V|Ts],
  create_vertex_term(V, V_Term),
  maplist(tree_to_vertices_and_edges, Ts, V_TermsTs, E_TermsTs, TopV_Terms),
  findall(
    V_Term-W_Term,
    member(W_Term, TopV_Terms),
    TopE_Terms
  ),
  ord_union([[V_Term]|V_TermsTs], V_Terms),
  ord_union([TopE_Terms|E_TermsTs], E_Terms).

create_vertex_term(V, vertex(Id, V)):-
  flag(vertex, Id, Id + 1).

