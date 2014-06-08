:- module(
  trees,
  [
    all_subpaths_to_tree/2, % +AllSubPaths:list(list)
                            % -Tree:tree
    print_tree/2, % ?Out
                  % +Tree:compound
    print_tree/3, % :Options:list(nvpair)
                  % ?Out
                  % +Tree:compound
    some_subpaths_to_tree/2, % +SomeSubPaths:list(list)
                             % -Tree:tree
    tree_depth/2, % +Tree:compound
                  % -Depth:nonneg
    tree_to_leaf_coord/2, % +Tree:compound
                          % -Coord:list(nonneg)
    tree_to_ugraph/2, % +Tree:compound
                      % -UGraph:ugraph
    tree_to_vertices_and_edges/3 % +Tree:compound,
                                 % -Vertices:ordset,
                                 % -Edges:ordset
  ]
).

/** <module> Trees

@author Wouter Beek
@version 2013/04-2013/05, 2013/07-2013/08, 2014/03, 2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(codes_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(option_ext)).
:- use_module(ugraph(ugraph_ext)).

:- meta_predicate(print_tree(:,?,+)).



%! all_subpaths_to_tree(+Subpaths:list(list), -Tree:compound) is det.
% Creates the tree based on the given subpaths.
%
% ### Examples
%
% ~~~{.pl}
% ?- all_subpaths_to_tree([[a,b],[a,c]], T).
% T = [a-[b-[], c-[]]].
%
% ?- all_subpaths_to_tree([[a,b],[a,c],[a,d]], T).
% T = [a-[b-[], c-[], d-[]]].
%
% ?- all_subpaths_to_tree([[a,b],[a,c,e],[a,d]], T).
% T = [a-[b-[], c-[e-[]], d-[]]].
%
% ?- all_subpaths_to_tree([[a,b],[a,c,e],[a,c,f],[a,d]], T).
% T = [a-[b-[], c-[e-[], f-[]], d-[]]].
% ~~~

all_subpaths_to_tree(Subpaths, Tree):-
  all_subpaths_to_tree0(Subpaths, [Tree]), !.
all_subpaths_to_tree(_, []).

% Only the empty tree has zero subpaths.
all_subpaths_to_tree0([], []):- !.
% The subpaths may have different lengths.
% If one subpath is fully processed, we still need to process the rest.
all_subpaths_to_tree0([[]|Ls], Tree):- !,
  all_subpaths_to_tree(Ls, Tree).
% Create the (sub)trees for the given subpaths recursively.
all_subpaths_to_tree0(Ls1, Trees):-
  % Groups paths by parent node.
  findall(
    H-T,
    member([H|T], Ls1),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),

  % For each parent node, construct its subtrees.
  findall(
    H-Subtrees,
    (
      member(H-Ls2, Pairs2),
      all_subpaths_to_tree0(Ls2, Subtrees)
    ),
    Trees
  ).


is_meta(transformation).

print_node(O1, Node1) -->
  % Hierarchic structure prefixing the node representation.
  {option(indent(I), O1, 0)},
  `|`,
  '#'(I, hyphen),
  `- `,

  % The node, written after arbitrary transformation.
  {option(transformation(Predicate), O1, identity)},
  {call(Predicate, Node1, Node2)},
  atom(Node2),

  % End with a newline.
  newline.

print_trees(_O1, []) --> [].
print_trees(O1, [H|T]) -->
  print_tree(O1, H),
  print_trees(O1, T).

print_tree(Out, Tree):-
  print_tree([], Out, Tree).

%! print_tree(+Options:list(nvpair), +Out, +Tree:compound) is det.
% Prints the given tree compound term to the given output device.
%
% The following options are supported:
%   * =|indent(+Indent:nonneg)|=
%     The indentation depth of the root node (default 0).
%   * =|transformation(:Predicate)|=
%     The transformation that is performed upon the nodes of the tree,
%     if any.

print_tree(O1, Out, Tree):-
  meta_options(is_meta, O1, O2),
  add_default_option(O2, indent, 0, O3),
  once(phrase(print_tree(O3, Tree), Codes)),
  put_codes(Out, Codes).

print_tree(O1, Tree) -->
  % 'Root' node.
  {Tree =.. [Node|Trees]},
  print_node(O1, Node),

  % Alter indentation level.
  {update_option(O1, indent, succ, O2)},

  % Sub trees / child nodes.
  print_trees(O2, Trees).

some_subpaths_to_tree(SomeSubPaths, Tree):-
  aggregate_all(
    set(SubPath),
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  all_subpaths_to_tree(AllSubPaths, Tree).


%! tree_depth(+Tree:compound, -Depth:nonneg) is det.

tree_depth(Tree, Depth2):-
  Tree =.. [_|Children],
  Children \== [], !,
  maplist(tree_depth, Children, Depths),
  max_list(Depths, Depth1),
  Depth2 is Depth1 + 1.
tree_depth(_, 0).


%! tree_to_leaf_coord(+Tree:compound, -Coord:list(nonneg)) is nondet.

tree_to_leaf_coord(_-[], []):- !.
tree_to_leaf_coord(_-Children, [Index|Coord]):-
  nth0(Index, Children, Child),
  tree_to_leaf_coord(Child, Coord).


tree_to_ugraph(T, G):-
  tree_to_vertices_and_edges(T, Vs, Es),
  ugraph_vertices_edges_to_ugraph(Vs, Es, G).


%! tree_to_vertices_and_edges(
%!   +Tree:compound,
%!   -Vertices:ordset,
%!   -Edges:ordset
%! ) is det.

tree_to_vertices_and_edges(T, Vs, Es):-
  flag(vertex, _, 0),
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

