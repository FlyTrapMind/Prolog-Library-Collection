:- module(
  archive_tree,
  [
    archive_tree/2, % +Source
                    % -Tree:compound
    archive_tree_coords/2 % +Source
                          % -Coords:list(pair(atom,list(nonneg)))
  ]
).

/** <module> Archive tree

Predicates that construct a tree datastructure reflecting
an archive's tree structure.

@author Wouter Beek
@version 2014/04, 2014/06-2014/09
*/

:- use_module(library(archive)).

:- use_module(generics(list_ext)).
:- use_module(os(archive_ext)).



%! archive_tree(+Source, -Tree:compound) is det.
%
% ### Example
%
% ~~~{.pl}
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_tree(File, Tree).
% File = '.../data/abcde.tar.gz',
% Tree = '.../data/abcde.tar.gz'-[
%          'ab.tar.gz'-['a.txt'-[], 'b.txt'-[]],
%          'cd.tar.gz'-['d.txt'-[], 'c.txt'-[]],
%          'e.txt'-[]
%        ].
% ~~~

archive_tree(File, Tree):-
  archive_subpaths(File, Subpaths1),
  remove_sublists(Subpaths1, Subpaths2),
  all_subpaths_to_tree(Subpaths2, Tree).

archive_subpaths(Source, Subpaths):-
  archive_goal(Source, archive_assert_subpaths, [Source]),
  findall(
    Subpath,
    retract(entry_path(Subpath)),
    Subpaths
  ).

archive_assert_subpaths(Archive, T):-
  repeat,
  (
    archive_next_header(Archive, EntryName),
    \+ archive_ext:is_leaf_entry(Archive, EntryName)
  ->
    L = [EntryName|T],
    reverse(L, Subpath),
    assert(entry_path(Subpath)),
    % Recurse archive entries.
    setup_call_cleanup(
      archive_open_entry(Archive, Stream2),
      archive_goal0(Stream2, archive_assert_subpaths, L),
      close(Stream2)
    ),
    fail
  ; !,
    true
  ).


%! archive_tree_coords(+Source, -Coordinates:list(coords)) is det.
%
% ### Example
%
% ~~~{.pl}
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_tree_coords(File, Coords).
% File = '.../data/abcde.tar.gz',
% Coords = [0, 0] ;
% File = '.../data/abcde.tar.gz',
% Coords = [0, 1] ;
% File = '.../data/abcde.tar.gz',
% Coords = [1, 0] ;
% File = '.../data/abcde.tar.gz',
% Coords = [1, 1] ;
% File = '.../data/abcde.tar.gz',
% Coords = [2].
% ~~~

archive_tree_coords(Source, Coords):-
  archive_tree(Source, Tree),
  tree_to_leaf_coord(Tree, Coords).

