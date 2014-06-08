:- module(
  archive_ext,
  [
    archive_extract/1, % +File:atom
    archive_extract/2, % +Source
                       % +Directory:atom
    archive_extract_directory/2, % +Directory:atom
                                 % +Options:list(nvpair)
    archive_goal/3, % +Source
                    % :Goal
                    % +Arguments:list
    archive_info/1, % +Source
    archive_tree/2, % +Source
                    % -Tree:compound
    archive_tree_coords/2 % +Source
                          % -Coords:list(pair(atom,list(nonneg)))
  ]
).

/** <module> Archive extraction

Based on library `archive`.

@author Wouter Beek
@version 2014/04, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(error)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).

:- use_module(generics(list_ext)).
:- use_module(generics(trees)).
:- use_module(generics(typecheck)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).

:- thread_local(archive_subpath/1).

:- meta_predicate(archive_goal(+,1)).
:- meta_predicate(archive_goal(+,2,+)).
:- meta_predicate(archive_goal0(+,1)).
:- meta_predicate(archive_goal0(+,2,+)).



%! archive_extract(+File:atom) is semidet.
% Extracts the given file into its own directory.
%
% @throws type_error When `Source` is neither an absolute file name nor a URL.
% @throws instantiation_error When File is a variable.

archive_extract(File):-
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir),
  archive_extract(File, Dir).
archive_extract(File):-
  type_error(file, File).

%! archive_extract(+Source, +Directory:atom) is semidet.
% Extracts the given file into the given directory.
%
% @throws type_error When `Source` is neither an absolute file name nor a URL.
% @throws instantiation_error When File is a variable.

archive_extract(Source, Dir):-
  archive_goal(Source, archive_extract0, Dir).

archive_extract0(Archive, Dir):-
  repeat,
  (
    archive_next_header(Archive, RelativeFile)
  ->
    setup_call_cleanup(
      archive_open_entry(Archive, Stream),
      (
        relative_file_path(File, Dir, RelativeFile),
        create_file_directory(File),
        file_from_stream(File, Stream),
        print_message(informational, archive_extracted(File))
      ),
      close(Stream)
    ),
    fail
  ; !,
    true
  ).


%! archive_extract_directory(+Directory:atom, +Options:list(nvpair)) is det.
% Extracts all archives in the given directory.
% Extract files recursively, e.g. first `gunzip`, then `tar`.
%
% Options are passed to directory_files/3. Important are:
%   * =|file_types(+FileTypes:list(atom))|=
%     Only extracts files of the given types.
%   * =|recursive(+Recursive:boolean)|=
%     Includes archives that reside in subdirectories.

archive_extract_directory(Dir, Options):-
  directory_files(Options, Dir, Files),
  maplist(archive_extract, Files).


%! archive_goal(+Source:atom, :Goal) is det.
%! archive_goal(+Source:atom, :Goal, +Argument) is det.
% `Source` is either an absolute file name or a URL.
%
% @throws type_error When `Source` is neither an absolute file name nor a URL.
% @throws instantiation_error When File is a variable.

archive_goal(File, Goal):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, read, Stream),
    archive_goal0(Stream, Goal),
    close(Stream)
  ).
archive_goal(Url, Goal):-
  is_url(Url), !,
  setup_call_cleanup(
    http_open(Url, Stream, []),
    archive_goal0(Stream, Goal),
    close(Stream)
  ).
archive_goal(Source, _):-
  type_error(file_or_url, Source).

archive_goal(File, Goal, Arg1):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, read, Stream),
    archive_goal0(Stream, Goal, Arg1),
    close(Stream)
  ).
archive_goal(Url, Goal, Arg1):-
  is_url(Url), !,
  setup_call_cleanup(
    http_open(Url, Stream, []),
    archive_goal0(Stream, Goal, Arg1),
    close(Stream)
  ).
archive_goal(Source, _, _):-
  type_error(file_or_url, Source).

archive_goal0(Source, Goal):-
  setup_call_cleanup(
    archive_open(
      Source,
      Archive,
      [close_parent(false),filter(all),format(all),format(raw)]
    ),
    call(Goal, Archive),
    archive_close(Archive)
  ).

archive_goal0(Source, Goal, Arg1):-
  setup_call_cleanup(
    archive_open(
      Source,
      Archive,
      [close_parent(false),filter(all),format(all),format(raw)]
    ),
    call(Goal, Archive, Arg1),
    archive_close(Archive)
  ).


%! archive_info(+Source) is det.
% Writes archive information for the given file or URL to current input.
%
% ### Example
%
% ~~~{.pl}
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_info(File).
% ab.tar.gz
%   filetype(file)
%   mtime(1402126051.0)
%   size(128)
%   format(posix ustar format)
%   a.txt
%     filetype(file)
%     mtime(1402126033.0)
%     size(2)
%     format(posix ustar format)
%   b.txt
%     filetype(file)
%     mtime(1402126038.0)
%     size(2)
%     format(posix ustar format)
% cd.tar.gz
%   filetype(file)
%   mtime(1402126098.0)
%   size(128)
%   format(posix ustar format)
%   d.txt
%     filetype(file)
%     mtime(1402126074.0)
%     size(2)
%     format(posix ustar format)
%   c.txt
%     filetype(file)
%     mtime(1402126067.0)
%     size(2)
%     format(posix ustar format)
% e.txt
%   filetype(file)
%   mtime(1402126131.0)
%   size(2)
%   format(posix ustar format)
% File = '.../data/abcde.tar.gz'.
% ~~~

archive_info(Source):-
  archive_goal(Source, archive_info0, 0).

archive_info0(Archive, Indent1):-
  repeat,
  (
    archive_next_header(Archive, EntryName),
    \+ is_leaf_entry(Archive, EntryName)
  ->
    print_message(information, archive_entry(Indent1,Archive,EntryName)),
    succ(Indent1, Indent2),
    % Recurse archive entries.
    setup_call_cleanup(
      archive_open_entry(Archive, Stream),
      archive_goal0(Stream, archive_info0, Indent2),
      close(Stream)
    ),
    fail
  ; !,
    true
  ).


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
    retract(archive_subpath(Subpath)),
    Subpaths
  ).

archive_assert_subpaths(Archive, T):-
  repeat,
  (
    archive_next_header(Archive, EntryName),
    \+ is_leaf_entry(Archive, EntryName)
  ->
    L = [EntryName|T],
    reverse(L, Subpath),
    assert(archive_subpath(Subpath)),
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



% HELPERS

is_leaf_entry(Archive, EntryName):-
  archive_header_property(Archive, format(EntryFormat)),
  EntryName == data,
  EntryFormat == raw.



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(archive_entry(Indent1,Archive,EntryName)) -->
  archive_header(Indent1, EntryName),
  {
    findall(
      Property,
      archive_header_property(Archive, Property),
      Properties
    ),
    succ(Indent1, Indent2)
  },
  archive_properties(Indent2, Properties).

prolog:message(archive_extracted(File)) -->
  ['Extracted ~w'-[File]].

archive_header(Indent, EntryName) -->
  indent(Indent),
  [EntryName,nl].

archive_properties(_, []) --> !, [].
archive_properties(Indent, [H|T]) -->
  indent(Indent),
  ['~w'-[H],nl],
  archive_properties(Indent, T).

indent(0) --> !, [].
indent(I1) -->
  ['  '],
  {succ(I2, I1)},
  indent(I2).

