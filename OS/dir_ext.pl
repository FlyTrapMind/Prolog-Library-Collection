:- module(dir_ext,
  [
    create_personal_subdirectory/1, % +SubDir:list(atom)
    create_directory/1, % +Dir:atom
    create_nested_directory/1, % +NestedDirs:compound
    create_nested_directory/2, % +NestedDirs:compound
                               % -Absolute:atom
    directory_files/3, % +Directory:atom
                       % +FileType:atom
                       % -Entries:list(atom)
    safe_delete_directory_contents/2, % +Directory:atom
                                      % +FileType:atom
    safe_delete_directory_contents/2, % +Directory:atom
                                      % +FileTypes:list(atom)
    trashcan/1 % -Dir:atom
  ]
).

/** <module> DIR_EXT

Extensions for handling directories.

@author Wouter Beek
@tbd Add safe_delete_directory/1.
@version 2013/06
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(os(file_ext)).
:- use_module(os(filepath_ext)).



%! create_personal_directory is det.
% Asserts the home directory of the current user as a Prolog search path.

create_personal_directory:-
  file_search_path(home, _), !.
create_personal_directory:-
  % Make sure that the project name has been asserted.
  current_predicate(project_name/1),
  expand_file_name('~', [Home]),
  db_add_novel(user:file_search_path(home, Home)),
  project_name(Project),
  hidden_file_name(Project, Hidden),
  create_nested_directory(home(Hidden), _Dir),
  db_add_novel(user:file_search_path(personal, home(Hidden))).

%! create_personal_subdirectory(+SubDir:atom) is det.
% Asserts a project-specific directory that is a direct subdirectory of the
% current user's home.
%
% This can be used to write files to. Something that is not guaranteed for
% the location where the code is run from.
%
% This requires that the project name has been set using project_name/1.

create_personal_subdirectory(SubDir):-
  create_personal_directory,
  create_nested_directory(project(SubDir), _Dir).

%! create_directory(+Dir:atom) is det.
% Creates a directory with the given name.
%
% @arg Dir The atomic name of a directory.

% The directory already exists, so do nothing.
create_directory(Dir):-
  exists_directory(Dir), !.
% The directory does not already exist, so create it.
create_directory(Dir):-
  is_absolute_file_name(Dir), !,
  split_atom_exclusive(['/'], Dir, [Root | SubDirs]),
  % Recursively assert all subpaths.
  create_directory(Root, SubDirs).

create_directory(_CurrentDir, []):- !.
create_directory(CurrentDir, [NextSubDir | SubDirs]):-
  directory_file_path(CurrentDir, NextSubDir, Dir),
  (
    exists_directory(Dir)
  ;
    make_directory(Dir)
  ),
  !,
  create_directory(Dir, SubDirs).

create_nested_directory(NestedDir):-
  create_nested_directory(NestedDir, _Absolute).

%! create_nested_directory(+NestedDir:compound, -Absolute:atom) is det.
% Returns a nested file path.
%
% @arg NestedDir A compound term of linearly nested atoms
%      representing the subsequent subdirectories. The final atom
%      is the name of the file.
% @arg Absolute The absolute path of the nested directory specification.

create_nested_directory(Absolute, Absolute):-
  is_absolute_file_name(Absolute), !,
  create_directory(Absolute).
create_nested_directory(NestedDir, Absolute):-
  atom(NestedDir), !,
  Spec =.. [NestedDir, '.'],
  absolute_file_name(Spec, Absolute),
  create_directory(Absolute).
create_nested_directory(NestedDir, Absolute):-
  % First we construct the atomic name of the outer directory.
  NestedDir =.. [NestedOuter, NestedInner],
  create_nested_directory(NestedOuter, AbsoluteOuter),
  % Then we add the inner directories recursively.
  create_nested_directory(NestedInner, AbsoluteOuter, Absolute).

%! create_nested_directory(+NestedDir:term, +OldDir:atom, -NewDir:atom) is det.
% Adds the nested directories term to the given atomic directory,
% returning another atomic directory.

create_nested_directory(SubDir, OldDir, NewDir):-
  atom(SubDir), !,
  % Note that adding the option =|file_type(directory)|= makes this clause
  % throw an exception, because this option assumed that the directory
  % exists.
  atomic_list_concat([OldDir, '/', SubDir], NewDir),
  create_directory(NewDir).
create_nested_directory(NestedDir, OldDir, NewDir):-
  NestedDir =.. [OuterDir, InnerNestedDir],
  % Note that adding the option =|file_type(directory)|= makes this clause
  % throw an exception, because this option assumes that the directory
  % exists.
  atomic_list_concat([OldDir, '/', OuterDir], TempDir),
  create_directory(TempDir),
  create_nested_directory(InnerNestedDir, TempDir, NewDir).

%! directory_files(
%!   +Directory:atom,
%!   +FileType:atom,
%!   -Entries:list(atom)
%! ) is det.

directory_files(Directory, FileType, Entries):-
  directory_files2(Directory, Files),
  setoff(
    Entry,
    (
      member(File, Files),
      member(FileType0, [FileType, directory]),
      file_name_type(_Base, FileType0, File),
      directory_file_path(Directory, File, Entry)
    ),
    Entries
  ).

directory_files2(Directory, Files):-
  directory_files(Directory, Files0),
  once(select('.', Files0, Files1)),
  once(select('..', Files1, Files)).

%! safe_delete_directory_contents(+Dir:atom, +FileType:atom) is det.
%! safe_delete_directory_contents(+Dir:atom, +FileType:list(atom)) is det.
% Deletes all file in the given directory that are of the given file type.
%
% @arg Dir The atomic absolute name of a directory.
% @arg FileType The atomic name of a file type, registered via
%      prolog_file_type/2.
% @see safe_delete_directory_contents/1
% @throws existence_error In case a file type is not registered.

% Remove all files that are of the given file type from the given directory.
safe_delete_directory_contents(Dir, FileType):-
  % Make sure that the given file type is registered.
  once(prolog_file_type(_Ext, FileType)), !,

  % Recurse over the given file type, since it may be associated with
  % multiple extensions.
  findall(
    File,
    (
      prolog_file_type(Ext, FileType),
      format(atom(RE), '.*\\.~w', [Ext]),
      path_walk_tree(Dir, RE, Files0),
      member(File, Files0)
    ),
    Files
  ),

  % Delete all files.
  % This may throw permission exceptions.
  maplist(safe_delete_file, Files).
% Allow multiple file types to be given.
safe_delete_directory_contents(Directory, FileTypes):-
  is_list(FileTypes), !,
  maplist(safe_delete_directory_contents(Directory), FileTypes).
% Throw an exception if the given file type is not registered.
safe_delete_directory_contents(_Dir, FileType):-
  atom(FileType), !,
  throw(
    error(
      existence_error(unknown_file_type, FileType),
      context(
        'safe_delete_directory_contents/2',
        'The given file type is not registered.'
      )
    )
  ).

trashcan_init:-
  file_search_path(trash, _Dir), !.
trashcan_init:-
  create_personal_subdirectory(trash).

trashcan(Dir):-
  trashcan_init,
  absolute_file_name(
   personal(trash),
    Dir,
    [access(write), file_type(directory)]
  ).

