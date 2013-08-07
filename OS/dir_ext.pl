:- module(dir_ext,
  [
    create_personal_subdirectory/2, % +SubDir:list(atom)
                                    % -Absolute:atom
    create_directory/1, % +Dir:atom
    create_nested_directory/1, % +NestedDirs:compound
    create_nested_directory/2, % +NestedDirs:compound
                               % -Absolute:atom
    directory_files2/2, % +Directory:atom
                        % -Absolutes:list(atom)
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
@version 2013/06-2013/07
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(os(file_ext)).
:- use_module(os(filepath_ext)).



%! create_directory(+Dir:atom) is det.
% Creates a directory with the given name.
%
% @param Dir The atomic name of a directory.

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
% @param NestedDir A compound term of linearly nested atoms
%      representing the subsequent subdirectories. The final atom
%      is the name of the file.
% @param Absolute The absolute path of the nested directory specification.

create_nested_directory(Absolute, Absolute):-
  atomic(Absolute), is_absolute_file_name(Absolute), !,
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

%! create_personal_directory is det.
% Asserts the home directory of the current user as a Prolog search path.

create_personal_directory:-
  file_search_path(personal, _), !.
create_personal_directory:-
  % Make sure that the project name has been asserted.
  current_predicate(project_name/1),
  expand_file_name('~', [Home]),
  db_add_novel(user:file_search_path(home, Home)),
  project_name(Project),
  hidden_file_name(Project, Hidden),
  create_nested_directory(home(Hidden), _Dir),
  db_add_novel(user:file_search_path(personal, home(Hidden))).

%! create_personal_subdirectory(+SubDir:atom, -Absolute:atom) is det.
% Asserts a project-specific directory that is a direct subdirectory of the
% current user's home.
%
% This can be used to write files to. Something that is not guaranteed for
% the location where the code is run from.
%
% This requires that the project name has been set using project_name/1.

create_personal_subdirectory(SubDir, Absolute):-
  create_personal_directory,
  create_nested_directory(personal(SubDir), Absolute).

%! directory_files(
%!   +Directory:atom,
%!   +FileType:atom,
%!   -Entries:list(atom)
%! ) is det.

directory_files(Directory, FileType, Absolutes2):-
  directory_files2(Directory, Absolutes1),
  setoff(
    Absolute,
    (
      member(Absolute, Absolutes1),
      member(FileType0, [FileType, directory]),
      file_name_type(_Base, FileType0, Absolute)
    ),
    Absolutes2
  ).

%! directory_files2(+Directory:atom, -Absolutes:list(atom)) is det.
% @see Variant of directory_files/2 that returns absolute file names
%      instead of relative ones.

directory_files2(Directory, Absolutes):-
  directory_files(Directory, Files1),
  selectchk('.', Files1, Files2),
  selectchk('..', Files2, Files3),
  findall(
    Absolute,
    (
      member(File, Files3),
      directory_file_path(Directory, File, Absolute)
    ),
    Absolutes
  ).

%! safe_delete_directory_contents(+Dir:atom, +FileType:atom) is det.
%! safe_delete_directory_contents(+Dir:atom, +FileType:list(atom)) is det.
% Deletes all file in the given directory that are of the given file type.
%
% @param Dir The atomic absolute name of a directory.
% @param FileType The atomic name of a file type, registered via
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

trashcan(Dir):-
  trashcan_init,
  absolute_file_name(
   personal(trash),
    Dir,
    [access(write), file_type(directory)]
  ).

trashcan_init:-
  file_search_path(trash, _Dir), !.
trashcan_init:-
  create_personal_subdirectory(trash, Absolute),
  db_add_novel(user:file_search_path(trash, Absolute)).

