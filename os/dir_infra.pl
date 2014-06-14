:- module(
  dir_infra,
  [
    create_personal_subdirectory/2, % +NestedDirectories:compound
                                    % -AbsoluteDirectory:atom
    output_directory/1, % -OutputDirectory:atom
    trashcan/1 % -Directory:atom
  ]
).

/** <module> Directory infrastructure

Predicates for creating an maintaining a specific infrastructure of
directories that are used for certain purposes, e.g. output, data, trash.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09, 2013/11-2014/02, 2014/04
*/

:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).



%! create_personal_subdirectory(
%!   +NestedDirectories:compound,
%!   -AbsoluteDirectory:atom
%! ) is det.
% Asserts a project-specific directory that is a direct subdirectory of the
% current user's home.
%
% This can be used to write files to. Something that is not guaranteed for
% the location where the code is run from.
%
% This requires that the project name has been set using project/2.

create_personal_subdirectory(Nested, Abs):-
  % Make sure the personal directory is there.
  personal_directory_init,

  create_nested_directory(personal(Nested), Abs).


home_init:-
  file_search_path(home, _), !.
home_init:-
  expand_file_name('~', [HomeDir]),
  assert(user:file_search_path(home, HomeDir)).


output_directory(OutDir):-
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_file_path(DataDir, 'Output', OutDir),
  make_directory_path(OutDir).


personal_directory_init:-
  file_search_path(personal, _), !.
personal_directory_init:-
  % Make sure the home directory is there.
  home_init,
  
  % @tbd Multiple projects can be loaded at the same time.
  user:project(Project, _, _),

  hidden_file_name(Project, Hidden),
  create_nested_directory(home(Hidden), _),
  assert(user:file_search_path(personal, home(Hidden))).


trashcan(Dir):-
  trashcan_init,
  absolute_file_name(
   personal('Trash'),
    Dir,
    [access(write),file_type(directory)]
  ).


trashcan_init:-
  file_search_path(trash, _), !.
trashcan_init:-
  create_personal_subdirectory('Trash', Dir),
  assert(user:file_search_path(trash, Dir)).

