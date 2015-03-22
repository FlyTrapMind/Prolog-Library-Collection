:- module(
  dir_infra,
  [
    create_personal_subdirectory/2, % +Subdirs:list(atom)
                                    % -AbsoluteDir:atom
    home_directory/1, % -HomeDir:atom
    output_directory/1, % -OutputDir:atom
    project_abbreviation/1, % ?Name:atom
    project_description/1, % ?Name:atom
    project_name/1, % ?Name:atom
    trashcan/1 % -TrashDir:atom
  ]
).

/** <module> Directory infrastructure

Predicates for creating an maintaining a specific infrastructure of
directories that are used for certain purposes, e.g. output, data, trash.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09, 2013/11-2014/02, 2014/04, 2014/10-2014/11,
         2015/03
*/

:- use_module(plc(io/dir_ext)).
:- use_module(plc(io/file_ext)).





%! create_personal_subdirectory(
%!   +Subdirs:list(atom),
%!   -AbsoluteDir:atom
%! ) is det.
% Asserts a project-specific directory that is a direct subdirectory of the
% current user's home.
%
% This can be used to write files to. Something that is not guaranteed for
% the location where the code is run from.
%
% This requires that the project name has been set using project/2.

create_personal_subdirectory(Subdirs, AbsDir):-
  % Make sure the personal directory is there.
  personal_directory_init,
  create_directory(personal, Subdirs, AbsDir).



%! home_directory(-HomeDir:atom) is det.

home_directory(HomeDir):-
  expand_file_name('~/', HomeDir).



%! output_directory(-OutputDir:atom) is det.

output_directory(OutputDir):-
  create_directory(data, ['Output'], OutputDir).



%! project_abbreviation(?Abbreviation:atom) .

project_abbreviation(Abbr):-
  (   user:project(_, _, Abbr)
  ->  true
  ;   user:project(Abbr, _)
  ).

%! project_description(?Description:atom) .

project_description(Desc):-
  (   user:project(_, Desc, _)
  ->  true
  ;   user:project(_, Desc)
  ).

%! project_name(?Name:atom) .

project(Name):-
  (   user:project(Name, _, _)
  ->  true
  ;   user:project(Name, _)
  ).



%! trashcan(-TrashDir:atom) is det.

trashcan(TrashDir):-
  trashcan_init,
  create_directory(personal, ['Trash'], TrashDir).



% HELPERS

home_init:-
  file_search_path(home, _), !.
home_init:-
  expand_file_name('~', [HomeDir]),
  assert(user:file_search_path(home, HomeDir)).



personal_directory_init:-
  file_search_path(personal, _), !.
personal_directory_init:-
  % Make sure the home directory is there.
  home_init,

  % @tbd Multiple projects can be loaded at the same time.
  user:project(Project, _, _),

  hidden_file_name(Project, Hidden),
  create_directory(home, [Hidden], _),
  assert(user:file_search_path(personal, home(Hidden))).



trashcan_init:-
  file_search_path(trash, _), !.
trashcan_init:-
  create_personal_subdirectory('Trash', Dir),
  assert(user:file_search_path(trash, Dir)).
