:- module(
  dir_infra,
  [
    create_project_subdirectory/1, % +Subdir:atom
    create_project_subdirectory/2, % +Subdir:atom
                                   % -Path:atom
    project_abbreviation/1, % ?Name:atom
    project_description/1, % ?Name:atom
    project_name/1 % ?Name:atom
  ]
).

/** <module> Directory infrastructure

@author Wouter Beek
@version 2015/03
*/

:- use_module(library(filesex)).

:- dynamic(user:project/2).
:- multifile(user:project/2).





%! create_project_subdirectory(+Subdir:atom) is det.

create_project_subdirectory(Subdir):-
  create_project_subdirectory(Subdir, _).



%! create_project_subdirectory(+Subdir:atom, -Path:atom) is det.

create_project_subdirectory(Child, Path):-
  project_abbreviation(Parent),
  Spec =.. [Parent,Child],
  (   absolute_file_name(
        Spec,
        Path,
        [access(read),file_type(directory),file_errors(fail)]
      )
  ->  true
  ;   absolute_file_name(Parent, Dir, [access(write),file_type(directory)]),
      directory_file_path(Dir, Child, Path),
      make_directory_path(Path)
  ).



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

project_name(Name):-
  (   user:project(Name, _, _)
  ->  true
  ;   user:project(Name, _)
  ).
