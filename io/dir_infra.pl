:- module(
  dir_infra,
  [
    create_project_subdirectory/1, % +Subdir:atom
    create_project_subdirectory/2 % +Subdir:atom
                                  % -Path:atom
  ]
).

/** <module> Directory infrastructure

@author Wouter Beek
@version 2015/03
*/

:- use_module(library(filesex)).

:- use_module(plc(io/dir_ext)).





%! create_project_subdirectory(+Subdir:atom) is det.

create_project_subdirectory(Subdir):-
  create_project_subdirectory(Subdir, _).



%! create_project_subdirectory(+Subdir:atom, -Path:atom) is det.

create_project_subdirectory(Subdir, Path):-
  project(Name, _),
  Spec =.. [Name,Subdir],
  (   absolute_file_name(
        Spec,
        Path,
        [access(read),file_type(directory),file_errors(fail)]
      )
  ->  true
  ;   absolute_file_name(Name, Dir, [access(write),file_type(directory)]),
      directory_file_path(Dir, rdf, Path),
      make_directory_path(Path)
  ).
