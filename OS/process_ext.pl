:- module(
  process_ext,
  [
    process_absolute_file_name/4, % +Process:atom
                                  % +Spec
                                  % -Absolute
                                  % +Options:list(nvpair)
    process_create_nested_directory/3, % +Process:atom
                                       % +NestedDirectories:compound
                                       % -AbsoluteDirectory:atom
    process_file_search_path/3 % +Project:atom
                               % +Alias:atom
                               % ?Path
  ]
).

/** <module> Process extensions

Support for process-specific directories.

@author Wouter Beek
@version 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(os(dir_ext)).



process_absolute_file_name(Process, Spec1, Absolute, Options):-
  process_alias(Process, Spec1, Spec2),
  absolute_file_name(Spec2, Absolute, Options).

process_alias(Process, Spec1, Spec2):-
  Spec1 =.. [H1|T],
  atomic_list_concat([Process,H1], '_', H2),
  Spec2 =.. [H2|T].

%! process_create_nested_directory(
%!   +Process:atom,
%!   +NestedDirectories:compound,
%!   -AbsoluteDirectory:atom
%! ) is det.

process_create_nested_directory(Process, Spec1, Dir):-
  process_alias(Process, Spec1, Spec2),
  create_nested_directory(Spec2, Dir).

%! process_file_search_path(+Process:atom, +Alias:compound, ?Path) is semidet.
% Read/write file search paths relative to a given process name.
% This allows similar directory structures to be used for different processes.

process_file_search_path(Process, Spec1, Path):-
  process_alias(Process, Spec1, Spec2),
  (
    % Retrieve a file search path relative to a process.
    var(Path),
    file_search_path(Spec2, Path), !
  ;
    % Assert a file search path relative to a process.
    db_add_novel(user:file_search_path(Spec2, Path))
  ).


