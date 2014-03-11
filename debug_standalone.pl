% Startup script for loading only the PGC in debug mode.

% The extra thing about loading standalone PGC in debug mode,
% is that (1) we have to set the `project` file search path to PGC,
% and that (2) we have to assert that we run the project in debug mode.

:- use_module(library(filesex)).

:- initialization(run_debug_standalone).

:- assert(user:debug_project).

run_debug_standalone:-
  source_file(run_debug_standalone, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  set_data_path,
  ensure_loaded(debug).

set_data_path:-
  current_prolog_flag(argv, [Directory]),
  exists_directory(Directory), !,
  set_data_path(Directory).
set_data_path:-
  absolute_file_name(
    project('Data'),
    Directory,
    [access(write),file_type(directory)]
  ),
  set_data_path(Directory).

set_data_path(Directory):-
  make_directory_path(Directory),
  assert(user:file_search_path(data, Directory)).

