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
  ensure_loaded(debug),
  use_module(ckan(ckan_scrape)).

set_data_path:-
  current_prolog_flag(argv, [Dir]),
  exists_directory(Dir), !,
  set_data_path(Dir).
set_data_path:-
  absolute_file_name(project('.'), Dir1, [access(write),file_type(directory)]),
  directory_file_path(Dir1, 'Data', Dir2),
  set_data_path(Dir2).

set_data_path(Dir):-
  make_directory_path(Dir),
  assert(user:file_search_path(data, Dir)).

