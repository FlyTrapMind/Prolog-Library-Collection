% Startup script for loading only the PGC in debug mode.

% The extra thing about loading standalone PGC in debug mode,
% is that (1) we have to set the `project` file search path to PGC,
% and that (2) we have to assert that we run the project in debug mode.

:- initialization(run_debug_standalone).

:- assert(user:debug_project).

run_debug_standalone:-
  source_file(run_debug_standalone, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  assert(user:file_search_path(data, project('Data'))),
  ensure_loaded(debug).

