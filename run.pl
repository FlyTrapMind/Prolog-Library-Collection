% Run file for PGC.
% This is used to run the PGC independent of any other projects/submodules.

:- initialization(run_pgc).

run_pgc:-
  % Entry point.
  source_file(run_pgc, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(project, ThisDir)),
  assert(user:file_search_path(pgc, ThisDir)),
  ensure_loaded(pgc(debug)).

