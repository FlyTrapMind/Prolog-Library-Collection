% Run file for PLC.
% This is used to run the PLC independent of any other projects/submodules.

:- initialization(run_plc).

run_plc:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),

  ensure_loaded(prolog_repository),
  prolog_repository(local, ProjectDir),

  % Load PLC.
  ensure_loaded(load).

