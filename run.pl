% Run file for PLC.
% This is used to run the PLC independent of any other projects/submodules.

:- initialization(run_plc).

run_plc:-
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),
  assert(user:file_search_path(plc, ProjectDir)),
  ensure_loaded(plc(index)),
  ensure_loaded(plc(load)).

