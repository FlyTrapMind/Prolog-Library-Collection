% Run file for PLC.
% This is used to run the PLC independent of any other projects/submodules.

:- use_module(library(filesex)).

:- initialization(run_plc).

run_plc:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),
  
  % Replace remote loads with local loads.
  % Also loads the index.
  ensure_loaded(prolog_local_init),
  
  % Load PLC.
  ensure_loaded(load).

