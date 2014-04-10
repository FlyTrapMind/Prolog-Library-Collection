% Run file for PLC.
% This is used to run the PLC independent of any other projects/submodules.

:- use_module(library(filesex)).

:- meta_predicate(user:ensure_remote_loaded(:)).
:- meta_predicate(user:reexport_remote_module(:)).
:- meta_predicate(user:reexport_remote_module(+,:)).
:- meta_predicate(user:reexport_remote_module(+,:,+)).
:- meta_predicate(user:use_remote_module(:)).
:- meta_predicate(user:use_remote_module(+,:)).
:- meta_predicate(user:use_remote_module(+,:,+)).

:- initialization(run_plc).

run_plc:-
  % Assert project file search path.
  absolute_file_name('.', ProjectDir, [access(read),file_type(directory)]),
  assert(user:file_search_path(project, ProjectDir)),
  
  % Load PLC.
  ensure_loaded(load).


user:ensure_remote_loaded(CallingModule:CalledModuleSpec):-
  CallingModule:ensure_loaded(CalledModuleSpec).

user:reexport_remote_module(CallingModule:CalledModuleSpec):-
  CallingModule:reexport(CalledModuleSpec).

user:reexport_remote_module(_, ModuleSpec):-
  user:reexport_remote_module(ModuleSpec).

user:reexport_remote_module(_, CallingModule:CalledModuleSpec, Import):-
  CallingModule:reexport(CalledModuleSpec, Import).

user:use_remote_module(CallingModule:CalledModuleSpec):-
  CallingModule:use_module(CalledModuleSpec).

user:use_remote_module(_, ModuleSpec):-
  user:use_remote_module(ModuleSpec).

user:use_remote_module(_, CallingModule:CalledModuleSpec, ImportList):-
  CallingModule:use_module(CalledModuleSpec, ImportList).

