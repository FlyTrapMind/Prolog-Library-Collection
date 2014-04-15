% PLC load file.
% File search path `plc` must be set for this.

user:prolog_file_type(html, 'text/html'    ).
user:prolog_file_type(md,   'text/markdown').
user:prolog_file_type(txt,  'text/plain'   ).

:- initialization(load_plc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_plc:-
  % Requirement!
  user:file_search_path(plc, _),
  
  % Load the PLC index as file search path statements.
  set_project,
  ensure_loaded(index),
  
  % Check SWI-Prolog version.
  use_module(pl(pl_version)),
  check_pl_version,

  % Set data subdirectory.
  use_module(pl(pl_clas)),
  process_options,

  % Start logging.
  use_module(generics(logging)),
  start_log,

  % Enumerate the external program support
  % for the currently loaded modules.
  use_module(os(run_ext)),
  list_external_programs.


% If there is no outer project, then PLC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PLC', 'Prolog Library Collection')).

