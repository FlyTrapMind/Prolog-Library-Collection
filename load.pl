% PLC load file.

user:prolog_file_type(html, 'text/html'    ).
user:prolog_file_type(md,   'text/markdown').
user:prolog_file_type(txt,  'text/plain'   ).

:- initialization(load_plc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_plc:-
  set_project,

  % Check SWI-Prolog version.
  use_remote_module(pl(pl_version)),
  check_pl_version,

  % Set data subdirectory.
  use_remote_module(pl(pl_clas)),
  process_options,

  % Start logging.
  use_remote_module(generics(logging)),
  start_log,

  % Enumerate the external program support
  % for the currently loaded modules.
  use_remote_module(os(run_ext)),
  list_external_programs.


% If there is no outer project, then PGC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PGC', 'Prolog Generics Collection')).

