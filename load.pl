% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.
:- set_prolog_flag(encoding, utf8).

:- initialization(load_plc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_plc:-
  set_project,

  assert(user:prolog_file_type(html, 'text/html')),
  assert(user:prolog_file_type(md,   'text/markdown')),
  assert(user:prolog_file_type(txt,  'text/plain')),
  
  % Check SWI-Prolog version.
  use_remote_module(pl(pl_version)),
  check_pl_version,

  % Set data subdirectory.
  use_remote_module(pl(pl_clas)),
  process_options,

  % Start logging.
  use_remote_module(generics(logging)),
  start_log.


% If there is no outer project, then PGC is the project.

set_project:-
  current_predicate(project/2), !.
set_project:-
  assert(user:project('PGC', 'Prolog Generics Collection')).

