% PLC load file.
% File search path `plc` must be set for this.

user:prolog_file_type(html, 'text/html').
user:prolog_file_type(log, logging).
user:prolog_file_type(md, 'text/markdown').
user:prolog_file_type(tmp, temporary).
user:prolog_file_type(txt, 'text/plain').

:- initialization(load_plc).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_plc:-
  % Requirement!
  user:file_search_path(plc, _), !,

  % Load the PLC index as file search path statements.
  ensure_loaded(plc(index)),

  % Enumerate the external program support
  % for the currently loaded modules.
  use_module(os(run_ext)),
  list_external_programs.
load_plc:-
  print_message(warning, required_file_search_path(plc)).



% Messages

:- multifile(prolog:message//1).

prolog:message(required_file_search_path(Alias)) -->
  ['File search path ',Alias,' must be set.'].

