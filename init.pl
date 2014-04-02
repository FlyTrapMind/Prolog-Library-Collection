% PGC initialization file.
% This is called from [load].

:- use_module(generics(db_ext)).
:- use_module(generics(logging)).
:- use_module(library(apply)).
:- ensure_loaded(pgc(pl_debug_option)).
:- use_module(pl(pl_clas)).
:- use_module(pl(pl_package)).
:- use_module(pl(pl_version)).

:- initialization(init_pgc).



init_pgc:-
  % Check SWI-Prolog version.
  check_pl_version,

  % Set data subdirectory.
  process_options(_),
  
  % Initialize Web module registration.
  db_add_novel(user:prolog_file_type(db, database)),
  absolute_file_name(
    project(web_modules),
    File,
    [access(write),file_type(database)]
  ),
  % @tbd Why?
  delete_file(File),

  % Install packages.
  % This requires user interaction on the first load.
  maplist(load_pl_package, [regex,smtp]),
  
  % Start logging.
  start_log.

