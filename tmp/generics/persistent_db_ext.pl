:- module(
  persistent_db_ext,
  [
    persistent_db_init/2 % +File:atom
                         % :Goal
  ]
).

/** <module> Persistent DB Extensions

Additional support for persistent databases in Prolog.

@author Wouter Beek
@version 2014/08, 2014/10
*/

:- use_module(library(persistency)).

:- use_module(plc(generics/db_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(io/file_gnu)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- meta_predicate(persistent_db_init(+,1)).





%! persistent_db_attach(+Module:atom, +File:atom) is det.
% Safe attachement of a persistent database dump.
% This first make sure the given file exists.

persistent_db_attach(Module, File):-
  exists_file(File), !,
  Module:db_attach(File, []).
persistent_db_attach(Module, File):-
  touch_file(File),
  persistent_db_attach(Module, File).


%! persistent_db_init(+File:atom, :Goal) is det.
% Generic initialization of persistent databases.
%
% `Goal` is the update goal that is set by a specific persistent database.
% It takes the persistency file's age as an argument.

persistent_db_init(File, Module:Goal):-
  persistent_db_attach(Module, File),
  file_age(File, Age),
  call(Module:Goal, Age).


