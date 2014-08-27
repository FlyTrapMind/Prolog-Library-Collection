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
@version 2014/08
*/

:- use_module(library(persistency)).

:- use_module(generics(db_ext)).
:- use_module(os(file_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- meta_predicate(persistent_db_init(+,1)).



%! persistent_db_attach(+File:atom) is det.
% Safe attachement of a persistent database dump.
% This first make sure the given file exists.

persistent_db_attach(File):-
  exists_file(File), !,
  db_attach(File, []).
persistent_db_attach(File):-
  touch_file(File),
  persistent_db_attach(File).


%! persistent_db_init(+File:atom, :Goal) is det.
% Generic initialization of persistent databases.
%
% `Goal` is the update goal that is set by a specific persistent database.
% It takes the persistency file's age as an argument.

persistent_db_init(File, Goal):-
  persistent_db_attach(File),
  file_age(File, Age),
  call(Goal, Age).

