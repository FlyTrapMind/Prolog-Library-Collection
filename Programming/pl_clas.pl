:- module(
  pl_clas,
  [
    set_data_path/0
  ]
).

/** <module> Prolog command line arguments

Support for command line arguments given at Prolog startup.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(filesex)).
:- use_module(library(lists)).



% Already set.
set_data_path:-
  user:file_search_path(data, _), !.
% Set using command line argument.
set_data_path:-
  current_prolog_flag(argv, Args),
  last(Args, Dir),
  exists_directory(Dir), !,
  set_data_path(Dir).
% Default directory.
set_data_path:-
  absolute_file_name(
    project('.'),
    Dir1,
    [access(write),file_type(directory)]
  ),
  directory_file_path(Dir1, 'Data', Dir2),
  set_data_path(Dir2).

set_data_path(Dir):-
  make_directory_path(Dir),
  assert(user:file_search_path(data, Dir)).

