% The load file for the Project Library Collection,

:- dynamic(user:project/3).
:- multifile(user:project/3).
user:project('Prolog-Library-Collection', 'Prolog Library Collection', plc).


:- use_module(load_project).
:- load_project([
   ]).

