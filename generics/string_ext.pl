:- module(
  string_ext,
  [
    string_list_concat/3 % ?Strings:list(string)
                         % ?Separator:string
                         % ?String:string
  ]
).

/** <module> String extensions

Additional support for strings in SWI-Prolog.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(apply)).



string_list_concat(Strings, Separator, String):-
  maplist(nonvar, [Strings,Separator]), !,
  maplist(atom_string, [Separator0|Atoms], [Separator|Strings),
  atomic_list_concat(Atoms, Separator0, Atom),
  atom_string(Atom, String).
string_list_concat(Strings, Separator, String):-
  maplist(nonvar, [Separator,String]), !,
  maplist(atom_string, [Separator,Atom], [Separator0,String]),
  atomic_list_concat(Atoms, Separator0, Atom),
  maplist(atom_string, Atoms, Strings).
