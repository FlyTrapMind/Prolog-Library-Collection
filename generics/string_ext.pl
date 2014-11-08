:- module(
  string_ext,
  [
    codes_atom/2, % ?Codes:list(nonneg)
                  % ?Atom:atom
    string_list_concat/3 % ?Strings:list(string)
                         % ?Separator:string
                         % ?String:string
  ]
).

/** <module> String: Extensions

Additional support for strings in SWI-Prolog.

@author Wouter Beek
@version 2014/08, 2014/11
*/

:- use_module(library(apply)).



%! codes_atom(+Codes:list(nonneg), +Atom:atom) is semidet.
%! codes_atom(+Codes:list(nonneg), -Atom:atom) is det.
%! codes_atom(-Codes:list(nonneg), +Atom:atom) is det.

codes_atom(Codes, Atom):-
  atom_codes(Atom, Codes).



%! string_list_concat(
%!   +Strings:list(string),
%!   +Separator:string,
%!   +String:string
%! ) is semidet.
%! string_list_concat(
%!   +Strings:list(string),
%!   +Separator:string,
%!   -String:string
%! ) is det.
%! string_list_concat(
%!   -Strings:list(string),
%!   +Separator:string,
%!   +String:string
%! ) is det.

string_list_concat(Strings, Separator, String):-
  maplist(nonvar, [Strings,Separator]), !,
  maplist(atom_string, [Separator0|Atoms], [Separator|Strings]),
  atomic_list_concat(Atoms, Separator0, Atom),
  atom_string(Atom, String).
string_list_concat(Strings, Separator, String):-
  maplist(nonvar, [Separator,String]), !,
  maplist(atom_string, [Separator,Atom], [Separator0,String]),
  atomic_list_concat(Atoms, Separator0, Atom),
  maplist(atom_string, Atoms, Strings).
