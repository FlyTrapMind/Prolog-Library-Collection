:- module(
  char_ext,
  [
    first_char/2, % +Input:or([atom,list(char),list(code),number,string])
                  % ?Char:char
    is_char/1, % @Term
    last_char/2, % +Input:or([atom,list(char),list(code),number,string])
                 % ?Char:char
    to_chars/2 % +Input:or([atom,list(char),list(code),number,string])
               % -Chars:list(char)
  ]
).

/** <module> Character extensions

Extensions to character support in Prolog.

@author Wouter Beek
@version 2014/08, 2014/10
*/

:- use_module(library(apply)).
:- use_module(library(lists)).



%! first_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   +Char:char
%! ) is semidet.
%! first_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Char:char
%! ) is semidet.

first_char(Input, Char):-
  to_chars(Input, [Char|_]).



% is_char(@Term) is semidet.

is_char(Term):-
  atom(Term),
  atom_length(Term, 1).



%! last_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   +Char:char
%! ) is semidet.
%! last_char(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Char:char
%! ) is semidet.
% Silently fails if the input maps to the empty list of characters.

last_char(Input, Char):-
  to_chars(Input, Chars),
  last(Chars, Char).



%! to_chars(
%!   +Input:or([atom,list(char),list(code),number,string]),
%!   -Chars:list(char)
%! ) is det.
% Notice that the empty list of characters and the empty list of codes
% both map onto the empty list of characters.

% Atom.
to_chars(Atom, Chars):-
  atom(Atom), !,
  atom_chars(Atom, Chars).
% Empty list.
to_chars([], []):- !.
% Non-empty list of characters.
to_chars(Chars, Chars):-
  maplist(is_char, Chars), !.
% Non-empty list of codes.
to_chars(Codes, Chars):-
  maplist(char_code, Chars, Codes).
% Number.
to_chars(Number, Chars):-
  number(Number), !,
  number_chars(Number, Chars).
% String.
to_chars(String, Chars):-
  string(String), !,
  string_chars(String, Chars).
