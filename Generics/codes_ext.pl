:- module(
  codes_ext,
  [
    codes_to_atom/2, % +Codes:list(code)
                     % -Atom:atom
    put_codes/1, % +Codes:list(code)
    put_codes/2, % +Stream:stream
                 % +Codes:list(code)
    strip_codes/3, % +Strip:list(list(code))
                   % +In:list(code)
                   % -Out:list(code)
    strip_codes_begin/3, % +Strip:list(list(code))
                         % +In:list(code)
                         % -Out:list(code)
    strip_codes_end/3, % +Strip:list(list(code))
                       % +In:list(code)
                       % -Out:list(code)
    to_codes/2 % +In:or([atom,list(code),number])
               % -Codes:list(code)
  ]
).

/** <module> Codes extensions

Predicates for handling codes.

# Replace

Replacements in list of codes can be made using:
~~~{.pl}
phrase(dcg_replace([From-To|Pairs]), In, Out)
~~~

# Split

Lists of codes can be splitted using:
~~~{.pl}
phrase(dcg_separated_list(:SeparatorDCG,-Sublists:list(list(code))), Codes)
~~~

# Strip

Stripping codes lists is simply done using append,
 see strip_codes/3, strip_codes_begin/3, and strip_codes_end/3.

--

@author Wouter Beek
@version 2013/05-2013/07, 2013/12-2014/01
*/

:- use_module(dcg(dcg_replace)).
:- use_module(library(apply)).
:- use_module(library(lists)).



%! codes_to_atom(+Codes:list(code), -Atom:atom) is det.
% This is solely used in contexts where the argument order is fixed,
%  and the codes parameter just happends to occur before the atom parameter.

codes_to_atom(Codes, Atom):-
  atom_codes(Atom, Codes).


%! put_codes(+Codes:list(code)) is det.
%! put_codes(+Out:stream, +Codes:list(code)) is det.
% @see Wrapper around put_code/1 that works on lists of codes
%      and that can write to an arbitrary stream.

put_codes(Codes):-
  maplist(put_code, Codes).
put_codes(Out, Codes):-
  with_output_to(Out, maplist(put_code, Codes)).


%! strip_codes(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
%! strip_codes_begin(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
%! strip_codes_end(+Strips:list(list(code)), +In:list(code), -Out:list(code)) is det.
% Strips the given atom's front and/or back for the given character.
%
% Notice that the order in which the strip atoms occur is significant.
%
% @tbd Do this with DCG rules instead of lists in `Strips`.

strip_codes(Strips, C1, C3):-
  strip_codes_begin(Strips, C1, C2),
  strip_codes_end(Strips, C2, C3).
strip_codes_begin(Strips, C1, C3):-
  member(Strip, Strips),
  append(Strip, C2, C1),
  strip_codes_begin(Strips, C2, C3).
strip_codes_begin(_, C, C).
strip_codes_end(Strips, C1, C3):-
  member(Strip, Strips),
  append(C2, Strip, C1),
  strip_codes_end(Strips, C2, C3).
strip_codes_end(_, C, C).


%! to_codes(+In:or([atom,list(code),number]), -Out:list(code)) is det.
% Make sure atomic terms are converted to codes lists.

to_codes(Atom, Codes):-
  atom(Atom), !,
  atom_codes(Atom, Codes).
to_codes(Number, Codes):-
  number(Number), !,
  number_codes(Number, Codes).
to_codes(Codes, Codes):-
  is_list(Codes).

