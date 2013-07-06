:- module(
  codes_ext,
  [
    codes_replace1/4, % +Old:list(code)
                      % +From:list(code)
                      % +To:list(code)
                      % -New:list(code)
    codes_replace2/4, % +Old:list(code)
                      % +From:list(code)
                      % +To:list(code)
                      % -New:list(code)
    split_codes/3, % +Codes:list(code)
                   % +Split:list(code)
                   % -Results:list(list(code))
    strip_codes/3, % +Strip:list(code)
                   % +In:list(code)
                   % -Out:list(code)
    to_codes/2 % +AtomOrCodes
               % -Codes:list(code)
  ]
).

/** <module> CODES_EXT

Predicates for handling codes.

@author Wouter Beek
@version 2013/05-2013/07
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).



codes_replace1(Old, From, To, New):-
  phrase(dcg_replace(From, To), Old, New).
codes_replace2([], _From, _To, []):- !.
codes_replace2(Old, From, To, New):-
  append(From, OldRest, Old), !,
  codes_replace2(OldRest, From, To, NewRest),
  append(To, NewRest, New).
codes_replace2([H|T], From, To, [H|NewT]):-
  codes_replace2(T, From, To, NewT).

split_codes(Codes, Split, Results):-
  \+ is_list(Split),
  !,
  split_codes(Codes, [Split], Results).
split_codes(Codes, Split, [Result | Results]):-
  append(Result, Temp, Codes),
  append(Split, NewCodes, Temp),
  !,
  split_codes(NewCodes, Split, Results).
split_codes(Result, _Split, [Result]).

strip_codes(_Strip, [], []):-
  !.
strip_codes(Strip, [H | In], Out):-
  memberchk(H, Strip),
  !,
  strip_codes(Strip, In, Out).
strip_codes(Strip, [H | In], [H | Out]):-
  strip_codes(Strip, In, Out).

to_codes(Atom, Codes):-
  atom(Atom), !,
  atom_codes(Atom, Codes).
to_codes(Codes, Codes).

